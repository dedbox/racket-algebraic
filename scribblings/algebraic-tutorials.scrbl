#lang scribble/manual

@title[#:tag "tut" #:style '(toc)]{Tutorial Series: From Models to Interpreters}

@require{./algebraic-includes.rkt}
@(require texmath
          (for-label (except-in algebraic/racket/base fun?)
                     rackunit))

A core use case for @algebraic-mod is fast and cheap interpreter development.
With the right tooling, throw-away interpreters can be an easy way to explore
and validate language design choices quickly.

As it happens, Haskell is already pretty good at this. Algebraic data types
and functional destructuring syntax make interpreters easy to read and write,
and its type system keeps track of pervasive breaking changes for you.
@algebraic-mod addresses the untyped half of this equation---algebraic data
@emph{structures} (ADTs sans typing constraints) with destructuring syntax.

In this series, we will implement three interpreters based on the models
originally used to design @algebraic-mod itself:

@itemlist[
  #:style 'ordered

  @item{A @emph{core} model based on the untyped λ-calculus,}

  @item{An @emph{extended} syntax that compiles down to core constructs, and}

  @item{A @emph{hosted} variant that borrows additional constructs from the
  implementing platform.}

]

The tutorials are written in an informal style with code and discussion mixed
together. If you prefer source code, it can be read all in one place
@hyperlink["https://github.com/dedbox/racket-algebraic/tree/master/model"]{on
github}.

@local-table-of-contents[]

@; =============================================================================

@section[#:tag "tut:core"]{The Core Calculus}

@defmodulelang[algebraic/model/core]

The core model has three major components:

@itemlist[

  @item{A context-free grammar for the formal syntax,}

  @item{An operational semantics that defines evaluation and pattern matching
  operations, and}

  @item{A collection of sample programs and their expected normal forms.}

]

@; -----------------------------------------------------------------------------

@subsection[#:tag "tut:core:syntax"]{Syntax}

The core defines three syntactic categories: terms, values, and patterns.

A @emph{term} (@${t}) is a concrete expression in the calculus.

A @emph{value} (@${v}) is a term in normal form with respect to the evaluation
semantics.

When a term eventually reduces to a particular value, we say the term
@emph{evaluates} to that value. A term may never reduce to a value if the
computation diverges or gets stuck. Some divergent terms are useful as
infinite loops. Any stuck term is an error.

A @emph{pattern} (@${p}) is a syntactic form that produces a set of variable
bindings (σ) when matched with a compatible term.

@centered[
  @grammar["t"
    (list @list{@${t} @${t}} "application")
    (list @list{@${t};@${t}} "sequence")
    (list @list{φ@${p}.@${t}} "function clause")
    (list @list{μ@${p}.@${t}} "macro clause")
    (list @${x} "variable")
    (list @${δ} "constructor")
    (list "◊" "unit")
  ] "\n" "\n"

  @grammar["p"
    (list @list{@${t} @${t}} "application")
    (list @list{@${t};@${t}} "sequence")
    (list "_" "wildcard")
    (list @${x} "variable")
    (list @${δ} "constructor")
    (list "◊" "unit")
  ] "\n" "\n"

  @grammar["v"
    (list @list{φ@${p}.@${t};·} "function")
    (list @list{μ@${p}.@${t};·} "macro")
    (list @${δ} "constructor")
    (list @list{@${δ} (@${v};·)} "instance")
    (list "◊" "unit")
  ]
]

An @emph{application} (@${t} @${t}) is a pair of juxtaposed sub-terms. Nested
applications are always parenthesized.

@core-code{
  (A ◊)
}

A @emph{sequence} (@${t};@${t}) is a pair of sub-terms separated by a
semicolon in the model and prefixed by a dollar sign (@id[$]) in code.
Sequences combine multiple terms into a single unit. They are used for holding
the non-tag elements of a tagged @tech{list}.

@core-code{
  (A ($ ◊ ◊))
}

A @emph{function clause} (φ@${p}.@${t}) or @emph{macro clause} (μ@${p}.@${t})
is a λ-abstraction with the formal parameter generalized to a pattern.

@core-code{
  (φ x x)
  (μ a a)
}

A @emph{uniform sequence} (@${t};·) is a right-hand nesting of sequences in
which every left-hand side holds the same kind of term and every right-hand
side, exept perhaps the last, is the same kind of uniform sequence. Uniform
sequences combine multiple abstractions into a single unit.

A @emph{function} (φ@${p}.@${t};·) is a function clause or a uniform sequence
of them, and a @emph{macro}@elem['nbsp](μ@${p}.@${t};·) is one or more macro
clauses in sequence.

@core-code{
  ($ (φ x x) ($ (φ y y) (φ z z)))
  ($ (μ a a) ($ (μ b b) (μ c c)))
}

A @emph{variable} (@${x}) is a name that may be bound to a term in the body of
a function clause or macro clause. We say a variable not bound by an
abstraction is @emph{free}. All free variables are stuck.

A @emph{constructor} (@${δ}) is a name that identifies a data type.
Constructors evaluate to themselves.

For convenience, constructor names begin with an uppercase letter and variable
names begin with a lowercase letter.

An @emph{instance} (@${δ} (@${v};·)) is a constructor applied to a value or
sequence of values.

@core-code{
  (A ◊)
  (B ($ ◊ ◊))
}

Finally, the @emph{unit} (◊) value denotes the presence of a value, as a
convenient notation for when the actual value is irrelevant.

@subsubsub*section{Abstract Syntax}

According to the formal syntax, we need seven kinds of terms and six kinds of
patterns.

@algebraic-code{
  (data Term (TApp TSeq TFun TMac TVar TCon TUni)
        Patt (PApp PSeq PWil PVar PCon PUni))
}

Abstract syntax drives the interpreter, but it makes a poor surface syntax. We
might have to cross-check the code and the model several times, so it would be
convenient to have a surface syntax that resembles the formal syntax.

@subsubsub*section{The Parser}

The parser's job is to map s-expressions onto the members of @Term. Some terms
have a pattern, and patterns are parsed by different rules than terms.
Although constructor arities are not encoded in the @racket[data] declaration,
we can anticipate the following:

@itemlist[

  @item{Applications, sequences, and the abstractions take two arguments
  each.}

  @item{Variables and constructors take one argument each.}

  @item{The wildcard and the unit value take no arguments.}

  @item{Applications and sequences are purely structurally recursive.}

]

With these points in mind, the parser will look like this:

@algebraic-code{
  (define (parse t)
    (define term
      (function
        [(  t1 t2) (TApp (term t1) (term t2))]
        [($ t1 t2) (TSeq (term t1) (term t2))]
        [('φ p1 t2) ... (TFun ...) ...]
        [('μ p1 t2) ... (TMac ...) ...]
        [x #:if (con-name? x) (TCon x)]
        [x #:if (var-name? x) (TVar x)]
        [◊ TUni]))
    (define patt
      (function
        [(  p1 p2) (PApp (patt p1) (patt p2))]
        [($ p1 p2) (PSeq (patt p1) (patt p2))]
        [x #:if (con-name? x) (PCon x)]
        [x #:if (var-name? x) (PVar x)]
        ['_ PWil]
        [◊ PUni]))
    (term t))
}

The @id[parse] @tech{function} is defined as a pair of s-expression parser
combinators: @id[term] and @id[patt]. The @id[term] combinator translates
terms in the formal syntax onto @Term, while @id[patt] maps patterns in the
formal syntax onto @Patt. Parsing always starts and ends with a single term.

In the patterns of these @tech{functions}, some symbols are quoted while
others are not. The Greek symbols are quoted because @id[φ] and @id[μ] are
actually lowercase letters and we don't want them to behave like variables.
The underscore is quoted because an unquoted @id[_] is a wildcard pattern for
the enclosing @racket[function] form. Quoting is optional for @id[$] and
@id[◊] because neither is an uppercase or lowercase character.

For other symbols, we need a way to tell constructor names from variable
names. Inspecting characters is too low level for s-expression parsing, so
we've put that logic into helper predicates: @id[con-name?] checks if the
first character is uppercase while @id[var-name?] checks if it is lowercase.

@algebraic-code{
  (define (con-name? x)
    (and (symbol? x) (char-lower-case? (first-char x))))

  (define (var-name? x)
    (and (symbol? x) (char-upper-case? (first-char x))))

  (define (first-char x)
    (string-ref (symbol->string s) 0))
}

But what about the abstraction parsers? Pure structural recursion is an
option, but we can save some trouble later if we do a little extra work here
in the parser.

The following is a standard illustration of an issue called @emph{unintended
variable capture}:

@core-code{
  ((φ x (φ y (x y))) y)
}

If we substitute @id[y] for @id[x] naively, the result would be a function
clause wherein the free outer @id[y] is indistinguishable from the bound inner
@id[y]:

@core-code{
  (φ y (y y))
}

Unintended variable capture is a well-studied problem with multiple solutions.
Our goal is to produce correct code that is easy to read, so we'll take a
straight-forward approach of uniquely renaming all non-free variable names as
they are parsed. This is equivalent to rewriting the original s-expression so
every bound variable gets a unique index. For example, the original term could
be rewritten as:

@core-code{
  ((φ x1 (φ y2 (x1 y2))) y)
}

Reducing this term does not lead to the unintended capture of @id[y2].

@core-code{
  (φ y2 (y y2))
}

Later, we'll implement a @tech{function} named @id[α-rename] to
automate this variable-renaming process. For now, we'll just use it to finish
the @id[parse] @tech{function}.

@algebraic-code{
  (define (parse t)
    (define term
      (function
        [(  t1 t2) (TApp (term t1) (term t2))]
        [($ t1 t2) (TSeq (term t1) (term t2))]
        [('φ p1 t2) (values-> TFun (α-rename (patt p1) (term t2)))]
        [('μ p1 t2) (values-> TMac (α-rename (patt p1) (term t2)))]
        [x #:if (con-name? x) (TCon x)]
        [x #:if (var-name? x) (TVar x)]
        [◊ TUni]))
    (define patt
      (function
        [(  p1 p2) (PApp (patt p1) (patt p2))]
        [($ p1 p2) (PSeq (patt p1) (patt p2))]
        [x #:if (con-name? x) (PCon x)]
        [x #:if (var-name? x) (PVar x)]
        ['_ PWil]
        [◊ PUni]))
    (term t))

  (define-syntax values->
    (μ* (f xs-expr)
      (call-with-values (λ () xs-expr) f)))
}

The @id[α-rename] @tech{function} takes a @Patt and a @Term. It returns two
values: a copy of the @Patt with all of its variables renamed, and a copy of
the @Term with its variables renamed as they were in the @Patt.

The @id[values->] @tech{macro} is a simple syntactic patch for
@racket[call-with-values] that rearranges the arguments for a more compact
applicative style.

@subsubsub*section{The Printer}

The printer's job is to translate the members of @Term back into
s-expressions. The printer has the same structure as the parser, but the roles
of pattern and body are swapped.

@algebraic-code{
  (define (show t)
    (define term
      (function
        [(TApp t1 t2) `(  ,(term t1) ,(term t2))]
        [(TSeq t1 t2) `($ ,(term t1) ,(term t2))]
        [(TMac p1 t2) `(μ ,(patt p1) ,(term t2))]
        [(TFun p1 t2) `(φ ,(patt p1) ,(term t2))]
        [(TVar x1) (α-restore x1)]
        [(TCon δ1) δ1]
        [TUni '◊]))
    (define patt
      (function
        [(PApp p1 p2) `(  ,(patt p1) ,(patt p2))]
        [(PSeq p1 p2) `($ ,(patt p1) ,(patt p2))]
        [(PVar x1) (α-restore x1)]
        [(PCon δ1) δ1]
        [PWil '_]
        [PUni '◊]))
    (term t))
}

The @id[show] function is also defined as a pair of @id[term]/@id[patt]
combinators. This time, each @racket[function] pattern deconstructs a member
of @Term or @Patt and produces a term or pattern in the formal syntax as an
s-expression. A helper named @id[α-restore] is introduced pre-emptively to
undo whatever @id[α-rename] does to variable names.

@subsubsub*section{Values}

Operationally, a value is a term that evaluates to itself trivially. Values
are the subset of terms that constitute a valid computational result. To
recognize them, we'll need a series of predicates on the members of @Term.

Our goal is to produce a @id[value?] predicate that checks whether a @Term is:

@itemlist[

  @item{a function,}

  @item{a macro,}

  @item{a constructor,}

  @item{an instance of a constructor, or}

  @item{the unit value.}

]

To do all this, we need to recognize three kinds of uniform sequence:
functions, macros, and instance data.

@algebraic-code{
  (define-syntax define-uniform-sequence-pred
    (μ* (name? kind?)
      (define name?
        (function
          [(TSeq t1 t2) #:if (kind? t1) (name? t2)]
          [t (kind? t)]))))

  (define-uniform-sequence-pred fun? TFun?)
  (define-uniform-sequence-pred mac? TMac?)
  (define-uniform-sequence-pred dat? value?)
}

A @id[define-uniform-sequence-pred] form takes two arguments: a name to bind
and a predicate name. It binds the name to a new predicate for uniform
sequences of terms satisfying the named predicate.

The @id[fun?], @id[mac?], and @id[dat?] predicates recognize a uniform
sequence of function clauses, macro clauses, and values, respectively.

@algebraic-code{
  (define ins? (function [(TApp (TCon _) t) (dat? t)] [_ #f]))
}

The @id[ins?] predicate recognizes an instance as a constructor applied to
instance data. Although instance data is not strictly uniform, thinking of it
as a ``uniform'' sequence of values is convenient because the predicates all
have the same shape.

@algebraic-code{
  (require racket/contract/base)

  (define value? (or/c fun? mac? TCon? ins? TUni?))
}

The @id[value?] predicate combines all three uniform sequence predicates with
the @TCon? and @TUni? predicates generated by our @racket[data] declaration to
cover all five cases.

@; -----------------------------------------------------------------------------

@subsection[#:tag "tut:core:eval-semantics"]{Evaluation Semantics}

Every interpreter in the tutorial series provides a ``top level'' evaluator
@tech{macro} named @id[algebraic] which:

@itemlist[
  #:style 'ordered

  @item{@id[parse]s an unquoted s-expression as a @Term,}

  @item{attempts to evaluate the @Term to a @id[value?], and}

  @item{@id[show]s the result.}

]

@algebraic-code{
  (define-syntax algebraic
    (μ t (show (interpret (parse 't)))))
}

The @id[interpret] @tech{function} will drive the computation of @id[t] in a
tight loop that calls another @tech{function} named @id[step] until it returns
a @id[value?].

@algebraic-code{
  (define interpret
    (function
      [v #:if (value? v) v]
      [t
       ;; (writeln (show t))
       (interpret
        (or (step t)
            (error 'interpret "stuck at ~v" (show t))))]))
}

The @id[step] @tech{function} will take a @id[Term] for the current step of a
computation and return the next step or @racket[#f] if stuck.

If a term gets stuck, we'll want to trace the steps of the computation to
figure out what went wrong. We could just toggle the commented middle line in
a pinch, but the conditional looping behavior of @id[interpret] is easy to
capture with a @tech{macro}.

@margin-note{

  The colon-delimited @stech{syntax class} name (@id[:id]) labels some
  @tech{macro} arguments as identifiers. These extra bits of notation give
  better error messages when something goes wrong.

}

@algebraic-code{
  (define-syntax define-interpreter
    (μ* ((name:id t:id) step-expr ...+)
      (define name
        (function
          [v #:if (value? v) v]
          [t (name
              (or (begin step-expr ...)
                  (error 'name "stuck at ~v" (show t))))]))))
}

A @id[define-interpreter] form looks like the @racket[define] shorthand for a
unary function. Its argument receives the current step of the computation and
its body calculates the next step.

With @id[define-interpreter], we can cleanly define the @id[interpret]
@tech{function} alongside a more verbose @id[trace] variant.

@algebraic-code{
  (define-interpreter (interpret t)
    (step t))

  (define-interpreter (trace t)
    (writeln (show t))
    (step t))
}

We still need a @id[step] @tech{function}. Its job is to implement the
semantics of our language. The core model consists of ten inference rules
governing the evaluation of terms to values. Each rule corresponds to a clause
of the @id[step] @tech{function}, so we'll put them into their own
@tech{functions} and have @id[step] try them in order.

The rule @tech{functions} have a regular shape:

@algebraic-code{
  (define step-name (function [pattern result] [_ #f]))
}

With a @tech{macro} that abstracts away everything but the @id[step-name]s,
@id[pattern]s, and @id[result]s, our @id[step] definition will look like this:

@algebraic-code{
  (define-stepper step
    (rule1 rule2 ... ruleN)
    [rule1 pattern1 result1]
    [rule2 pattern2 result2]
    ...
    [ruleN patternN resultN])
}

A @id[define-stepper] form with @${n} rules defines @${n+1} names: a
@tech{function} for each rule and one more that tries them in the @id[order]
specified.

@margin-note{

  The @id[fun-patt] @stech{syntax class} is provided by @algebraic-mod for
  denoting @tech{function} patterns in @tech{macro} arguments.

}

@algebraic-code{
  (define-syntax define-stepper
    (μ* (stepper-name:id
         (order:id ...+)
         [step-name:id pattern:fun-patt result] ...+)
      (begin
        (define (stepper-name t) (or (order t) ...))
        (define step-name (function [pattern result] [_ #f]))
        ...)))
}

The @id[define-stepper] @tech{macro} expects a list of rule names in addition
to definitions so we can change the order of application without having to
move the definitions around.

@subsubsub*section{Applications}

@tabular[
  #:style full-width
  #:cell-properties '((center))
  (list
   (list
    @inferrule[
          @${t_1 ↝ t_1'}
      ----------------------- @App1
      @${t_1 t_2 ↝ t_1' t_2}
    ]

    @inferrule[
      @list{@${v_1}≁μ@${p}.@${t};·}
            @${t_2 ↝ t_2'}
      ------------------------------ @App2
         @${v_1 t_2 ↝ v_1 t_2'}
    ]
   ))
]

Applications evaluate quasi-eagerly, starting on the left. If the left side is
a macro, the macro will operate on the un-evaluated right side. Otherwise,
evaluation proceeds to the right.

@algebraic-code{
  (define-stepper step
    ...
    [app1 (TApp t1 t2) (let ([t1* (step t1)]) (and t1* (TApp t1* t2 )))]
    [app2 (TApp v1 t2) (let ([t2* (step t2)]) (and t2* (TApp v1  t2*)))]
    ...)
}

This defines two @tech{functions}: @id[app1] and @id[app2].

The @id[app1] @tech{function} takes a @TApp with two sub-terms and tries to
@id[step] the first. If successful, it returns a copy of the @TApp with the
new first sub-term. Otherwise, it returns @racket[#f].

The @id[app2] @tech{function} does essentially the same thing with the second
sub-term of a @TApp. If we always try @id[app1] first, we can assume the first
sub-term of a @TApp is fully evaluated in @id[app2].

Most of the rules have this @racket[(let ([t* ...]) (and t* ...))] form, so
we'll put it in a @tech{macro}.

@algebraic-code{
  (define-syntax sub-step
    (μ* (t*-expr t*:id result)
      (let ([t* t*-expr]) (and t* result))))
}

The @id[sub-step] form expresses structural recursion a little more compactly:

@algebraic-code{
  (define-stepper step
    ...
    [app1 (TApp t1 t2) (sub-step (step t1) t1* (TApp t1* t2 ))]
    [app2 (TApp v1 t2) (sub-step (step t2) t2* (TApp v1  t2*))]
    ...)
}

@subsubsub*section{Sequences}

@tabular[
  #:style full-width
  #:cell-properties '((center))
  (list
   (list
    @inferrule[
          @${t_1 ↝ t_1'}
      ----------------------- @Seq1
      @${t_1;t_2 ↝ t_1';t_2}
    ]

    @inferrule[
          @${t_2 ↝ t_2'}
      ----------------------- @Seq2
      @${v_1;t_2 ↝ v_1;t_2'}
    ]
   ))
]

Sequences always evaluate eagerly from left to right.

@algebraic-code{
  (define-stepper step
    ...
    [seq1 (TSeq t1 t2) (sub-step (step t1) t1* (TSeq t1* t2 ))]
    [seq2 (TSeq v1 t2) (sub-step (step t2) t2* (TSeq v1  t2*))]
    ...)
}

@subsubsub*section{Simple Reduction}

@tabular[
  #:style full-width
  #:cell-properties '((center))
  (list
   (list
    @inferrule[
                 @${p_{11}×v_2=σ}
                @${σ(t_{12})=t_{12}'}
      ---------------------------------------------- @AppF
      @list{(φ@${p_{11}.t_{12}})@${ v_2 ↝ t_{12}'}}
    ]

    @inferrule[
                 @${p_{11}×t_2=σ}
                @${σ(t_{12})=t_{12}'}
      ---------------------------------------------- @AppM
      @list{(μ@${p_{11}.t_{12}})@${ t_2 ↝ t_{12}'}}
    ]
   ))
]

Function clauses attempt to match a single argument value to a pattern. If the
match succeeds, any variables bound by the pattern are substituted into the
body of the function. If the match fails, the term is stuck.

Macro clauses work similarly. The only semantic difference between function
clauses and macro clauses is that macro clauses do not evaluate their argument
before attempting the match.

@algebraic-code{
  (define-stepper step
    ...
    [appF (TApp (TFun p11 t12) v2) (sub-step (× p11 v2) σ (subst σ t12))]
    [appM (TApp (TMac p11 t12) t2) (sub-step (× p11 t2) σ (subst σ t12))]
    ...)
}

The @id[×] (``cross'') @tech{function} defines the semantics of pattern
matching. It takes a @Patt and a @Term and returns a @rtech{hash} named @id[σ]
as described in the next section.

The @id[subst] @tech{function} implements variable substitution. Here, it
takes two arguments: a @rtech{hash} @id[σ] and a term @id[t12]. The
@rtech{hash} maps variable names to @Terms, and @id[subst] applies those
mappings to @id[t12] recursively.

@algebraic-code{
  (require racket/set)

  (define (subst σ t [mask (seteq)])
    ((function
       [(TApp t1 t2) (TApp (subst σ t1 mask) (subst σ t2 mask))]
       [(TSeq t1 t2) (TSeq (subst σ t1 mask) (subst σ t2 mask))]
       [(TFun p1 t2) (TFun p1 (subst σ t2 (set-union mask (vars p1))))]
       [(TMac p1 t2) (TMac p1 (subst σ t2 (set-union mask (vars p1))))]
       [(TVar x1) (if (set-member? mask x1) t (hash-ref σ x1))]
       [(TCon _) t]
       [TUni t])
     t))
}

The @id[subst] @tech{function} passes around an optional @rtech{set} named
@id[mask] to remember which variables are already bound in the active
substitution context. This is how we prevent unintended variable capture.
Thanks to @id[α-rename], variable names can be compared directly with
@racket[eq?].

@algebraic-code{
  (define vars
    (function
      [(PApp p1 p2) (set-union (vars p1) (vars p2))]
      [(PSeq p1 p2) (set-union (vars p1) (vars p2))]
      [(PVar x1) (seteq x1)]
      [(PCon _) (seteq)]
      [PWil (seteq)]
      [PUni (seteq)]))
}

The @id[vars] @tech{function} takes a @Patt and returns a @rtech{set}
containing all of the variable names it binds.

It's worth noting @algebraic-mod abstractions do not offer explicit syntax for
default argument values. We could have implemented @id[subst] as a two-clause
@racket[function*]:

@algebraic-code{
  (define subst
    (function*
      [(σ t) (subst σ t (seteq))]
      [(σ t mask)
       ((function ...) t)]))
}

Alternatively, we could have repeated the first and final arguments in every
clause but the first:

@algebraic-code{
  (define subst
    (function*
      [(σ t) (subst σ t (seteq))]
      [(σ (TApp t1 t2) mask) ...]
      [(σ (TSeq t1 t2) mask) ...]
      ...))
}

@subsubsub*section{Full Reduction}

@tabular[
  #:style full-width
  #:cell-properties '((center))
  (list
   (list
    @inferrule[
       @list{(φ@${p_{11}}.@${t_{12}}) @${v_2 ↝ t_{12}'}}
      ---------------------------------------------------- @Fun1
      @list{(φ@${p_{11}}.@${t_{12}};@${v_{13}}) @${v_2 ↝ t_{12}'}}
    ]

    @inferrule[
       @${p_{11}×v_2 undefined}
      ---------------------------------------------------- @Fun2
      @list{(φ@${p_{11}}.@${t_{12}};@${v_{13}}) @${v_2 ↝ v_{13} v_2}}
    ]
   ))
]

@tabular[
  #:style full-width
  #:cell-properties '((center))
  (list
   (list
    @inferrule[
       @list{(μ@${p_{11}}.@${t_{12}}) @${t_2 ↝ t_{12}'}}
      ---------------------------------------------------- @Mac1
      @list{(φ@${p_{11}}.@${t_{12}};@${v_{13}}) @${t_2 ↝ t_{12}'}}
    ]

    @inferrule[
       @${p_{11}×t_2 undefined}
      ---------------------------------------------------- @Mac2
      @list{(φ@${p_{11}}.@${t_{12}};@${v_{13}}) @${t_2 ↝ v_{13} v_2}}
    ]
   ))
]

Multi-clause functions and macros attempt to match their inputs to each clause
in the sequence, taking the body of the first succesful match as their result.
If every match fails, the term is stuck.

If we transcribe @Fun1, @Fun2, @Mac1, and @Mac2 directly from the operational
semantics, our @id[step] function would look like this:

@algebraic-code{
  (define-stepper step
    ...
    [fun1 (TApp (TSeq (TFun x t) v12) v2) (step (TApp (TFun x t) v2))]
    [mac1 (TApp (TSeq (TFun x t) v12) v2) (step (TApp (TFun x t) v2))]
    [fun2 (TApp (TSeq (TFun _ _) v12) v2) (TApp v12 v2)]
    [mac2 (TApp (TSeq (TFun _ _) v12) v2) (TApp v12 v2)]
    ...)
}

but these four rules can easily be merged into two:

@algebraic-code{
  (define-stepper step
    ...
    [redF (TApp (TSeq (TFun x t) v12) v2) (or (step (TApp (TFun x t) v2)) (TApp v12 v2))]
    [redM (TApp (TSeq (TMac x t) v12) t2) (or (step (TApp (TMac x t) t2)) (TApp v12 t2))]
    ...)
}

and the structural similarity of these rules is compelling, given their
length. We can keep our rule set compact by capturing the @racket[(or (step
(TApp ...)) (TApp ...))] form in another @tech{macro}.

@algebraic-code{
  (define-syntax alt-step
    (μ* ((abs val) (alt alt-val))
      (or (step (TApp abs val)) (TApp alt alt-val))))
}

The @id[alt-step] form more compactly expresses this kind of choice.

@algebraic-code{
  (define-stepper step
    ...
    [redF (TApp (TSeq (TFun x t) v12) v2) (alt-step ((TFun x t) v2) (v12 v2))]
    [redM (TApp (TSeq (TMac x t) v12) v2) (alt-step ((TMac x t) v2) (v12 v2))]
    ...)
}

@subsubsub*section{The Full Stepper}

When assembled, the rules define a complete @id[step] @tech{function}.

@algebraic-code{
  (define-stepper step
    (seq1 seq2 app1 appM redM app2 appF redF)
    [app1 (TApp t1 t2) (sub-step (step t1) t1* (TApp t1* t2 ))]
    [app2 (TApp v1 t2) (sub-step (step t2) t2* (TApp v1  t2*))]
    [seq1 (TSeq t1 t2) (sub-step (step t1) t1* (TSeq t1* t2 ))]
    [seq2 (TSeq v1 t2) (sub-step (step t2) t2* (TSeq v1  t2*))]
    [appF (TApp (TFun p11 t12) v2) (sub-step (× p11 v2) σ (subst σ t12))]
    [appM (TApp (TMac p11 t12) t2) (sub-step (× p11 t2) σ (subst σ t12))]
    [redF (TApp (TSeq (TFun x t) v12) v2) (alt-step ((TFun x t) v2) (v12 v2))]
    [redM (TApp (TSeq (TMac x t) v12) v2) (alt-step ((TMac x t) v2) (v12 v2))])
}

@; -----------------------------------------------------------------------------

@subsection[#:tag "tut:core:match-semantics"]{Pattern Matching Semantics}

Pattern matching (@${p×t=σ}) is a binary operation on patterns and terms which
produces variable bindings when a pattern and term are compatible in one of
the following six ways.

@tabular[
  #:style full-width
  #:cell-properties '((center))
  (list
   (list
    @inferrule[
                      @list{@${p_{11}}×@${t_{21}} = @${σ_1}}
                      @list{@${p_{12}}×@${t_{22}} = @${σ_2}}
      ---------------------------------------------------------------------------
      @list{(@${p_{11}} @${p_{12}})×(@${t_{21}} @${t_{22}}) = @${σ_1} ∪ @${σ_2}}
    ]

    @inferrule[
                      @list{@${p_{11}}×@${t_{21}} = @${σ_1}}
                      @list{@${p_{12}}×@${t_{22}} = @${σ_2}}
      ---------------------------------------------------------------------------
      @list{(@${p_{11}};@${p_{12}})×(@${t_{21}};@${t_{22}}) = @${σ_1} ∪ @${σ_2}}
    ]
  ))
]

@tabular[
  #:style full-width
  #:cell-properties '((center))
  (list
   (list
    @list{@${x_1}×@${t_2} = {@${x_1}↦@${t_2}}}

    @inferrule[
       @list{@${δ_1} = @${δ_2}}
      ---------------------------
      @list{@${δ_1}×@${δ_2} = {}}
    ]

    @list{_×@${t_1} = {}}

    @list{◊×◊ = {}}
  ))
]

An @emph{application pattern} (@${p} @${p}) matches an application if the
sub-patterns and sub-terms all match by position, passing along any variable
bindings.

A @emph{sequence pattern} (@${p;p}) behaves similarly.

A @emph{variable} (@${x}) matches any term and binds itself to the term.

A @emph{constructor pattern} (@${δ}) matches a constructor with the same name
and binds no variables.

The @emph{wildcard} (_) matches any term and binds no variables.

The @emph{unit pattern} (◊) matches only the unit value.

Any other combination is undefined.

@subsubsub*section{The Cross Function}

The @id[×] @tech{function} implements these six equations directly.

@algebraic-code{
  (require racket/hash)

  (define ×
    (function*
      [((PApp p11 p12) (TApp t21 t22))
       (let ([σ1 (× p11 t21)]
             [σ2 (× p12 t22)])
         (and σ1 σ2 (hash-union σ1 σ2)))]
      [((PSeq p11 p12) (TSeq t21 t22))
       (let ([σ1 (× p11 t21)]
             [σ2 (× p12 t22)])
         (and σ1 σ2 (hash-union σ1 σ2)))]
      [((PCon δ) (TCon δ)) (make-immutable-hasheq)]
      [((PVar x1) t2) (make-immutable-hasheq `([,x1 . ,t2]))]
      [(PWil _) (make-immutable-hasheq)]
      [(PUni TUni) (make-immutable-hasheq)]
      [(_ _) #f]))
}

In the constructors clause, @algebraic-mod implicitly guarantees the pattern
and term refer to the same constructor. When a @tech{function} variable name
appears more than once in a single @tech{function} pattern, all of the values
it binds must be @racket[equal?].

@; -----------------------------------------------------------------------------

@subsection[#:tag "tut:core:pragmatics"]{Pragmatics}

It's time to pay the piper.

@algebraic-code{
  (define (α-rename p t)
    (define (term x y)
      (function
        [(TApp t1 t2) (TApp ((term x y) t1) ((term x y) t2))]
        [(TSeq t1 t2) (TSeq ((term x y) t1) ((term x y) t2))]
        [(TFun p1 t2) (TFun p1 ((term x y) t2))]
        [(TMac p1 t2) (TMac p1 ((term x y) t2))]
        [(TVar x1) (TVar (if (equal? x1 x) y x1))]
        [(TCon δ1) (TCon δ1)]
        [TUni TUni]))
    (define (patt x y)
      (function
        [(PApp p1 p2) (PApp ((patt x y) p1) ((patt x y) p2))]
        [(PSeq p1 p2) (PSeq ((patt x y) p1) ((patt x y) p2))]
        [(PVar x1) (PVar (if (equal? x1 x) y x1))]
        [(PCon δ1) (PCon δ1)]
        [PWil PWil]
        [PUni PUni]))
    (define p-vars (set->list (vars p)))
    (let loop ([xs p-vars]
               [ys (map genvar p-vars)]
               [p* p]
               [t* t])
      (if (null? xs)
          (values p* t*)
          (let ([p** ((patt (car xs) (car ys)) p*)]
                [t** ((term (car xs) (car ys)) t*)])
            (loop (cdr xs) (cdr ys) p** t**)))))

  (define genvar (φ x (string->uninterned-symbol (symbol->string x))))
}

The @id[α-rename] @tech{function} builds on the @id[term]/@id[patt] combinator
technique. It takes a @Patt and a @Term and returns copies with every variable
bound by the @Patt consistently renamed in both.

Most of the code is just plumbing for structural recursion, but the @TVar and
@PVar cases are more interesting. If the name of a variable is @racket[equal?]
to @id[x], we replace it with @id[y].

The body @id[loop] does three things:

@itemlist[
  #:style 'ordered

  @item{Extracts the variables of a @Patt,}

  @item{Generates a fresh @rtech{uninterned} symbol with the same name as each
  variable extracted, and}

  @item{Recursively substitutes the new name for the original in both arguments.}

]

The @id[genvar] @tech{function} takes an @rtech{interned} symbol and generates
an @rtech{uninterned} symbol with the same name. If we replace @id[genvar]
with @racket[gensym], the bound variables will be tagged with unique numbers
when parsed:

@core-mod-example[
  #:hidden
  (define (parse/gensym t)
    (define term
      (function
        [(  t1 t2) (TApp (term t1) (term t2))]
        [($ t1 t2) (TSeq (term t1) (term t2))]
        [('φ p1 t2) (values-> TFun (α-rename/gensym (patt p1) (term t2)))]
        [('μ p1 t2) (values-> TMac (α-rename/gensym (patt p1) (term t2)))]
        [x #:if (con-name? x) (TCon x)]
        [x #:if (var-name? x) (TVar x)]
        [◊ TUni]))
    (define patt
      (function
        [(  p1 p2) (PApp (patt p1) (patt p2))]
        [($ p1 p2) (PSeq (patt p1) (patt p2))]
        [x #:if (con-name? x) (PCon x)]
        [x #:if (var-name? x) (PVar x)]
        ['_ PWil]
        [◊ PUni]))
    (term t))

  (define (α-rename/gensym p t)
    (define (term x y)
      (function
        [(TApp t1 t2) (TApp ((term x y) t1) ((term x y) t2))]
        [(TSeq t1 t2) (TSeq ((term x y) t1) ((term x y) t2))]
        [(TFun p1 t2) (TFun p1 ((term x y) t2))]
        [(TMac p1 t2) (TMac p1 ((term x y) t2))]
        [(TVar x1) (TVar (if (equal? x1 x) y x1))]
        [(TCon δ1) (TCon δ1)]
        [TUni TUni]))
    (define (patt x y)
      (function
        [(PApp p1 p2) (PApp ((patt x y) p1) ((patt x y) p2))]
        [(PSeq p1 p2) (PSeq ((patt x y) p1) ((patt x y) p2))]
        [(PVar x1) (PVar (if (equal? x1 x) y x1))]
        [(PCon δ1) (PCon δ1)]
        [PWil PWil]
        [PUni PUni]))
    (define p-vars (set->list (vars p)))
    (let loop ([xs p-vars]
               [ys (map gensym p-vars)]
               [p* p]
               [t* t])
      (if (null? xs)
          (values p* t*)
          (let ([p** ((patt (car xs) (car ys)) p*)]
                [t** ((term (car xs) (car ys)) t*)])
            (loop (cdr xs) (cdr ys) p** t**)))))
]

@core-mod-example[
  (show (trace (parse/gensym '((φ x (φ y (x y))) y))))
]

There's a wrinkle. Racket can still tell the difference between two
@rtech{uninterned} symbols with the same name. If the @id[show]
@tech{function} can prevent @rtech{uninterned} symbols from leaking to the
surface, we can compare surface terms with @racket[equal?] in unit tests.

The @id[α-restore] @tech{function} will reconstruct the @rtech{interned}
symbol for printing.

@algebraic-code{
  (define (α-restore x)
    (string->symbol (symbol->string x)))
}

@; -----------------------------------------------------------------------------

@subsection[#:tag "tut:core:hash-lang"]{Putting It All Together}

We now have a complete @id[algebraic] form for evaluating core terms.

@core-mod-example[
  (algebraic ◊)
  (algebraic ((φ x x) (φ y y)))
]

This is good enough for unit testing, but it's still a little clunky in
interactive sessions. We won't need to embed every term in an @id[algebraic]
form if we turn the module into a @shlang.

@margin-note{

  The @algebraic-mod package installs an @id[algebraic/model] collection that
  implements @shlangs for all of the tutorial interpreters. If you want to
  work with uninstalled modules, see the @racket[module] form or
  @hash-lang[s-exp] in the @secref["module-languages" #:doc '(lib
  "scribblings/guide/guide.scrbl")] section of @other-doc['(lib
  "scribblings/guide/guide.scrbl")].

}

To do that, we need to write a little boilerplate. With everything in place,
we should be able to enter a @id[.rkt] file with the first line set to
@hash-lang[algebraic/model/core] (or wherever your module is installed) and
interact with the interpreter directly.

@algebraic-code{
  (define-syntax core-module-begin
    (μ* (form ...)
      (#%plain-module-begin ((current-print) (algebraic form)) ...)))

  (define-syntax core-top-interaction
    (μ* form (#%top-interaction . (algebraic form))))
}

The default @racket[#%module-begin] form wraps non-interactive top-level
expressions, ``to print non-@seclink["void+undefined" #:doc '(lib
"scribblings/guide/guide.scrbl")]{@void-const} results using
@racket[current-print].'' We also wrap them with an @id[algebraic] form.

The default @racket[#%top-interaction] form wraps interactive top-level
expressions as a hook into the default @racket[read-eval-print-loop]. We also
wrap the inputs with an @id[algebraic] form.

To replace the default top-level forms with our own, we'll rename them on
export. We also need to export the default @racket[#%app] and @racket[#%datum]
forms because Racket will complain if we don't.

@algebraic-code{
  (provide (all-defined-out)
           (for-syntax (all-defined-out))
           (rename-out [core-module-begin #%module-begin]
                       [core-top-interaction #%top-interaction])
           #%app #%datum)
}

Exporting the top-level forms this way makes our module work as a
@gtech{module language}.

When a @id[.rkt] file begins with @hash-lang[algebraic/model/core], Racket
uses another module named @id[algebraic/model/core/lang/reader] to load our
customizations:

@typeset-code{#lang s-exp syntax/module-reader algebraic/model/core}

This file configures Racket to use @id[algebraic/model/core] as a
@gtech{module language} with the default s-expression @gtech{reader} whenever
it encounters a @id[.rkt] file that begins with
@hash-lang[algebraic/model/core].

@; -----------------------------------------------------------------------------

@subsection[#:tag "tut:core:examples"]{Examples}

We'll implement each example twice: once in @hash-lang[algebraic/racket/base]
and again in @hash-lang[algebraic/model/core]. Our interpreter is too crude
for real development work, so we'll discuss concepts and constructs in
@algebraic-mod proper before attempting to encode them for our interpreter.

@subsubsection{Peano Numbers}

@hyperlink["https://en.wikipedia.org/wiki/Peano_axioms#First-order_theory_of_arithmetic"]{Peano
numbers} are an easy way to represent the natural numbers (0, 1, 2, ...) with
algebraic data.

First, we designate a 0 element and define a
@hyperlink["https://en.wikipedia.org/wiki/Successor_function"]{successor
function} for it. A simple two-@tech{product} @tech{sum} would suffice.

@example[
  (data Peano (Zero Succ))
]

We can now define @Peano numbers inductively:

@itemlist[

  @item{@Zero is the smallest Peano number.}

  @item{For every Peano number @${n}, (@Succ @${n}) is also a Peano number.}

]

Every natural number has a unique @Peano encoding. If @${n} is a natural
number, its @Peano encoding is a series of @${n} @Succs terminated by a @Zero.
The number 3, for example, is encoded as @code{(Succ (Succ (Succ Zero)))}.

By definition, @Zero is not the successor of any other Peano number, and @Succ
is an injective function from one Peano number to another. Whereas every Peano
number is a member of the @Peano @tech{sum}, the converse is not true in
general. Proving that an @tech{instance} of @Peano really is a Peano number
takes some work:

@example[
  (define peano? (function [Zero #t] [(Succ n) (peano? n)] [_ #f]))
  (peano? (Succ Zero))
  (peano? (Zero Succ))
]

Now that we know what Peano number are, how do we use them?

@subsubsection{Peano Arithmetic & Recursion}

Addition (+) and multiplication (×) on Peano numbers are defined inductively:

@tabular[
  #:style full-width
  @list[
    @list[
      @relation[
                  @list{@${n} + 0} = @${n}
        @list{@${n} + @Succ @${m}} = @list{@Succ(@${n} + @${m})}
      ]
      @relation[
                  @list{@${n} × 0} = "0"
        @list{@${n} × @Succ @${m}} = @list{@${a} + @${a} × @${b}}
      ]
    ]
  ]
]

The core calculus can express these relationships directly:

@centered{
  @relation[
    "add" = @list{φ(@${a} @Zero).@${a};φ(@${a} (@Succ @${b})).@Succ(add (@${a} @${b}))}
    "mul" = @list{φ(@${a} @Zero).@Zero;φ(@${a} (@Succ @${b})).add(@${a} (mul (@${a} @${b})))}
  ]
}

and so can @list[@algebraic-mod ":"]

@example[
  (define add
    (function*
      [(n Zero) n]
      [(n (Succ m)) (Succ (add n m))]))
  (add (Succ Zero) (Succ (Succ Zero)))
]

@example[
  (define mul
    (function*
      [(n Zero) Zero]
      [(n (Succ m)) (add n (mul n m))]))
  (mul (Succ (Succ Zero)) (Succ (Succ (Succ Zero))))
]

Both functions have two clauses: a base case and an inductive step. The
inductive step is a recursive call. If we had a @racket[letrec] form, we could
express the recursion directly:

@core-code{
  (letrec
      [add ($ (φ (a Zero) a)
              (φ (a (Succ b)) (Succ (add (a b)))))]
    (add ((Succ Zero) (Succ (Succ Zero)))))
}

But our interpreter just isn't there yet. With a simple rewrite, we can
express @racket[letrec] as @racket[let] plus a
@hyperlink["https://en.wikipedia.org/wiki/Fixed-point_combinator#Strict_fixed_point_combinator"]{fixed-point
combinator}:

@relation[
  @tt{(letrec [id val] body)} ↝ @tt{(let [id (fix (φ id val))] body)}
]

With a second rewrite, we can express @id[let] as a function application:

@relation[
  @tt{(let [id val] body)} ↝ @tt{((φ id body) val)}
]

And if we fold this rewrite into the first one, the resulting form contains
only functions and applications:

@relation[
  @tt{(letrec [id val] body)} ↝ @tt{(let [id (fix (φ id val))] body)}
  ~                           ↝ @tt{((φ id body) (fix (φ id val)))}
]

The @id[fix] function gives us a way to encode functions that might call
themselves.

@centered{
  fix = φ@${f}.(φ@${x}.@${f}(φ@${y}.@${(x x) y}))
               (φ@${x}.@${f}(φ@${y}.@${(x x) y}))
}

With the following encoding of @id[fix], our interpreter will be able to
handle the functional encodings of @id[let] and @id[letrec]:

@core-code{
  (φ f
    ((φ x (φ f (φ y ((x x) y))))
     (φ x (φ f (φ y ((x x) y))))))
}

We'll start with a program in the hypothetical syntax:

@core-code{
  (let [fix (φ f
              ((φ x (φ f (φ y ((x x) y))))
               (φ x (φ f (φ y ((x x) y))))))]
     (letrec [add ($ (φ (a Zero) a)
                     (φ (a (Succ b)) (Succ (add (a b)))))]
       (add ((Succ Zero) (Succ (Succ Zero))))))
}

and turn it into a runnable program with two rewrites. First, eliminate the
outer @id[let]:

@core-code{
  ((φ fix
     (letrec [add ($ (φ (a Zero) a)
                     (φ (a (Succ b)) (Succ (add (a b)))))]
       (add ((Succ Zero) (Succ (Succ Zero))))))
   (φ f
     ((φ x (φ f (φ y ((x x) y))))
      (φ x (φ f (φ y ((x x) y)))))))
}

Second, eliminate the inner @id[letrec]:

@core-code{
  ((φ fix
     ((φ add
        (add ((Succ Zero) (Succ (Succ Zero)))))
      (fix (φ add ($ (φ (a Zero) a)
                     (φ (a (Succ b)) (Succ (add (a b)))))))))
   (φ f
     ((φ x (f (φ y ((x x) y))))
      (φ x (f (φ y ((x x) y)))))))
}

This program runs. It calculates 1 + 2 = 3 with Peano numbers. To run it, put
the code in a @id[.rkt] file that begins with @hash-lang[algebraic/model/core]
and point Racket at the file.

@core-example[
  ((φ fix
     ((φ add
        (add ((Succ Zero) (Succ (Succ Zero)))))
      (fix (φ add ($ (φ (a Zero) a)
                     (φ (a (Succ b)) (Succ (add (a b)))))))))
   (φ f
     ((φ x (f (φ y ((x x) y))))
      (φ x (f (φ y ((x x) y)))))))
]

We can even turn it into a unit test inside the @id[algebraic/model/core]
module:

@algebraic-code{
  (module+ test
    (require rackunit)
    (check equal?
           (algebraic
            ((φ fix
               ((φ add
                  (add ((Succ Zero) (Succ (Succ Zero)))))
                (fix (φ add ($ (φ (a Zero) a)
                               (φ (a (Succ b)) (Succ (add (a b)))))))))
             (φ f
               ((φ x (f (φ y ((x x) y))))
                (φ x (f (φ y ((x x) y)))))))
          '(Succ (Succ (Succ Zero))))))
}

By similar treatment, we can transform the hypothetical program:

@core-code{
  (let [fix (φ f
              ((φ x (φ f (φ y ((x x) y))))
               (φ x (φ f (φ y ((x x) y))))))]
     (letrec [add ($ (φ (a Zero) a)
                     (φ (a (Succ b)) (Succ (add (a b)))))]
       (letrec [mul ($ (φ (a Zero) Zero)
                       (φ (a (Succ b)) (add (a (mul (a b))))))]
         (mul ((Succ (Succ Zero)) (Succ (Succ (Succ Zero))))))))
}

into a runnable program that computes 2 × 3 = 6 with Peano numbers in three
rewrites:

@core-example[
  ((φ fix
     ((φ add
        ((φ mul
           (mul ((Succ (Succ Zero)) (Succ (Succ (Succ Zero))))))
         (fix (φ mul ($ (φ (a Zero) Zero)
                        (φ (a (Succ b)) (add (a (mul (a b))))))))))
      (fix (φ add ($ (φ (a Zero) a)
                     (φ (a (Succ b)) (Succ (add (a b)))))))))
   (φ f
     ((φ x (f (φ y ((x x) y))))
      (φ x (f (φ y ((x x) y)))))))
]

@subsubsection{Boolean Logic}

The Booleans are a popular example of an enumerated type. Assuming two
constructors @False and @True that take no arguments, logical negation is a
short function.

@centered{
  @relation[
    "not" = @list{φ@id[False].@id[True];φ_.@id[False]}
  ]
}

Logical conjunction and disjunction, however, should exhibit ``short
circuiting'' behavior so only the arguments needed to determine the result are
evaluated. This behavior is impossible to implement directly with ordinary
functions, but macros make short work of them. We'll even throw in a
short-circuiting @racket[xor] for kicks.

@relation[
  "and" = @list{μ(@${a} @${b}).(φ@id[False].@False;φ_.@${b}) @${a}}
  "or"  = @list{μ(@${a} @${b}).(φ@id[False].@${b};φ@${x}.@${x}) @${a}}
  "xor" = @list{μ(@${a} @${b}).(φ@id[False].@${b};φ@${x}.and ((not @${b}) @${x})) @${a}}
]

In all of these operations, any non-@False value is equivalent to @True, and
the original value is preserved in the result whenever possible.

The @id[and] macro takes two arguments. If the first argument evaluates to
@False, the result is @False and the second argument is not evaluated.
Otherwise, the result is determined by the second argument. It works because
the outer macro captures @${a} and @${b} before they can be evaluated. It
forces the evaluation of @${a} by applying the inner function to it, which
then maps any non-@False value to @${b}.

The @id[or] macro uses the same technique, this time returning @${b} only if
@${a} evaluates to @False.

The @id[xor] macro goes through a little extra trouble to return the appropriate
value when the 

Encoding these macros with the technique covered in the previous section is
straight forward. Starting from the hypothetical program:

@core-code{
   (let [not ($ (φ False True) (φ _ False))]
     (let [and (μ (a b) (($ (φ False False) (φ _ b)) a))]
       (let [or (μ (a b) (($ (φ False b) (φ x x)) a))]
         (let [xor (μ (a b) (($ (φ False b) (φ x (and ((not b) x)))) a))]
           (or ((not 1) (and ((xor (2 3)) 4))))))))
}

We can derive a runnable program in four rewrites:

@core-example[
  ((φ not
     ((φ and
        ((φ or
           ((φ xor
              (or ((not True) (and ((xor (True True)) True)))))
            (μ (a b) (($ (φ False b) (φ x (and ((not b) x)))) a))))
         (μ (a b) (($ (φ False b) (φ x x)) a))))
      (μ (a b) (($ (φ False False) (φ _ b)) a))))
   ($ (φ False True) (φ _ False)))
]

This program calculates @list["¬" @True "∨(" @True "⊗" @True ")∧" @True] =
@False.

@subsubsection{Lists}

@; =============================================================================

@section[#:tag "tut:ext"]{A Syntax Extension}

@defmodulelang[algebraic/model/ext]

@; =============================================================================

@section[#:tag "tut:hosted"]{A Hosted Variant}

@defmodulelang[algebraic/model/hosted]
