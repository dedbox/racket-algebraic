#lang scribble/manual

@title[#:tag "tut"]{Tutorial Series: From Models to Interpreters}

@require{./algebraic-includes.rkt}
@(require texmath
          (for-label (except-in algebraic/racket/base fun?)))

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

@; -----------------------------------------------------------------------------

@section[#:tag "tut:core"]{The Core Calculus}

@defmodulelang[algebraic/model/core]

The core model has three major components:

@itemlist[

  @item{A context-free grammar for the formal syntax,}

  @item{An operational semantics that defines evaluation and pattern matching
  operations, and}

  @item{A collection of sample programs and their expected normal forms.}

]

@subsection*{Syntax}

The core defines three syntactic categories: terms, values, and patterns.

A @emph{term} (@${t}) is a concrete expression of the calculus.

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

@subsection*{Evaluation Semantics}

Every interpreter in the tutorial series provides its own ``top level''
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

The @id[interpret] @tech{function} drives the computation of @id[t] in a tight
loop that calls another @tech{function} named @id[step].

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

The @id[interpret] @tech{function} takes a @id[Term] for the current step of a
computation and returns the next step or @racket[#f] if stuck. When @id[step]
returns a @id[value?], the computation halts and the result is @id[show]n.

If a term gets stuck, we'll want to trace the steps of the computation to
figure out what went wrong. We could just toggle the commented middle line in
a pinch, but the generic looping behavior of @id[interpret] is easy to capture
with a @tech{macro}.

@margin-note{

  The colon-delimited @stech{syntax class} name (@id[:id]) labels each
  @tech{macro} argument as an identifier. These extra bits of notation give
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

The @id[define-interpreter] form looks like the @racket[define] shorthand for
a unary function. Its body calculates the next step of the computation based
on the current step and either produces a value or gets stuck.

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
to definitions so we can change the order or rule application without having
to touch the definitions.

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

Most of the core rules have this @racket[(let ([t* ...]) (and t* ...))] form,
so we'll put it in a @tech{macro}.

@algebraic-code{
  (define-syntax sub-step
    (μ* (t*-expr:expr t*:id result:expr)
      (let ([t* t*-expr]) (and t* result))))
}

The @id[sub-step] form lets us express structurally recursive steps a little
more compactly:

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

The @id[subst] @tech{function} implements our variable substition algorithm.
It takes a given @rtech{hash} @id[σ] that maps variable names to @Terms and
applies those mappings to a given @Term @id[t].

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

  (define vars
    (function
      [(PApp p1 p2) (set-union (vars p1) (vars p2))]
      [(PSeq p1 p2) (set-union (vars p1) (vars p2))]
      [(PVar x1) (seteq x1)]
      [(PCon _) (seteq)]
      [PWil (seteq)]
      [PUni (seteq)]))
}

The @id[subst] @tech{function} passes around an optional @rtech{set} named
@id[mask] to remember which variables are already bound in the active
substitution context. Thanks to @id[α-rename], variable names can be compared
with @racket[eq?].

The @id[vars] @tech{function} takes a @Patt and returns the @rtech{set} of the
variable names it binds.

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

but it turns out we can merge these four rules into two:

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
    (μ* ((abs:expr val:expr) (alt:expr alt-val:expr))
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

When assembled, the fragments define a comprehensive @id[step]
@tech{function}.

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

@subsection*{Pattern Matching Semantics}

Pattern matching is a binary operation on pattern-term pairs which produces a
set of variable bindings if the pattern and term are compatible in one of the
following six ways.

@centered{
  @${(p_{11} p_{12})×(t_{21} t_{22}) = (p_{11}×t_{21})∪(p_{12}×t_{22})}

  @${(p_{11};p_{12})×(t_{21};t_{22}) = (p_{11}×t_{21})∪(p_{12}×t_{22})}

  @${δ_1×δ_2 = \{\} if δ_1=δ_2}

  @${x_1×t_2 = \{x_1↦t_2\}}

  _@${×· = \{\}}

  @${◊×◊ = \{\}}
}

An @emph{application pattern} (@${p_1 p_2}) matches an application if the
sub-patterns and sub-terms all match by position, passing along any variable
bindings.

A @emph{sequence pattern} (@${p_1;p_2}) behaves similarly.

A @emph{constructor pattern} (@${δ}) matches a constructor with the same name
and binds no variables.

A @emph{variable} (@${x}) matches any term and binds itself to the term.

The @emph{wildcard} (_) matches any term and binds no variables.

The @emph{unit pattern} (◊) matches only the unit value.

Any other combination is undefined.

@subsubsub*section{The Cross Function}

The @id[×] @tech{function} implements these six equations directly, with a
little extra code to check for valid sub-matches before performing any
@rtech{set} operations.

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
and term refer to the same constructor because the @tech{function} pattern
only matches when the two names are @racket[equal?].

@; -----------------------------------------------------------------------------

@subsection*{Pragmatics}

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

The @id[α-rename] @tech{function} uses the familiar @id[term]/@id[patt] parser
combinator technique. It takes a @Patt and a @Term and returns a copy of each
with every variable bound by the @Patt consistently renamed across both of the
arguments.

Most of the code is just plumbing for structural recursion or no-ops, but the
@TVar and @PVar cases are more interesting. If the name of a variable is
@racket[equal?] to the name @id[x] being substituted, the name is replaced
with @id[y].

The body of the @id[α-rename] @tech{function} does three things:

@itemlist[
  #:style 'ordered

  @item{Extracts the variables of a @Patt,}

  @item{Generates a fresh @rtech{uninterned} symbol with the same name as each
  variable extracted, and}

  @item{Recursively substitutes the new name for the original in both arguments.}

]

We're using @rtech{uninterned} symbols because they are @racket[eq?] to
themselves only, which makes them easy to put into @rtech{sets} and
@rtech{hash}es.

But why write our own @id[genvar] @tech{function} instead of just using the
stock @racket[gensym]?

A big advantage of @rtech{uninterned} symbols or @racket[gensym]ed symbols is
the @rtech{uninterned} symbols look exactly the same as the originals when
printed. The only wrinkle is, Racket still knows the difference. If our
@id[show] @tech{function} leaks @rtech{uninterned} symbols into the surface
syntax, we won't be able to compare them with @racket[equal?]. The
@id[α-restore] @tech{function} reproduces the @rtech{interned} variable names
for printing.

@algebraic-code{
  (define (α-restore x)
    (string->symbol (symbol->string x)))
}

@subsection*{Putting It All Together}

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

To do that, we need to provide a little boilerplate and an extra file. When
everything is in place, we should be able to enter a @id[.rkt] file with the
first line set to @hash-lang[algebraic/model/core] (or whatever collection
your module is installed in) and interact with our interpreter directly.

@algebraic-code{
  (define-syntax module-begin
    (μ* (form ...)
      (#%plain-module-begin ((current-print) (algebraic form)) ...)))

  (define-syntax top-interaction
    (μ* form (#%top-interaction . (algebraic form))))
}

The @id[module-begin] form replaces the stock @racket[#%module-begin] form,
which wraps non-interactive top-level expressions ``to print
non-@seclink["void+undefined" #:doc '(lib
"scribblings/guide/guide.scrbl")]{@void-const} results using
@racket[current-print].'' Our version wraps those expressions with the
@id[algebraic] form before they are evaluated.

The @id[top-interaction] form is a hook into the default
@racket[read-eval-print-loop]. It wraps interactive inputs with @id[algebraic]
before they are evaluated.

To be sure our custom top-level forms replace the defaults, we can export them
with different names.

@algebraic-code{
  (provide (all-defined-out)
           (for-syntax (all-defined-out))
           (rename-out [module-begin #%module-begin]
                       [top-interaction #%top-interaction])
           #%app #%datum)
}

We're exporting the default @racket[#%app] and @racket[#%datum] forms in
addition to our @racket[data] definitions, @tech{functions}, and
@tech{macros}. Racket will complain if we don't export something under those
two names and the default behavior is fine.

When Racket sees @hash-lang[algebraic/model/core] at the beginning of a source
file, it tries to load another module named
@id[algebraic/model/core/lang/reader] that tells Racket how to use the @shlang
module:

@typeset-code["#lang s-exp syntax/module-reader algebraic/model/core"]

The @racketmodname[syntax/module-reader] language turns a @gtech{module path}
into a @gtech{module language} with the default @gtech{reader}, and the
@racketmodname[s-exp] meta-language turns the @gtech{module language} into a
@shlang.

@subsection*{Writing and Running Programs}

@subsubsub*section{Arithmetic}

@subsubsub*section{Boolean Logic}

@subsubsub*section{Lists}

@; -----------------------------------------------------------------------------

@section[#:tag "tut:ext"]{A Syntax Extension}

@defmodulelang[algebraic/model/ext]

@; -----------------------------------------------------------------------------

@section[#:tag "tut:hosted"]{A Hosted Variant}

@defmodulelang[algebraic/model/hosted]
