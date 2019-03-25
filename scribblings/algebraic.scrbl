#lang scribble/manual

@title{Algebraic structures for untyped Racket}
@author{@author+email["Eric Griffis" "dedbox@gmail.com"]}

@(require
  racket/sandbox
  scribble/core
  scribble/examples
  scribble/html-properties
  syntax/parse/define
  texmath
  (rename-in scribble/base [~ nbsp])
  (for-syntax racket/base)
  (for-label (except-in algebraic/racket/base fun?)
             racket/contract/base
             racket/match
             (except-in syntax/parse pattern)))

@(define (rtech . args)
   (apply tech #:doc '(lib "scribblings/reference/reference.scrbl") args))

@(define (stech . args)
   (apply tech #:doc '(lib "syntax/scribblings/syntax.scrbl") args))

@(define-simple-macro (id x) (racketid x))

@; -----------------------------------------------------------------------------
@; Syntax

@(define-simple-macro (define-ids x:id ...+)
   (begin (define x @racketid[x]) ...))

@define-ids[Term  TApp  TSeq  TFun  TMac  TVar  TCon  TUni ]
@define-ids[Term? TApp? TSeq? TFun? TMac? TVar? TCon? TUni?]
@define-ids[Patt  PApp  PSeq  PWil  PVar  PCon  PUni ]
@define-ids[Patt? PApp? PSeq? PWil? PVar? PCon? PUni?]

@(define Terms @list{@racketid[Term]s})
@(define Term. @list{@racketid[Term].})
@(define Patt. @list{@racketid[Patt].})

@(define App1 @list{A@smaller{PP}1})
@(define App2 @list{A@smaller{PP}2})
@(define Seq1 @list{S@smaller{EQ}1})
@(define Seq2 @list{S@smaller{EQ}2})
@(define AppF @list{A@smaller{PP}F})
@(define AppM @list{A@smaller{PP}M})
@(define Fun1 @list{F@smaller{UN}1})
@(define Fun2 @list{F@smaller{UN}2})
@(define Mac1 @list{M@smaller{AC}1})
@(define Mac2 @list{M@smaller{AC}2})

@; -----------------------------------------------------------------------------
@; algebraic eval

@(define algebraic-eval
   (call-with-trusted-sandbox-configuration
    (λ ()
      (parameterize
          ([sandbox-output 'string]
           [sandbox-error-output 'string])
        (make-base-eval #:lang 'algebraic/racket/base
          '(require (for-syntax syntax/parse)
                    racket/function))))))

@(define-syntax-rule (example expr ...)
   @examples[#:eval algebraic-eval #:label #f expr ...])

@; @(define-simple-macro (algebraic-code str ...)
@;    #:with stx (datum->syntax this-syntax 1)
@;    @typeset-code[#:context #'stx "#lang algebraic/racket/base\n\n" str ...])

@(define-simple-macro (algebraic-code str ...)
   #:with stx (datum->syntax this-syntax 1)
   @typeset-code[
     #:context #'stx
     #:keep-lang-line? #f
     "#lang algebraic/racket/base\n" str ...
   ])

@; core eval

@(define core-eval
   (call-with-trusted-sandbox-configuration
    (λ ()
      (parameterize
          ([sandbox-output 'string]
           [sandbox-error-output 'string])
        (make-base-eval #:lang 'algebraic/model/core)))))

@(define-syntax-rule (core-example expr ...)
   @examples[#:eval core-eval #:label #f expr ...])

@(define-simple-macro (core-code str ...)
   @typeset-code["#lang algebraic/model/core\n\n" str ...])

@; @(define-simple-macro (core-code str ...)
@;    @typeset-code[#:keep-lang-line? #f "#lang algebraic/model/core\n" str ...])

@; ext eval

@(define ext-eval
   (call-with-trusted-sandbox-configuration
    (λ ()
      (parameterize
          ([sandbox-output 'string]
           [sandbox-error-output 'string])
        (make-base-eval #:lang 'algebraic/model/ext)))))

@(define-syntax-rule (ext-example expr ...)
   @examples[#:eval ext-eval #:label #f expr ...])

@; hosted eval

@(define hosted-eval
   (call-with-trusted-sandbox-configuration
    (λ ()
      (parameterize
          ([sandbox-output 'string]
           [sandbox-error-output 'string])
        (make-base-eval #:lang 'algebraic/model/hosted)))))

@(define-syntax-rule (hosted-example expr ...)
   @examples[#:eval hosted-eval #:label #f expr ...])

@(define-syntax-rule (core-codeblock expr ...)
   @examples[#:eval hosted-eval #:label #f #:no-prompt #:no-result expr ...])

@; odds and ends

@(define-syntax-rule (hash-lang mod)
   (list
    (seclink "hash-lang" #:doc '(lib "scribblings/guide/guide.scrbl") "#lang")
    " "
    (racketmodname mod)))

@(define algebraic-mod @racketmodlink[algebraic/racket/base]{algebraic})

@(define (subsection* #:tag [tag #f] . args)
   (apply subsection #:tag tag #:style 'unnumbered args))

@(define (subsubsection* #:tag [tag #f] . args)
   (apply subsubsection #:tag tag #:style '(unnumbered toc-hidden) args))

@(define full-width
   (make-style "fullwidth"
               (list (make-css-addition "scribblings/css/fullwidth.css"))))

@(define grammar-style
   (make-style "grammar"
               (list (make-css-addition "scribblings/css/grammar.css"))))

@(define (grammar name . rules)
   (tabular
    #:sep (hspace 1)
    #:style grammar-style
    #:column-properties '(left center left right)
    (let loop ([fst (emph name)]
               [snd "⩴"]
               [rules rules])
      (if (null? rules)
          null
          (cons (list fst snd (caar rules) (list (hspace 4) (cadar rules)))
                (loop "" "|" (cdr rules)))))))

@; #############################################################################

This package provides @hash-lang[algebraic/racket/base] which extends
@hash-lang[racket/base] with free-form, lexically scoped
@seclink["sec:data"]{algebraic data structures} along with complementary
@seclink["sec:functions"]{function} and @seclink["sec:macros"]{macro}
abstractions with a uniform and compact destructuring syntax.

@; =============================================================================

@section{Overview}

@hash-lang[algebraic/racket/base] synthesizes and specializes the
functionality of @racket[struct], @racket[match-lambda], and
@racket[syntax-parser] to streamline the funtional programming experience in
vanilla Racket in two key areas:

@subsubsub*section{Consistent Syntax}

The destructuring syntax for algebraic @racket[data] and most other data is
the same across all @tech{function}- and @tech{macro}-producing forms.

@subsubsub*section{Full Transparency}

Algebraic data @tech{constructors} are like type tags. When applied to an
argument list, they produce an @tech{instance}---a @deftech{list} or ordered
sequence of unnamed fields with the @tech{constructor} at its head. They are
easy to print and easy to parse, like @rtech{prefab} structs. The main
difference is algebraic @tech{constructors} are lexically scoped and have a
natural ordering.

@; -----------------------------------------------------------------------------

@subsection*[#:tag "sec:data"]{Data}

The @racket[data] form defines a variety of procedures and syntax
@rtech{transformers} for working with named @tech{products} and @tech{sums}.

A @deftech{product} identifies a family of structures comprising an ordered
set of @deftech{fields}, and a @deftech{sum} identifies a @tech{list} of
@tech{products}.

@example[
  (data Peano (Succ Zero))
]

In this example, @racket[Peano] is a @tech{sum} comprising the @tech{products}
@racket[Succ] and @racket[Zero].

Each @tech{product} name is bound to a @deftech{constructor}, a function that
creates @tech{instances} of the named @tech{product}. An @deftech{instance} is
a concrete expression of the @tech{product} as a tagged tuple of run-time
values or expansion-time syntax fragments.

@example[
  (Succ Zero)
  (instance? (Succ Zero))
]

Equality is decided structurally for @tech{constructors} and their
@tech{instances}.

@example[
  (equal? Succ Succ)
  (equal? (Succ Zero) (Succ Zero))
  (equal? (Succ Zero) (Succ (Succ Zero)))
]

The @racket[data] form also defines several membership predicates.

@example[
  (Succ? Succ)
  ((sum Peano?) Succ)
  ((sum Peano?) (sum Peano))
]

To prevent name clashes in types like @racket[Unit], @tech{sum} bindings are
defined in their own @rtech{namespace}. The @racket[sum] form merely adds the
appropriate @rtech{scope} to an identifier.

@; -----------------------------------------------------------------------------

@subsection*[#:tag "sec:functions"]{Functions}

A @deftech{function} is a procedure that either deconstructs or rejects a
fully-evaluated argument or argument list. Functions are created with the
single-argument @racket[φ] (or @racket[phi]) and @racket[function] forms, or
their multi-argument variants @racket[φ*] (or @racket[phi*]) and
@racket[function*].

The @racket[φ] (@racket[phi]) form creates a @tech{function} of exactly one
argument with exactly one clause.

@example[
  (define inc (φ a (Succ a)))
  (define dec (φ (Succ b) b))
  (function? inc)
  (inc Zero)
  (dec (Succ (Succ Zero)))
]

The @racket[φ*] (@racket[phi*]) form creates a @tech{function} of any number
of arguments with exactly one clause.

@example[
  (define cmp
    (φ* (a b)
      ((cond [(number? a) <] [(char? a) char<?]) a b)))
  (cmp 1 2)
  (cmp #\y #\x)
]

The @racket[function] form creates a @tech{function} of exactly one argument
with one or more clauses.

@example[
  (define peano
    (function [0 Zero]
              [n (Succ (peano (- n 1)))]))
  (define num
    (function [Zero 0]
              [(Succ p) (+ 1 (num p))]))
  (peano 3)
  (num (Succ (Succ (Succ Zero))))
]

The @racket[function*] form creates a @tech{function} of any number of
arguments with one or more clauses.

@example[
  (define add
    (function* [(a Zero) a]
               [(a (Succ b)) (Succ (add a b))]))
  (num (add (peano 3) (peano 2)))
]

@tech{Functions} created by @racket[function*] can have clauses with no
arguments, and the number of arguments for each clause can vary.

@example[
  (define num-args
    (function* [() 0]
               [(_ . rest) (+ 1 (apply num-args rest))]))
  (num-args)
  (num-args - -)
  (num-args - - - - -)
]

@; -----------------------------------------------------------------------------

@subsection*[#:tag "sec:macros"]{Macros}

A @deftech{macro} is a syntax @rtech{transformer} that either deconstructs or
rejects an argument or argument list at @rtech{expansion} time. Macros are
created with the single-argument @racket[μ] (or @racket[mu]) and
@racket[macro] forms, or the multi-argument variants @racket[μ*] (or
@racket[mu*]) and @racket[macro*].

The @racket[μ] (@racket[mu]) form creates a @tech{macro} of exactly one
argument with exactly one clause.

@example[
  (define-syntax infix (μ (a op b) (op a b)))
  (infix (5 - 3))
]

The @racket[μ*] (@racket[mu*]) form creates a @tech{macro} of any number of
arguments with exactly one clause.

@example[
  (define-syntax --> (μ* (p q) (or (not p) q)))
  (for*/list ([p '(#t #f)] [q '(#t #f)]) (--> p q))
]

The @racket[macro] form creates a @tech{macro} of exactly one argument with
one or more clauses.

@example[
  (define-syntax bin (macro [#f 0] [_ 1]))
  (bin #f)
  (bin (values #f))
]

The @racket[macro*] form creates a @tech{macro} of any number of arguments
with one or more clauses.

@example[
  (define-syntax and2 (macro* [(a b) ((function [#f #f] [_ b]) a)]))
  (define-syntax and*
    (macro* [() #t]
            [(a b) (and2 a b)]
            [(a bs ...) (and2 a (and* bs ...))]))
  (and* 2 #f 0)
  (and* 2 1 0)
]

@tech{Macros} are designed to simplify mundane meta-programming tasks. The
following example is a run-time implementation of the ``power'' function from
@cite{Taha2004}:

@example[
  (define f-power
    (function*
      [(0 _) 1]
      [(n x) (* x (f-power (- n 1) x))]))
  (map (curry f-power 3) '(0 1 2 3 4 5 6))
]

With @racket[unsyntax] and the @racket[var] form, applications of the power
function can be unrolled at expansion time.

@example[#:escape UNSYNTAX
  (define-syntax m-power
    (macro*
      [(0 _) 1]
      [(1 x) x]
      [(n:nat x) (* x (m-power #,(- (var n) 1) x))]))
  (define-syntax m-power3 (μ y (m-power 3 y)))
  (map (φ x (m-power3 x)) '(0 1 2 3 4 5 6))
]

With @racket[quote] and @racket[local-expand], the generated code can be
exposed.

@example[#:escape UNSYNTAX
  (define-syntax q-power
    (macro*
      [(0 _) 1]
      [(1 x) x]
      [(n:nat x)
       '#,(local-expand #`(* x (q-power #,(- (var n) 1) x))
                        'expression null)]))
  (q-power 3 2)
]

@; =============================================================================

@section[#:tag "tut"]{Tutorial Series: From Models to Interpreters}

A core use case for @algebraic-mod is fast and cheap interpreter development.
With the right tooling, throw-away interpreters can be an easy way to explore
and validate language design choices quickly.

As it happens, Haskell is already pretty good at this. Algebraic data types
and functional destructuring syntax make interpreters easy to read and write,
and its type system keeps track of pervasive breaking changes for you.
@algebraic-mod addresses the untyped half of this equation---algebraic data
@emph{structures} (ADTs sans typing constraints) with destructuring syntax.

In this tutorial, we will build a series of interpreters based on the models
originally used to design @algebraic-mod itself:

@itemlist[
  #:style 'ordered

  @item{A @emph{core} model based on the untyped λ-calculus,}

  @item{An @emph{extended} syntax that compiles down to core constructs, and}

  @item{A @emph{hosted} variant that borrows additional constructs from the
    implementing platform.}

]

@; -----------------------------------------------------------------------------

@subsection[#:tag "tut:core"]{The Core Calculus}

@defmodulelang[algebraic/model/core]

The core interpreter has three major components:

@itemlist[

  @item{A context-free grammar for the formal syntax,}

  @item{Operational semantics for evaluation and pattern matching, and}

  @item{A collection of helper functions that implement the operational
    semantics.}

]

@subsubsection*{Syntax}

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

Later, we'll implement a @tech{function} named @id[α-rename-clause] to
automate this variable-renaming process. For now, we'll just use it to finish
the @id[parse] @tech{function}.

@algebraic-code{
  (define (parse t)
    (define term
      (function
        [(  t1 t2) (TApp (term t1) (term t2))]
        [($ t1 t2) (TSeq (term t1) (term t2))]
        [('φ p1 t2) (values-> TFun (α-rename-clause (patt p1) (term t2)))]
        [('μ p1 t2) (values-> TMac (α-rename-clause (patt p1) (term t2)))]
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

The @id[α-rename-clause] @tech{function} takes a @Patt and a @Term. It returns
two values: a copy of the @Patt with all of its variables renamed, and a copy
of the @Term with its variables renamed as they were in the @Patt.

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
undo whatever @id[α-reduce-clause] does to variable names.

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

@subsubsection*{Evaluation Semantics}

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
substitution context. Thanks to @id[α-rename-clause], variable names can be
compared with @racket[eq?].

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

@subsubsection*{Pattern Matching Semantics}

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

@subsubsection*{Pragmatics}

It's time to pay the piper.

α-rename-clause

α-restore

@; -----------------------------------------------------------------------------

@subsection[#:tag "tut:ext"]{A Syntax Extension}

@defmodulelang[algebraic/model/ext]

@; -----------------------------------------------------------------------------

@subsection[#:tag "tut:hosted"]{A Hosted Variant}

@defmodulelang[algebraic/model/hosted]

@; =============================================================================

@section[#:tag "ref"]{API Reference}

@defmodulelang[algebraic/racket/base]

@; -----------------------------------------------------------------------------

@subsection[#:tag "ref:sums-products"]{Sums and Products}

@defform[
  (data sum-decl ...+)
  #:grammar [(sum-decl (code:line sum-id (product-id ...)))]
]{

  Creates a new @tech{sum} on a @tech{list} of @tech{products} and binds
  variables related to them.

  A @tech{sum} with @var[n] @tech{products} defines 3+3@var[n] names:

  @itemlist[
    @item{for each @var[sum-id]:
      @itemlist[

        @item{a @rtech{transformer} binding that encapsulates information
          about the @tech{sum} declaration.}

        @item{a @tech{sum} structure that can be printed to retrieve its
          definition.}

      ]
    }

    @item{@var[sum-id]@tt{?}, for each @var[sum-id]; a predicate that returns
      @racket[#t] for the @tech{sum} bound to @var[sum-id], its
      @tech{constructors} or their @tech{instances}, and @racket[#f] for any
      other value.}

    @item{for each @var[product-id]:
      @itemlist[

        @item{a @rtech{transformer} binding that encapsulates information
          about the @tech{product} declaration.}

        @item{a @tech{constructor} that takes any number of arguments and
          returns a new @tech{instance} of the @tech{product}.}

      ]
    }

    @item{@var[product-id]@tt{?}, for each @var[product-id]; a predicate that
      returns @racket[#t] for the @tech{constructor} bound to @var[product-id]
      or its instances and @racket[#f] for any other value.}

  ]

  Example:
  @example[
    (data Unit (Unit))
    (Unit? Unit)
    (Unit? (Unit))
    ((sum Unit?) Unit)
    ((sum Unit?) (sum Unit))
  ]
}

@defform[(sum id)]{

  Adds the @tech{sum} scope to @var[id].

  To prevent clashes between @tech{sums} and @tech{products} with the same
  name, @tech{sum} bindings are defined in their own namespace. This form adds
  a scope to @var[id] that represents the @tech{sum} namespace.

  Example:
  @example[
    (data Either (Left Right))
    (eval:error (Either? Left))
    ((sum Either?) Left)
    (eval:error ((sum Either?) Either))
    ((sum Either?) (sum Either))
  ]
}

@defproc[(data-less-than? [Π1 product?] [Π2 product?]) boolean?]{

  Returns @racket[#t] if the arguments are in the defined order.

  Examples:
  @example[
    (data ABC (A B C))
    (values (data-less-than? A C) (data-less-than? C A))
  ]

  @example[
    (data XYZ (X Y Z))
    (sort (list Z Y X) data-less-than?)
  ]
}

@defproc[(sum? [v any/c]) boolean?]{

  Returns @racket[#t] if @var[v] is a @tech{sum}.

}

@defproc[(product? [v any/c]) boolean?]{

  Returns @racket[#t] if @var[v] is a @tech{constructor}.

}

@defproc[(instance? [v any/c]) boolean?]{

  Returns @racket[#t] if @var[v] is an @tech{instance}.

}

@; -----------------------------------------------------------------------------

@subsection[#:tag "ref:functions"]{Functions}

@deftogether[(
@defform[(φ patt maybe-if ... body ...+)]
@defform/subs[
  #:literals (quasiquote unquote quote void)
  (phi patt body ...+)
  [(patt literal
         wildcard-id
         variable-id
         product-id
         (product-id patt ...)
         (product-id patt ... . patt)
         reference-id
         (patt #:if cond-expr)
         (patt #:as patt)
         regexp
         (regexp patt ...+)
         (regexp patt ... . patt)
         symbol
         (patt ...)
         (patt ... . patt)
         (struct-id ([field patt] ...))
         (struct-id patt ...)
         (quasiquote fqp)
         (void)
         #,(racketparenfont "#(" (var patt) ")")
         #,(racketparenfont "#&" (var patt))
         #,(racketparenfont "#hash([" (var key) " . " (var patt) "] ...)"))
   (literal boolean
            character
            number
            string
            bytes
            (quote datum))
   (fqp literal
        id
        ()
        (fqp . fqp)
        (unquote patt))
   (maybe-if (code:line)
             #:if cond-expr)]
])]{

  Creates a @tech{function} of one argument with one clause.

  Optional @racket[#:if] @var[cond-expr]s specify that the pattern should only
  match if the @var[cond-expr]s produce true values. @var[cond-expr] is in the
  scope of all of the variables bound in @var[patt].

  Example:
  @example[
    (data SZ (S Z))
    (let ([f (function
               [(S x y) #:if (not x) #:if (not y) 0]
               [(S x y) #:if (not (and x y)) (or x y)]
               [(S x y) #:if x #:if y (+ x y)])])
      (map f (list (S 1 2) (S #f 2) (S 1 #f) (S #f #f))))
  ]

  A @var[patt] has one of the following forms:

  @specsubform[literal]{

    A Racket literal value: @racket[#t], @racket[#f], @var[character],
    @var[number], @var[string], @var[bytes], or @tt{(quote @var[datum])}.

    Matches an @racket[equal?] constant.

    Example:
    @example[
      ((φ "one" 1) "one")
    ]
  }

  @specsubform[wildcard-id]{

    An identifier whose name begins with an underscore ``@racketid[_]''.

    Matches anything, without binding any identifiers.

    Example:
    @example[
      ((φ _ 1) 0)
      (eval:error ((φ __x __x) 0))
    ]
  }

  @specsubform[variable-id]{

    An identifier whose name begins with a
    @racketlink[char-lower-case?]{lowercase} character.

    Matches anything, and binds the identifier to the matching value in the
    @var[body]s. If a variable binding is used multiple times within a
    pattern, the corresponding matches must be the same according to
    @racket[match-equality-test].

    Example:
    @example[
      ((φ x x) 1)
      ((φ (S x x) x) (S 2 2))
      (eval:error ((φ (S x x) x) (S 3 4)))
    ]
  }

  @specsubform[product-id]{

    Matches a @tech{constructor} named @var[product-id].

    Example:
    @example[
      ((φ S 1) S)
    ]
  }

  @defsubform*[
    #:kind " "
    #:link-target? #f
    #:id [product-id (var product-id)]
    [(product-id patt ...)
     (product-id patt ... . patt)]
  ]{

    Matches an @tech{instance} of the @tech{product} bound to @var[product-id]
    with @tech{fields} that match @var[patt]s.

    Example:
    @example[
      ((φ (S x . xs) (list x xs)) (S 1 2 3))
    ]

  }

  @specsubform[reference-id]{

    A @rtech{bound} identifier that is not a @var[wildcard-id],
    @var[variable-id], or @var[constructor-id].

    Matches the bound value.

    Example:
    @example[
      ((φ + 'Plus) +)
      (eval:error ((φ + 'Plus) -))
    ]
  }

  @specsubform[(patt #:if cond-expr)]{

    Matches @var[patt] if @var[cond-expr] produces a true value.
    @var[cond-expr] is in the scope of all of the variables bound in
    @var[patt].

    Example:
    @example[
      ((φ (n #:if (> n 0)) '+++) 5)
      (eval:error ((φ (n #:if (> n 0)) '+++) -3))
    ]
  }

  @specsubform[(patt #:as alias-patt)]{

    Matches @var[patt] if @var[alias-patt] also matches the same value.

    Example:
    @example[
      ((φ ((S x) #:as y) (list x y)) (S 1))
    ]
  }

  @defsubform*[
    #:kind " "
    #:link-target? #f
    #:id [regexp (var regexp)]
    [regexp
     (regexp patt ...+)
     (regexp patt ... . patt)]
  ]{

    Matches @var[regexp] (a @rtech{regexp value} or byte-@rtech{regexp-value})
    to a portion of its argument (a string, byte string, path, or input port)
    with @racket[regexp-match].

    Example:
    @example[
      (values
       ((φ #rx"x+y+" 1) "--xxyy++")
       ((φ #rx"a+b+" 2) (open-input-string "--aabb++")))
    ]

    If any @var[patt]s are given, they are matched against the results. If one
    or more capturing groups is present, the initial ``whole-match'' element
    of the result list is dropped before attempting to match @var[patt]s.

    Example:
    @example[
      (values
       ((φ (#rx"x+y+" xy) xy) "--xxyy++")
       ((φ (#rx"(a+)(b+)" as bs) (list as bs)) "--aabb++"))
      (eval:error ((φ (#rx"(a+)(b+)" as bs cs) 'OK) "--aabb++"))
    ]
  }

  @specsubform[symbol]{

    An unquoted datum literal that is not a @var[wildcard-id],
    @var[variable-id], or @var[product-id].

    Matches a symbol.

    Example:
    @example[
      ((φ $ 1) '$)
      (eval:error ((φ $ 1) $))
    ]
  }

  @defsubform*[
    #:kind " "
    #:link-target? #f
    #:id [patt (var patt)]
    [(patt ...)
     (patt ... . patt)]
  ]{

    Matches @var[patt]s against the elements of a list.

    Example:
    @example[
      ((φ (a b c) (+ a b c)) '(1 2 3))
    ]

    If the pattern contains a delimited @racketparenfont{.}, the final
    @var[patt] is matched against the argument's tail.

    Example:
    @example[
      ((φ (a . b) (list a b)) '(1))
    ]
  }

  @defsubform*[
    #:kind " "
    #:link-target? #f
    #:id [struct-id (var struct-id)]
    [(struct-id ([field patt] ...))
     (struct-id patt ..)]
  ]{

    Matches an instance of a structure type named @var[struct-id], where each
    field in the instance matches the corresponding @var[patt].

    Example:
    @example[
      (struct F (a b c))
      ((φ (F x y z) (+ x y z)) (F 1 2 3))
    ]

    If @var[field]s are present, any field of @var[struct-id] may be omitted,
    and such fields can occur in any order.

    Example:
    @example[
      (struct tree (val left right))
      ((φ (tree [val a]
                [left (tree [right #f] [val b] [left #f])]
                [right #f])
         (list a b))
       (tree 0 (tree 1 #f #f) #f))
      ((φ (tree a (tree b #f #f) #f) (list a b))
       (tree 0 (tree 1 #f #f) #f))
    ]
  }

  @specsubform[
    #:literals (quasiquote)
    (quasiquote fqp)
  ]{

    Introduces a @deftech{quasiquoted pattern}, wherein all identifiers match
    symbols and @racket[unquote] escapes back to normal patterns.

    Example:
    @example[
      ((φ `(x y . ,(S a b)) (+ a b))
       (list* 'x 'y (S 1 2)))
    ]
  }

  @specsubform[
    #:literals (void)
    (void)
  ]{

    Matches a @seclink["void" #:doc '(lib
    "scribblings/reference/reference.scrbl")]{void} value.

  }

  @specsubform[#,(racketparenfont "#(" (var patt) " ...)")]{

    Matches @var[patt]s against the elements of a @rtech{vector}.

    Example:
    @example[
      ((φ #(a b c) (+ a b c)) (vector 1 2 3))
    ]
  }

  @specsubform[#,(racketparenfont "#&" (var patt))]{

    Matches @var[patt] against the element of a @rtech{box}.

    Example:
    @example[
      ((φ #&x x) (box 1))
    ]
  }

  @specsubform[#,(racketparenfont "#hash([" (var key) " . " (var patt) "] ...)")]{

    Matches against a @rtech{hash table}'s key-value pairs, where @var[key] is
    a bare identifier or a @var[literal]. Any key-value pair of the hash table
    may be omitted, and such pairs can occur in any order.

    Example:
    @example[
      ((φ #hash([x . a] ["y" . b]) (list a b))
       (hash "y" 1 #t 2 'x 3))
    ]
  }
}

@defform[(function [patt maybe-if ... body ...+] ...+)]{

  Creates a @tech{function} of one argument with at least one clause. When
  multiple clauses are given, they are attempted in the order specified.

  Example:
  @example[
    (define fib
      (function [(n #:if (< n 2)) 1]
                [n (+ (fib (- n 1)) (fib (- n 2)))]))
    (map fib '(0 1 2 3 4 5 6))
  ]

}

@deftogether[(
@defform[(φ* formals maybe-if ... body ...+)]
@defform/subs[
  (phi* formals maybe-if ... body ...+)
  [(formals (patt ...)
            (patt ...+ . rest-patt)
            rest-patt)]
])]{

  Creates a @tech{function} of any number of arguments with one clause. The
  @var[formals] determine the number of arguments.

  A @var[formals] has one of the following forms:

  @specsubform[(patt ...)]{

    The function accepts as many argument values as the number of @var[patt]s.
    Each @var[patt] is associated with an argument value by position.

    Example:
    @example[
      (define fact
        (function* [(n) (fact n 1)]
                   [(0 a) a]
                   [(n a) (fact (- n 1) (* a n))]))
      (map fact '(0 1 2 3 4 5 6))
    ]
  }

  @specsubform[(patt ...+ . rest-patt)]{

    The function accepts at least as many arguments as the number of
    @var[patt]s. When the function is applied, the @var[patt]s are associated
    with argument values by position, and all leftover arguments are placed
    into a list that is associated to @var[rest-id].

    Example:
    @example[
      ((function* [(x y . zs) (list x y zs)]) 1 2 3 4)
    ]
  }

  @specsubform[rest-patt]{

    The function accepts any number of arguments and places them into a list
    that is associated with @var[rest-patt].

    Example:
    @example[
      ((function* [xs (reverse xs)]) 1 2 3 4)
    ]
  }
}

@defform[(function* [formals maybe-if ... body ...+] ...+)]{

  Creates a @tech{function} of any number of arguments with one or more
  clauses. When multiple clauses are given, they are attempted in the order
  specified.

}

@defproc[(function? [v any/c]) boolean?]{

  Returns @racket[#t] if @var[v] is a @tech{function}.

}

@; -----------------------------------------------------------------------------

@subsection[#:tag "ref:macros"]{Macros}

@deftogether[(
@defform[(μ macro-patt body ...+)]
@defform/subs[
  #:literals (void quasiquote unquote)
  (mu macro-patt directive ... body ...+)
  [(macro-patt literal
               wildcard-id
               variable-id
               id-literal
               (struct-id macro-patt ...)
               (quasiquote mqp)
               (macro-patt ...)
               (macro-patt ...+ . macro-patt)
               (macro-patt ooo . macro-patt))
   (mqp (unquote macro-patt)
        (mqp . mqp)
        datum)
   (ooo ...
        ...+)
   (directive (code:line #:with macro-patt stx-expr)
              (code:line #:if condition-expr))]
])]{

  Creates a @tech{macro} of one argument with one clause.

  A @var[macro-patt] is a @var[literal] or @var[wildcard-id] as defined for
  @racket[φ], or one of the following forms:

  @specsubform[variable-id]{

    An identifier whose name begins with a
    @racketlink[char-lower-case?]{lowercase} character.

    Matches anything, and binds the pattern variable to the matching sub-term
    in the @var[body]s. If the identifier is of the form
    @var[id:syntax-class-id], it behaves like an @tech[#:doc '(lib
    "syntax/scribblings/syntax.scrbl")]{annotated pattern variable} with the
    implicit class inserted. Otherwise, the pattern variable matches anything.

    Example:
    @example[
      (define-syntax m (μ x:id (identifier? #'x)))
      (m a)
      (eval:error (m 3))
    ]
  }

  @specsubform[id-literal]{
    
    An identifier that is not a @var[wildcard-id] or @var[variable-id].

    Matches an identifier literal.

    Example:
    @example[
      (define-syntax m (μ ++ "plus plus"))
      (m ++)
      (eval:error (m --))
    ]
  }

  @specsubform[(struct-id macro-patt ...)]{

    Matches a sequence of terms, where the first element @var[struct-id] names
    a structure type and subsequent elements match the corresponding
    @var[macro-patt].

    Example:
    @example[
      (struct F (a b c))
      (define-syntax m (μ (F x y z) (+ x y z)))
      (m (F 1 2 3))
    ]
  }

  @specsubform[
    #:literals (quasiquote)
    (quasiquote mqp)
  ]{

    Introduces a @deftech{quasiquoted macro pattern}, in which identifiers
    match symbols and @racket[unquote] escapes back to normal macro patterns.

    Example:
    @example[
      (define-syntax m (μ `x 1))
      (m x)
      (eval:error (m y))
    ]
  }

  @specsubform[(macro-patt ...)]{

    Matches a parenthesized sequence of @var[macro-patt]s.

    Example:
    @example[
      (define-syntax m (μ (a b) (b a)))
      (values (m (0 S)) (instance? (m (0 S))))
    ]
  }

  @specsubform[(macro-patt ...+ . macro-patt)]{

    Matches a term with a list head and a tail separated by a delimited
    @racketparenfont{.}.

    Example:
    @example[
      (define-syntax m (μ (x y . z) (list x y z)))
      (m (1 2 . 3))
    ]
  }

  @specsubform[(head-macro-patt ooo . tail-macro-pat)]{

    Matches any term that can be decomposed into a list head matching some
    number of repetitions of @var[head-macro-patt] followed by a list tail
    matching @var[tail-macro-patt].

    Example:
    @example[
      (define-syntax m
        (macro* [(x ... (y ...) z ...+)
                 (list* x ... y ... z ...)]))
      (values
       (m 1 2 (3 4) 5 6)
       (m (3 4) 5 6))
      (eval:error (m 1 2 (3 4)))
    ]
  }

  The following pattern directives may appear in a macro clause:

  @specsubform[(code:line #:with macro-patt expr)]{

    Evaluates @var[expr] in the context of all pattern bindings and matches it
    against @var[macro-patt]. The @var[expr] is implicitly
    @racket[quasisyntax]ed, so @var[unsyntax] and @var[unsyntax-splicing]
    escape to an expression within the transformer environment.

    Example:
    @example[#:escape UNSYNTAX
      (define-syntax m
        (macro [x #:with (a) #,(list 10)
                  #:with b 1
                  (+ x a b)]))
      (m 100)
    ]
  }

  @specsubform[(code:line #:if condition-expr)]{

    Evaluates the @var[condition-expr] in the context of all previous
    attribute bindings. If the value is @racket[#f], the match fails.

    Example:
    @example[#:escape UNSYNTAX
      (define-syntax m-fib
        (macro [n:nat #:if (< (var n) 2) 1]
               [n:nat (+ (m-fib #,(- (var n) 1))
                         (m-fib #,(- (var n) 2)))]))
      (values
       (m-fib 0) (m-fib 1) (m-fib 2)
       (m-fib 3) (m-fib 4) (m-fib 5) (m-fib 6))
      (eval:error (let ([a 7]) (m-fib a)))
    ]
  }
}

@defform[(macro [macro-patt directive ... body ...+] ...+)]{

  Creates a @tech{macro} of one argument with one or more clauses. When
  multiple clauses are given, they are attempted in the order specified.

}

@deftogether[(
@defform[(μ* (macro-patt ...) directive ... body ...+)]
@defform[(mu* (macro-patt ...) directive ... body ...+)]
)]{

  Creates a @tech{macro} with any number of arguments and one clause.

}

@defform[(macro* [(macro-patt ...) directive ... body ...+] ...+)]{

  Creates a @tech{macro} with any number of arguments and at least one clause.
  When multiple clauses are given, they are attempted in the order specified.

}

@defform[(var id)]{

  Returns the value bound to @var[id] in the transformer environment.

}

@section{Experimental Features}

@; =============================================================================

@bibliography[
  @; @bib-entry[
  @;   #:key "Krishnamurthi2001"
  @;   #:title "Linguistic Reuse"
  @;   #:author "Krishnamurthi, Sriram"
  @;   #:location "PhD dissertation, Rice University"
  @;   #:date "2001"
  @; ]

  @bib-entry[
    #:key "Taha2004"
    #:title "A gentle introduction to multi-stage programming"
    #:author "Taha, Walid"
    #:location @list{In @emph{Domain-Specific Program Generation} (pp 30-50). Springer, Berlin, Heidelberg, 2004}
  ]
]
