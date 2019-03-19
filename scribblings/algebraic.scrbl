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
             syntax/parse))

@(define (rtech . args)
   (apply tech #:doc '(lib "scribblings/reference/reference.scrbl") args))

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

@(define-simple-macro (algebraic-code str ...)
   #:with stx (datum->syntax this-syntax 1)
   @typeset-code[#:context #'stx "#lang algebraic/racket/base\n\n" str ...])

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
   @typeset-code[#:keep-lang-line? #f "#lang algebraic/model/core\n" str ...])

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

@; @(define (~cite . args) (list ~ (superscript (apply cite args))))

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

@(define ~ emph)

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

@subsubsub*section{Consistent syntax}

The destructuring syntax for algebraic @racket[data] and most other data is
the same across all @tech{function}- and @tech{macro}-producing forms.

@subsubsub*section{Convenient defaults}

Algebraic data @tech{constructors} are like type tags. When applied to an
argument list, they produce an @tech{instance}---a @deftech{list}, or ordered
sequence, of unnamed fields with the @tech{constructor} at its head. They are
easy to print and easy to parse, like @rtech{prefab} structs. The main
difference is that algebraic @tech{constructors} are lexically scoped and have
a natural ordering.

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

@section[#:tag "tut"]{Tutorial: Interpreters}

A core use case for @algebraic-mod is fast and cheap interpreter development.
With the right tooling, throw-away interpreters can be an easy way to explore
and validate language design choices quickly.

As it happens, Haskell is already pretty good at this. Algebraic data types
and functional destructuring syntax make interpreters easy to read and write,
and its type system keeps track of pervasive breaking changes for you.
@algebraic-mod addresses the untyped half of this equation---algebraic data
@emph{structures} (ADTs sans type constraints) with destructuring syntax.

In this tutorial, we will build a tower of interpreters based on the models
originally used to design @algebraic-mod itself:

@itemlist[
  #:style 'ordered

  @item{A @emph{core} model based on the untyped λ-calculus,}

  @item{An @emph{extended} syntax that compiles down to core constructs, and}

  @item{A @emph{hosted} variant that borrows additional constructs from the
    implementing platform.}

]

@; -----------------------------------------------------------------------------

@subsection*{The Core Model}

@defmodulelang[algebraic/model/core]

The core model started as a pure untyped λ-calculus, and it took a while to
get everything right. Each time a construct was added or removed, everything
else had to be re-tested. Without automation, exploration at this level of
detail was easier on paper. Whereas this tutorial begins with a presumably
viable model and proceeds directly to a robust solution, a realistic process
will converge over many iterations of design, development, and testing until
all three are in agreement.

@subsubsection*{Syntax}

The core defines three syntactic categories: terms, values, and patterns.

A @emph{term} is a concrete expression of the calculus.

A @emph{value} is a term that reduces to a normal form according to the
evaluation semantics. When a term eventually reduces to a particular value, we
say the term @emph{evaluates to} that value.

Non-value terms either diverge or get stuck. Some divergent terms are useful
as infinite loops, but all stuck terms indicate an error.

A @emph{pattern} is a predicate form that produces a set of variable bindings
when matched against a compatible term.

@centered[
  @grammar["t"
    (list @list{@~{t} @~{t}} "application")
    (list @list{@~{t};@~{t}} "sequence")
    (list @list{φ@~{p}.@~{t}} "function clause")
    (list @list{μ@~{p}.@~{t}} "macro clause")
    (list @~{x} "variable")
    (list @~{δ} "constructor")
    (list "◊" "unit")
  ] "\n" "\n"

  @grammar["p"
    (list @list{@~{t} @~{t}} "application")
    (list @list{@~{t};@~{t}} "sequence")
    (list "_" "wildcard")
    (list @~{x} "variable")
    (list @~{δ} "constructor")
    (list "◊" "unit")
  ] "\n" "\n"

  @grammar["v"
    (list @list{φ@~{p}.@~{t};·} "function")
    (list @list{μ@~{p}.@~{t};·} "macro")
    (list @~{δ} "constructor")
    (list @list{@~{δ} (@~{v};·)} "instance")
    (list "◊" "unit")
  ]
]

There are seven kinds of terms and six kinds of patterns.

An @emph{application} (@~{t} @~{t}) is a pair of juxtaposed sub-terms. Nested
applications are always parenthesized.

@core-code{
  (A ◊)
}

A @emph{sequence} (@~{t};@~{t}) is a pair of sub-terms separated by a
semicolon in the model and prefixed by a dollar sign (@racketid[$]) in code.
Sequences combine multiple terms into a single unit. They are used for holding
the non-tag elements of a tagged @tech{list}.

@core-code{
  (A ($ ◊ ◊))
}

A @emph{function clause} (φ@~{p}.@~{t}) is a λ-abstraction with the formal
parameter generalized to a pattern, and a @emph{macro clause} (μ@~{p}.@~{t})
is a generalized λ-abstraction.

@core-code{
  (φ x x)
  (μ a a)
}

A @emph{uniform sequence} (@~{t};·) is a right-hand nesting of sequences in
which every left-hand side is the same kind of term and every right-hand side,
exept perhaps the last, is the same kind of uniform sequence. Uniform
sequences combine multiple abstractions into a single unit.

A @emph{function} (φ@~{p}.@~{t};·) is a function clause or a uniform sequence
of them, and a @emph{macro}@elem['nbsp](μ@~{p}.@~{t};·) is one or more macro
clauses in sequence.

@core-code{
  ($ (φ x x) ($ (φ y y) (φ z z)))
  ($ (μ a a) ($ (μ b b) (μ c c)))
}

A @emph{variable} (@~{x}) is a name that may be bound to a term in the body of
a function clause or macro clause. We say a variable not bound by an
abstraction is @emph{free}. All free variables are stuck.

A @emph{constructor} (@~{δ}) is a name that identifies a data type.
Constructors evaluate to themselves.

For convenience, constructors names begin with an uppercase letter and
variable names begin with a lowercase letter.

An @emph{instance} (@~{δ} (@~{v};·)) is a constructor applied to a value or
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

The parser's job is to map s-expressions onto the members of @racketid[Term].
Some terms have a pattern, and patterns are parsed by different rules than
terms. Although constructor arities are not encoded in the @racket[data]
declaration, we can anticipate the following:

@itemlist[

  @item{Applications, sequences, and the abstractions take two arguments.}

  @item{Variables and constructors take one argument.}

  @item{Wildcards and units take no arguments.}

]

We can also assume applications and sequences are structurally recursive. With
all of this in mind, the parser looks like this:

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

The @racketid[parse] function is defined as a pair of s-expression parser
combinators: @racketid[term] and @racketid[patt]. The @racketid[term]
combinator translates terms in the formal syntax onto @racketid[Term], while
@racketid[patt] maps patterns in the formal syntax onto @racket[Patt]. Parsing
always starts and ends with a single term.

In the patterns of these functions, some symbols are quoted and others are
not. The Greek symbols are quoted because @racketid[φ] and @racketid[μ] are
lowercase characters and we don't want them to behave like variables.
@racketid[_] is quoted because an unquoted underscore is a wildcard pattern
for the enclosing @racket[function] form. Quoting is optional for @racketid[$]
and @racketid[◊] because neither is an uppercase or lowercase character.

For symbols other than @racketid[_] and @racketid[◊], we need a way to detect
constructor and variable names. Inspecting the characters of a symbol's name
is too low level for s-expression parsing, so we'll put that logic into helper
predicates. The @racketid[con-name?] predicate checks if the first character
of a symbol's name is uppercase, and @racketid[var-name?] checks if it is
lowercase.

But what about the abstraction parsers? Structural recursion is an option, but
it turns out we can save some trouble in the interpreter if we do a little
extra work here in the parser.

The following term is a standard illustration of @emph{unintended variable
capture}.

@core-code{
  ((φ x (φ y (x y))) y)
}

Substituting @racketid[y] for @racket[x] naively would produce a function
clause wherein the free outer @racketid[y] is indistinguishable from the bound
inner @racketid[y]:

@core-code{
  (φ y (y y))
}

Unintended variable capture is a well-known problem with multiple solutions.
Our goal is to produce correct and compact code, so we'll take a
straight-forward approach and make all bound variable names unique as they are
parsed. This is equivalent to rewriting the original s-expression so every
bound variable gets a unique index:

@core-code{
  ((φ x1 (φ y2 (x1 y2))) y)
}

Reducing this term does not lead to the unintended capture of @racketid[y].

@core-code{
  (φ y2 (y y2))
}

We'll name this rewriting function @racketid[α-rename-clause]. It takes two
arguments---a @racketid[Patt] and a @racketid[Term]---and returns two values:
a copy of the @racketid[Patt] with all of its variables renamed, and a copy of
the @racketid[Term] with its variables renamed as they were in the
@racketid[Patt]. The definition of @racketid[α-rename-clause] will be given
later; for now, we'll just use it to finish the parser.

The abstraction parsers also need to pass multiple return values to a
constructor. To keep our parsing rules compact, we'll define our first
@tech{macro}. The @racketid[values->] @tech{macro} is a simple syntactic patch
for @racket[call-with-values] that merely rearranges the arguments and reduces
the number of parentheses, admitting a more applicative style.

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

  (define (con-name? x)
    (and (symbol? x) (char-lower-case? (first-char x))))

  (define (var-name? x)
    (and (symbol? x) (char-upper-case? (first-char x))))

  (define (first-char x)
    (string-ref (symbol->string s) 0))
}

@subsubsub*section{The Printer}

The printer's job is to translate the members of @racketid[Term] back into
s-expressions. The printer has the same structure as the parser, but the roles
of the pattern and body are swapped.

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

The @racketid[show] function is also defined as a pair of
@racketid[term]/@racketid[patt] combinators. This time, each @racket[function]
pattern deconstructs a member of @racketid[Term] or @racketid[Patt] and
produces a term or pattern in the formal syntax as an s-expression. A helper
named @racketid[α-restore] is introduced pre-emptively to undo whatever
@racketid[α-reduce-clause] does to variable names.

@subsubsub*section{Values}

A @emph{value} is a term that evaluates to itself trivially. Values are the
subset of terms that constitute a valid computational result. To recognize
them, we'll need a series of predicates on the members of @racketid[Term].

The over-arching goal is to produce a @racketid[value?] predicate that checks
whether a @racketid[Term] is a value. Values include:

@itemlist[

  @item{functions,}

  @item{macros,}

  @item{constructors,}

  @item{instances of a constructor, and}

  @item{the unit value.}

]

We'll also need to recognize three kinds of uniform sequence: functions,
macros, and instance data. Since these predicate all share the same internal
structure, we'll create our second @racket[macro],
@racketid[define-uniform-seq-pred]. It takes two arguments---an identifier and
a predicate---and binds the identifier to a new predicate that recognizes
uniform sequences of terms that satisfy the given predicate.

@algebraic-code{
  (define-syntax define-uniform-seq-pred
    (μ* (name? kind?)
      (define name?
        (function
          [(TSeq t1 t2) #:if (kind? t1) (name? t2)]
          [t (kind? t)]))))

  (define-uniform-seq-pred fun? TFun?)
  (define-uniform-seq-pred mac? TMac?)
  (define-uniform-seq-pred dat? value?)

  (define ins? (function [(TApp (TCon _) t) (dat? t)] [_ #f]))

  (define value? (or/c fun? mac? TCon? ins? TUni?))
}

The @racketid[fun?], @racketid[mac?], and @racketid[dat?] predicates recognize
a uniform sequence of function clauses, macro clauses, and instance data,
respectively; and the @racketid[ins?] predicate recognizes an instance as a
constructor applied to instance data.

@; -----------------------------------------------------------------------------

@subsubsection*{Evaluation Semantics}

The top-level evaluator is a
@racketid[parse]-@racketid[interpret]-@racketid[show] @tech{macro} named
@racketid[algebraic]. It uses a function named @racketid[interpret] to
evaluate a term to its normal form, which in turn calls a @racketid[step]
@tech{function} that returns either a term representing the next step of the
computation, or @racket[#f] if the term is stuck.

@algebraic-code{
  (define-syntax algebraic
    (μ t (show (interpret (parse 't)))))

  (define interpret
    (function
      [v #:if (value? v) v]
      [t
       ;; (writeln (show t))
       (interpret
        (or (step t)
            (error 'interpret "stuck at ~v" (show t))))]))
}

@tabular[
  #:style full-width
  #:cell-properties '((center))
  (list
   (list
    @inferrule[
          @${t_1 ↝ t_1'}
      ----------------------- @list{A@smaller{PP}1}
      @${t_1 t_2 ↝ t_1' t_2}
    ]

    @inferrule[
                 @${p_{11}×t_2=σ}
                @${σ(t_{12})=t_{12}'}
      ---------------------------------------------- @list{A@smaller{PP}M}
      @list{(μ@${p_{11}.t_{12}})@${ t_2 ↝ t_{12}'}}
    ]
   ))
]

@tabular[
  #:style full-width
  #:cell-properties '((center))
  (list
   (list
    @inferrule[
      @list{@${v_1}≁μ@${p}.@${t};·}
            @${t_2 ↝ t_2'}
      ------------------------------ @list{A@smaller{PP}2}
         @${v_1 t_2 ↝ v_1 t_2'}
    ]

    @inferrule[
                 @${p_{11}×v_2=σ}
                @${σ(t_{12})=t_{12}'}
      ---------------------------------------------- @list{A@smaller{PP}F}
      @list{(φ@${p_{11}.t_{12}})@${ v_2 ↝ t_{12}'}}
    ]
   ))
]

Applications (@~{t} @~{t}) reduce quasi-eagerly, starting on the left. If the
left side reduces to a macro, the macro is applied to the un-reduced right
side. If the left side reduces to a function or constructor, evaluation
continues on the right.

Function clauses (φ@~{p}.@~{t}) attempt to match a single argument value to a
pattern @~{p}. If the match succeeds, any variables bound by the pattern are
substituted into the body term @~{t}. If the match fails, the application is
stuck.

@core-example[
  ((φ x x) (φ y y))
]

A @emph{macro clause} (μ@~{p}.@~{t}) attempts to match a single argument term
to a pattern. The only semantic difference between function clauses and macro
clauses is that macro clauses do not reduce their argument before attempthing
the match.

@core-example[
  ((μ (fx fy) (fy fx)) ((φ x x) (φ y y)))
]

@tabular[
  #:style full-width
  #:cell-properties '((center))
  (list
   (list
    @inferrule[
          @${t_1 ↝ t_1'}
      ----------------------- @list{S@smaller{EQ}1}
      @${t_1;t_2 ↝ t_1';t_2}
    ]

    @inferrule[
          @${t_2 ↝ t_2'}
      ----------------------- @list{S@smaller{EQ}2}
      @${v_1;t_2 ↝ v_1;t_2'}
    ]
   ))
]

Sequences always reduce eagerly from left to right. The semicolon is special
to Racket, so we'll prefix sequenced pairs with a dollar sign instead.

@core-example[
  ($ ((φ x x) (φ y y)) (φ z z))
]

@tabular[
  #:style full-width
  #:cell-properties '((center))
  (list
   (list
    @inferrule[
       @list{(φ@${p_{11}}.@${t_{12}}) @${v_2 ↝ t_{12}'}}
      ---------------------------------------------------- @list{F@smaller{UN}1}
      @list{(φ@${p_{11}}.@${t_{12}};@${v_{13}}) @${v_2 ↝ t_{12}'}}
    ]

    @inferrule[
       @${p_{11}×v_2 undefined}
      ---------------------------------------------------- @list{F@smaller{UN}2}
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
      ---------------------------------------------------- @list{M@smaller{AC}1}
      @list{(φ@${p_{11}}.@${t_{12}};@${v_{13}}) @${t_2 ↝ t_{12}'}}
    ]

    @inferrule[
       @${p_{11}×t_2 undefined}
      ---------------------------------------------------- @list{M@smaller{AC}2}
      @list{(φ@${p_{11}}.@${t_{12}};@${v_{13}}) @${t_2 ↝ v_{13} v_2}}
    ]
   ))
]

Functions and macros attempt to match their inputs to each clause in the
sequence, taking the body of the first succesful match as its result. If every
clause fails, the term is stuck.

@core-example[
  (($ (φ ◊ ◊) (φ x (φ z x))) (φ y y))
  (($ (φ ◊ ◊) (φ x (φ z x))) ◊)
]

Applying a constructor to a sequence produces an instance of the constructor,
and the existing inferences rules already cover this case.

@core-example[
  (A ($ B ($ (φ x x) (φ y y))))
]

@; -----------------------------------------------------------------------------

@subsubsection*{Pattern Matching Semantics}

@; -----------------------------------------------------------------------------

@subsubsection*{Pragmatics}

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
  @bib-entry[
    #:key "Taha2004"
    #:title "A gentle introduction to multi-stage programming"
    #:author "Taha, Walid"
    #:location @list{In @emph{Domain-Specific Program Generation} (pp 30-50). Springer, Berlin, Heidelberg, 2004}
  ]
]
