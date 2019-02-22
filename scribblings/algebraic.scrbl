#lang scribble/manual

@title{Algebraic structures for untyped Racket}
@author{@author+email["Eric Griffis" "dedbox@gmail.com"]}

@(require
  racket/sandbox
  scribble/core
  scribble/examples
  scribble/html-properties
  (for-label algebraic/racket/base
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

@; ---

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
@seclink["sec:data"]{algebraic data structures} and complementary
@seclink["sec:functions"]{function} and @seclink["sec:macros"]{macro}
abstractions with a uniform and compact destructuring syntax.

@; =============================================================================

@section{Overview}

@hash-lang[algebraic/racket/base] synthesizes and specializes the
functionality of @racket[struct], @racket[match], and @racket[syntax-parse] to
streamline the funtional programming experience in two key areas:

@subsubsub*section{Consistent syntax}

The destructuring syntax for algebraic @racket[data] and most other data is
the same across all @tech{function}- and @tech{macro}-producing forms.

@subsubsub*section{Convenient defaults}

Algebraic data @tech{constructors} are like type tags. When applied to an
argument list, they produce an @tech{instance}---an ordered sequence of
unnamed fields with the @tech{constructor} at its head. They are easy to print
and easy to parse, like @rtech{prefab} structs. The main difference is that
algebraic @tech{constructors} are lexically scoped and have a natural
ordering.

@; -----------------------------------------------------------------------------

@subsection*[#:tag "sec:data"]{Data}

The @racket[data] form defines a variety of functions and syntax
@rtech{transformers} for working with named @tech{products} and @tech{sums}.

A @deftech{product} identifies a family of structures comprising an ordered
set of @deftech{fields}, and a @deftech{sum} identifies an ordered set of
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
  (values (Succ Zero) (instance? (Succ Zero)))
]

Equality is decided structurally for @tech{constructors} and their
@tech{instances}.

@example[
  (values (equal? (Succ Zero) (Succ Zero))
          (equal? (Succ Zero) (Succ (Succ Zero))))
]

The @racket[data] form also produces membership predicates for each name it
defines.

@example[
  (values (Succ? Succ)
          ((sum Peano?) Succ)
          ((sum Peano?) (sum Peano)))
]

To prevent name clashes in types like @racket[Unit], @tech{sum} bindings are
defined in their own @rtech{namespace}. The @racket[sum] form merely adds the
appropriate @rtech{scope} to an identifier.

@; -----------------------------------------------------------------------------

@subsection*[#:tag "sec:functions"]{Functions}

A @deftech{function} is a procedure that deconstructs or rejects a
fully-evaluated argument or argument list. Functions are created with the
single-argument @racket[φ] (or @racket[phi]) and @racket[function] forms, or
the multi-argument variants @racket[φ*] (or @racket[phi*]) and
@racket[function*].

The @racket[φ] (@racket[phi]) form creates a function of exactly one argument
with exactly one clause.

@example[
  (define inc (φ a (Succ a)))
  (define dec (φ (Succ b) b))
  (values (function? inc) (inc Zero) (dec (Succ (Succ Zero))))
]

The @racket[φ*] (@racket[phi*]) form creates a function of any number of
arguments with exactly one clause.

@example[
  (define cmp
    (φ* (a b)
      ((cond [(number? a) <] [(char? a) char<?]) a b)))
  (values (cmp 1 2) (cmp #\y #\x))
]

The @racket[function] form creates a function of exactly one argument with one
or more clauses.

@example[
  (define num
    (function [0 Zero]
              [n (Succ (num (- n 1)))]))
  (num 3)
]

The @racket[function*] form creates a function of any number of arguments with
one or more clauses.

@example[
  (define add
    (function* [(a Zero) a]
               [(a (Succ b)) (Succ (add a b))]))
  (add (num 3) (num 2))
]

Functions created by @racket[function*] can have clauses with no arguments,
and the number of arguments for each clause can vary.

@example[
  (define num-args
    (function* [() 0]
               [(_ . rest) (+ 1 (apply num-args rest))]))
  (values (num-args) (num-args -) (num-args - -))
]

@; -----------------------------------------------------------------------------

@subsection*[#:tag "sec:macros"]{Macros}

A @deftech{macro} is a syntax @rtech{transformer} that deconstructs or rejects
an argument or argument list at expansion time. Macros are created with the
single-argument @racket[μ] (or @racket[mu]) and @racket[macro] forms, or the
multi-argument variants @racket[μ*] (or @racket[mu*]) and @racket[macro*].

The @racket[μ] (@racket[mu]) form creates a macro of exactly one argument with
exactly one clause.

@example[
  (define-syntax infix (μ (a op b) (op a b)))
  (infix (5 - 3))
]

The @racket[μ*] (@racket[mu*]) form creates a macro of any number of arguments
with exactly one clause.

@example[
  (define-syntax implies (μ* (p q) (or (not p) q)))
  (for*/list ([p '(#t #f)] [q '(#t #f)]) (implies p q))
]

The @racket[macro] form creates a macro of exactly one argument with one or
more clauses.

@example[
  (define-syntax bin (macro [#f 0] [_ 1]))
  (values (bin #f) (bin (values #f)))
]

The @racket[macro*] form creates a macro of any number of arguments with one
or more clauses.

@example[
  (define-syntax and2 (macro* [(a b) ((function [#f #f] [_ b]) a)]))
  (define-syntax or2 (macro* [(a b) ((function [#f b] [x x]) a)]))
  (values
   (for*/list ([a '(#t #f)] [b '(#t #f)]) (and2 a b))
   (for*/list ([a '(#t #f)] [b '(#t #f)]) (or2 a b)))
]

Macros are designed to simplify mundane meta-programming tasks. The following
example is a run-time implementation of the ``power'' function from
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

  @item{A @emph{core} model based on the untyped lambda calculus,}

  @item{An @emph{extended} syntax that compiles down to core constructs, and}

  @item{A @emph{hosted} variant that borrows additional constructs from the
    implementing platform.}

]

@; -----------------------------------------------------------------------------

@subsection*{The Core Model}

@defmodulelang[algebraic/model/core]

The core is a pure untyped lambda calculus extended with macros, tagged
tuples, and destructuring patterns. The result is compact and easy to
implement as a small-step s-expression interpreter.

Before diving into the details, some background might be useful.

The core model started as a pure untyped λ-calculus. It took a while to get
everything right. Each time something was added or removed, everything else
had to be re-checked. Without automation, exploration at this level of detail
may be easier off-screen. Thus we begin with a presumably viable model on
paper and, with the benefit of hindsight, proceed directly to a robust
solution. Realistically, a design will undergo several adjustments and
tangential explorations over multiple iterations before it solidifies.

@subsubsection*{Syntax}

The core defines three syntactic categories: terms, values, and patterns.

A @emph{term} is a concrete expression of the calculus.

A @emph{value} is a term that reduces to a normal form according to the
operational semantics. When a term eventually reduces to a particular value,
we say the term @emph{evaluates} to that value. Non-value terms either diverge
or get stuck. Some divergent terms are useful for e.g. looping, but all stuck
terms indicate an undesirable error.

A @emph{pattern} is a predicate form that maps to variable bindings when
matched against a compatible term.

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
    (list @list{@~{δ} (@~{v};·)} "constructor")
    (list "◊" "unit")
  ]
]

An @emph{application} (@~{t} @~{t}) is a pair of juxtaposed sub-terms. To keep
the implementation as simple as possible, nested applications are always
parenthesized.

A @emph{sequence} (@~{t};@~{t}) is a pair of sub-terms separated by a
semicolon. Sequences combine multiple abstractions into a single unit. They
are used for holding the non-tag components of a tagged tuple.

A @emph{uniform sequence} (@~{t};·) is a sequence in which every left-hand
side has the same type and every right-hand side (exept perhaps the last) has
the same shape. Uniform sequences combine multiple abstractions into a single
unit.

A @emph{function clause} (φ@~{p}.@~{t}) is a λ-abstraction with the formal
parameter generalized to a pattern. More generally, a @emph{function}
(φ@~{p}.@~{t};·) is a function clause or a uniform sequence of function
clauses. Similarly, a @emph{macro clause} (μ@~{p}.@~{t}) is a generalized
λ-abstraction and a @emph{macro} (μ@~{p}.@~{t};·) is one or more macro clauses
in sequence.

A @emph{variable} (@~{x}) is a name that may be bound to a term within the
body of a function or macro.

A @emph{constructor} (@~{δ}) is a name that identifies a data type.
Constructors evaluate to themselves.

For convenience, a ``TitleCase'' name denotes a constructor and a
``lowercase'' name denotes a variable.

An @emph{instance} (@~{δ} (@~{v};·)) is a constructor applied to a uniform
sequence of values.

The @emph{unit} (◊) value denotes the presence of a value, as a convenient
notation for when the actual value is irrelevant.

@; -----------------------------------------------------------------------------

@subsubsection*{Evaluation Semantics}

Applications (@~{t} @~{t}) reduce quasi-eagerly, starting on the left. If the
left side reduces to a macro, the macro is applied to the un-reduced right
side. If the left side reduces to a function or constructor, evaluation
continues on the right.

Function clauses (φ@~{p}.@~{t}) attempt to match a single input value to a
pattern @~{p}. If the match succeeds, any variables bound by the pattern are
substituted into the body term @~{t}. If the match fails, the application is
stuck.

@core-example[
  ((φ x x) (φ y y))
]

A @emph{macro clause} (μ@~{p}.@~{t}) attempts to match a single input
@emph{term} to a pattern. The only semantic difference between function
clauses and macro clauses is that macro clauses do not reduce their argument
before attempthing the match.

@core-example[
  ((μ (fx fy) (fy fx)) ((φ x x) (φ y y)))
]

Sequences always reduce eagerly from left to right. The semicolon is special
to Racket, so we'll prefix sequenced pairs with a dollar sign instead.

@core-example[
  ($ ((φ x x) (φ y y)) (φ z z))
]

Functions and macros attempt to match their inputs to each clause in the
sequence, taking the body of the first succesful match as its result. If every
clause fails, the term is stuck.

@core-example[
  (($ (φ ◊ ◊) (φ x (φ z x))) (φ y y))
  (($ (φ ◊ ◊) (φ x (φ z x))) ◊)
]

Applying a constructor to a sequence produces an instance of the constructor.

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

  Creates a new @tech{sum} on an ordered set of @tech{products} and binds
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
