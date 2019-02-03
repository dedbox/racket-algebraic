#lang scribble/manual

@title{Algebraic structures}
@author{@author+email["Eric Griffis" "dedbox@gmail.com"]}

@(require
  racket/sandbox
  scribble/examples
  (for-label algebraic/racket/base
             algebraic/racket/macro
             racket/contract/base))

@(define (rtech . args)
   (apply tech #:doc '(lib "scribblings/reference/reference.scrbl") args))

@(define algebraic-eval
   (call-with-trusted-sandbox-configuration
    (λ ()
      (parameterize
          ([sandbox-output 'string]
           [sandbox-error-output 'string])
        (make-base-eval #:lang 'algebraic/racket/base
          '(require (for-syntax algebraic/racket/macro)
                    racket/function))))))

@(define-syntax-rule (example expr ...)
   @examples[#:eval algebraic-eval #:label #f expr ...])

@; ------------------------------------------------------------------------------

This package extends the @racketmodname[racket/base] language with first-class
algebraic data constructors, functions, and macros with pattern-based
destructuring.

@defmodulelang[algebraic/racket/base]

@section{Overview}

@subsection{Constructors}

A @deftech{constructor} is a first-class value that identifies a data type.
Constructors can be matched against, collected into lists, and applied to
arguments. Constructors are created with the @racket[data] form.

@example[
  (data S)
  (values S (constructor? S))
]

An @deftech{instance} is created when a constructor is applied to an argument
list.

@example[
  (values (S 0) (instance? (S 0)))
]

Each constructor and instance is distinct from all the others. More precisely,
a pair of constructors or a pair of instances are @racket[equal?] iff they are
@racket[eq?].

@example[
  (let* ([x1 S] [x2 x1] [x3 (S)] [x4 (S)])
    (values
     (list (eq? x1 x2) (equal? x1 x2))
     (list (eq? x1 x3) (equal? x1 x3))
     (list (eq? x3 x4) (equal? x3 x4))))
]

The arguments of an instance can be accessed with the destructuring patterns
of an algebraic function or macro.

@subsection{Functions}

A @deftech{function} is a procedure that deconstructs or rejects a
fully-evaluated argument or argument list. Functions are created with the
@racket[phi] (or @racket[φ]), @racket[function], and @racket[function*] forms.

The @racket[phi] (or @racket[φ]) form creates a function of exactly one
argument with exactly one clause.

@example[
  (define inc (φ a (S a)))
  (define dec (φ (S b) b))
  (values (function? inc) (inc 0) (dec (S 0)))
]

The @racket[function] form creates a function of exactly one argument with at
least one clause.

@example[
  (define num (function [0 0] [n (S (num (- n 1)))]))
  (num 3)
]

The @racket[function*] form creates a function of any number of arguments
with at least one clause.

@example[
  (define add (function* [(a 0) a] [(a (S b)) (S (add a b))]))
  (add (num 3) (num 2))
]

Functions created by @racket[function*] can have clauses with no arguments,
and the number of arguments for each clause can vary.

@example[
  (define num-args (function* [() 0] [(_) 1] [(_ _) 2]))
  (values (num-args) (num-args 9) (num-args 8 7))
]

@subsection{Macros}

A @deftech{macro} is a syntax transformer that deconstructs or rejects an
argument list at expansion time. Macros are created with the @racket[mu] (or
@racket[μ]), @racket[macro], and @racket[macro*] forms.

The @racket[mu] (or @racket[μ]) form creates a macro of exactly one argument
with exactly one clause.

@example[
  (define-syntax infix (μ (a op b) (op a b)))
  (infix (5 - 3))
]

The @racket[macro] form creates a macro of exactly one argument with at least
one clause.

@example[
  (define-syntax bin (macro [#f 0] [_ 1]))
  (values (bin #f) (bin (values #f)))
]

The @racket[macro*] form creates a macro of any number of arguments with at
least one clause.

@example[
  (define-syntax and2 (macro* [(a b) ((function [#f #f] [_ b]) a)]))
  (define-syntax or2 (macro* [(a b) ((function [#f b] [x x]) a)]))
  (values
   (list (and2 #t #t) (and2 #t #f) (and2 #f #t) (and2 #f #f))
   (list (or2 #t #t) (or2 #t #f) (or2 #f #t) (or2 #f #f)))
]

@; (for*/list ([p (in-list '(#t #f))]
@;             [q (in-list '(#t #f))])
@;   (land #t #t))

Macros are designed to simplify mundane meta-programming tasks. Consider the
following run-time implementation of the ``power'' function from
@cite{Taha2004}:

@example[
  (define f-power
    (function*
      [(0 _) 1]
      [(n x) (* x (f-power (- n 1) x))]))
  (map (curry f-power 3) '(0 1 2 3 4 5 6))
]

We can use @racket[unsyntax] and the @racket[var] form to unroll instances of
the power function at expansion time.

@example[#:escape UNSYNTAX
  (define-syntax m-power
    (macro*
      [(0 _) 1]
      [(1 x) x]
      [(n:nat x) (* x (m-power #,(- (var n) 1) x))]))
  (define-syntax m-power3 (μ y (m-power 3 y)))
  (map (φ x (m-power3 x)) '(0 1 2 3 4 5 6))
]

With @racket[macro-expand], we can peek at the code produced by the macro.

@example[#:escape UNSYNTAX
  (define-syntax q-power
    (macro*
      [(0 _) 1]
      [(1 x) x]
      [(n:nat x) '#,(macro-expand #`(* x (q-power #,(- (var n) 1) x)))]))
  (q-power 3 2)
]

   @; [(n:nat x) #'#,(macro-expand #`(* x (q-power #,(- (var n) 1) x)))]))

@; ------------------------------------------------------------------------------

@section{API Reference}

@subsection[#:tag "ref-constructors"]{Constructors}

@defform[(data id ...+)]{

  Binds each @var[id] to a fresh @tech{constructor}.

}

@defproc[(constructor? [v any/c]) boolean?]{

  Returns @racket[#t] if @var[v] is a @tech{constructor}.

}

@defproc[(instance? [v any/c]) boolean?]{

  Returns @racket[#t] if @var[v] is an @tech{instance}.

}

@subsection[#:tag "ref-functions"]{Functions}

@deftogether[(
@defform[(phi patt body ...+)]
@defform/subs[
  (φ patt body ...+)
  [(patt literal
            wildcard
            variable
            reference
            con-id
            (con-id patt ...)
            (patt #:if cond-expr))
   (literal boolean
            character
            number
            string
            bytes)]
])]{

  Creates a @tech{function} of one argument with one clause.

  A @var[patt] has one of the following forms:

  @specsubform[literal]{

    A Racket literal value: @racket[#t], @racket[#f], @var[character],
    @var[number], @var[string], or @var[bytes].

    Matches an @racket[equal?] constant.

    Example:
    @example[
      ((φ "one" 1) "one")
      (eval:error ((φ "one" 1) "two"))
    ]

  }

  @specsubform[wildcard]{

    An identifier whose name begins with an underscore ``@racketid[_]''.

    Matches anything, without binding any identifiers.

    Example:
    @example[
      ((φ _ 1) 0)
      (eval:error ((φ __x __x) 0))
    ]

  }

  @specsubform[variable]{

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

  @specsubform[reference]{

    A @rtech{bound} identifier that is not a @var[wildcard], @var[variable],
    or @var[con-id].

    Matches the bound value.

    Example:
    @example[
      ((φ + 'Plus) +)
      (eval:error ((φ + 'Plus) -))
    ]

  }

  @specsubform[con-id]{

    Matches a @tech{constructor} named @var[con-id].

    Example:
    @example[
      ((φ S 1) S)
    ]

  }

  @specsubform[(con-id patt ...)]{

    Matches an @tech{instance} of the @tech{constructor} named @var[con-id],
    where each argument in the instance matches the corresponding @var[patt].

    Example:
    @example[
      ((φ (S x) x) (S 0))
    ]

  }

  @specsubform[(patt #:if cond-expr)]{

    A @deftech{conditional} pattern.

    Matches @var[patt] if @var[cond-expr] produces a true value.
    @var[cond-expr] is in the scope of all of the variables bound in
    @var[patt].

    Example:
    @example[
      ((φ (n #:if (> n 0)) '+++) 5)
      (eval:error ((φ (n #:if (> n 0)) '+++) -3))
    ]

  }

}

@defform[(function [patt body ...+] ...+)]{

  Creates a @tech{function} of one argument with at least one clause. When
  multiple clauses are given, they are attempted in the order specified.

  Example:
  @example[
    (define fib
      (function
        [(n #:if (< n 2)) 1]
        [n (+ (fib (- n 1)) (fib (- n 2)))]))
    (map fib '(0 1 2 3 4 5 6))
  ]

}

@defform[(function* [(patt ...) body ...+] ...+)]{

  Creates a @tech{function} of any number of arguments with at least one
  clause. When multiple clauses are given, they are attempted in the order
  specified.

  Example:
  @example[
    (define fact
      (function*
        [(n) (fact n 1)]
        [(0 a) a]
        [(n a) (fact (- n 1) (* a n))]))
    (map fact '(0 1 2 3 4 5 6))
  ]

}

@defproc[(function? [v any/c]) boolean?]{

  Returns @racket[#t] if @var[v] is a @tech{function}.

}

@subsection[#:tag "ref-macros"]{Macros}

@defmodule[algebraic/racket/macro]

The bindings documented in this section are provided by the
@racketmodname[algebraic/racket/macro] library, not
@racketmodname[algebraic/racket/base].

@deftogether[(
@defform[(mu macro-patt body ...+)]
@defform/subs[
  (μ macro-patt directive ... body ...+)
  [(macro-patt literal
               wildcard
               variable
               id-literal
               (macro-patt ...))
   (directive (code:line #:with macro-patt stx-expr)
              (code:line #:when condition-expr))]
])]{

  Creates a @tech{macro} of one argument with one clause.

  A @var[macro-patt] is a @var[literal] or @var[wildcard] as defined for
  @racket[φ], or one of the following forms:

  @specsubform[variable]{

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
    
    An identifier that is not a @var[wildcard] or @var[variable].

    Matches an identifier literal.

    Example:
    @example[
      (define-syntax m (μ ++ "plus plus"))
      (m ++)
      (eval:error (m --))
    ]

  }

  @specsubform[(macro-patt ...)]{

    Matches a sequence of @var[macro-patt]s.

    Example:
    @example[
      (define-syntax m (μ (a b) (b a)))
      (values (m (0 S)) (instance? (m (0 S))))
    ]

  }

  The following pattern directives may appear in a macro clause:

  @specsubform[(code:line #:with macro-patt expr)]{

    Evaluates @var[expr] in the context of all pattern bindings and matches it
    against the pattern. The @var[expr] is implicitly @racket[quasisyntax]ed,
    so @var[unsyntax] and @var[unsyntax-splicing] escape to an expression
    within the transformer environment.

    Example:
    @example[#:escape UNSYNTAX
      (define-syntax m
        (macro [x #:with '(a) '#,(list 10)
                  #:with b 1
                  (+ x a b)]))
      (m 100)
    ]

  }

  @specsubform[(code:line #:when condition-expr)]{

    Evaluates the @var[condition-expr] in the context of all previous
    attribute bindings. If the value is @racket[#f], the matching process
    backtracks.

    Example:
    @example[#:escape UNSYNTAX
      (define-syntax m-fib
        (macro [n:nat #:when (< (var n) 2) 1]
               [n:nat (+ (m-fib #,(- (var n) 1))
                         (m-fib #,(- (var n) 2)))]))
      (values
       (m-fib 0) (m-fib 1) (m-fib 2)
       (m-fib 3) (m-fib 4) (m-fib 5) (m-fib 6))
      (eval:error (let ([a 6]) (m-fib a)))
    ]

  }

}

@defform[(macro [macro-patt body ...+] ...+)]{

  Creates a @tech{macro} with one argument and at least one clause. When
  multiple clauses are given, they are attempted in the order specified.

}

@defform[(macro* [(macro-patt ...) body ...+] ...+)]{

  Creates a @tech{macro} with any number of arguments and at least one clause.
  When multiple clauses are given, they are attempted in the order specified.

}

@defform[(var id)]{

  Returns the value bound to @var[id] in the transformer environment.

}

@defproc[(macro-expand [stx syntax?]) syntax?]{

  Expands @var[stx] in the lexical context of the expression currently being
  expanded.

}

@; ------------------------------------------------------------------------------

@bibliography[
  @bib-entry[
    #:key "Taha2004"
    #:title "A gentle introduction to multi-stage programming"
    #:author "Taha, Walid"
    #:location @list{In @emph{Domain-Specific Program Generation} (pp 30-50). Springer, Berlin, Heidelberg, 2004}
  ]
]
