#lang scribble/manual

@title[#:tag "ref:mac"]{Macros}

@require{./algebraic-includes.rkt}

@require[
  @for-label[
    algebraic/racket/base
    racket/contract/base
    syntax/transformer
  ]
]

@; #############################################################################

@defmodule[algebraic/macro]

The bindings documented in this section are provided by the
@racketmodname[algebraic/macro] and @racketmodname[algebraic/racket/base]
libraries.

A @deftech{macro} is a syntax @rtech{transformer} that either deconstructs or
rejects an argument or argument list at @rtech{expansion} time. Macros are
created with the single-argument @racket[μ] (or @racket[mu]) and
@racket[macro] forms, or the multi-argument variants @racket[μ*] (or
@racket[mu*]) and @racket[macro*].

The @racket[μ] (@racket[mu]) form creates a @tech{macro} of exactly one
argument with exactly one clause.

@example[
  (define-syntax infix (μ (a op b) (op a b)))
  (infix (1 - (infix ((infix (2 + (infix (3 / 4)))) * (infix (5 - 6))))))
]

The @racket[μ*] (@racket[mu*]) form creates a @tech{macro} of any number of
arguments with exactly one clause.

@example[
  (define-syntax infi* (μ* (a op b) (op a b)))
  (infi* 1 - (infi* (infi* 2 + (infi* 3 / 4)) * (infi* 5 - 6)))
]

The @racket[macro] form creates a @tech{macro} of exactly one argument with
one or more clauses.

@example[
  (define-syntax infixr
    (macro
      [(a op b) (op (infixr a) (infixr b))]
      [a a]))
  (infixr (1 - ((2 + (3 / 4)) * (5 - 6))))
]

The @racket[macro*] form creates a @tech{macro} of any number of arguments
with one or more clauses.

@example[#:escape UNSYNTAX
  (define-syntax infixr*
    (macro*
      [(a op b ...) (op (infixr* a) (infixr* b ...))]
      [((a ...)) (infixr* a ...)]
      [(a) a]))
  (infixr* 1 - (2 + 3 / 4) * (5 - 6))
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

With @racket[quote] and @racket[local-expand], we can expose the generated
code.

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

@; -----------------------------------------------------------------------------

@deftogether[(
@defform[(μ0 expr ...+)]
@defform[(mu0 expr ...+)]
)]{

  Creates an identifier @tech{macro} that behaves like a
  @racket[make-variable-like-transformer]. If multiple @var[expr]s are given,
  they are implicitly wrapped with @racket[begin].

  Examples:
  @example[
    (define-syntax juicy-cons (μ0 (println 'POW!) ::))
    (begin-for-syntax
      (println (syntax-local-value #'juicy-cons)))
    (juicy-cons 1 (juicy-cons 2 3))
  ]
}

@deftogether[(
@defform[(μ parse-option ... mac-patt mac-directive ... body ...+)]
@defform[(mu parse-option ... mac-patt mac-directive ... body ...+)]
@defform/subs[
  #:literals (void quasiquote unquote)
  (macro parse-option [mac-patt mac-directive ... body ...+] ...+)
  [(mac-patt literal-data
             (quasiquote #,(var qmp))
             wildcard-id
             variable-id
             #,(racketparenfont "#(" (var mac-patt) " ...)")
             #,(racketparenfont "#&" (var mac-patt))
             (struct-id mac-patt ...)
             (mac-patt #:as alias-mac-patt)
             (mac-patt #:if condition-expr)
             (mac-patt ooo . mac-patt)
             (mac-patt ...)
             (mac-patt ...+ . mac-patt))
   (qmp (unquote mac-patt)
        (qmp . qmp)
        datum)
   (ooo ...
        ...+)
   (mac-directive (code:line #:do [defn-or-expr ...])
                  (code:line #:as alias-mac-patt)
                  (code:line #:if condition-expr)
                  (code:line #:with consequent-mac-patt premise-expr))]
])]{

  Creates a @tech{macro} of one argument with one clause. When multiple
  clauses are given, they are attempted in the order specified.

  Any @var[parse-option]s are passed to @racket[syntax-parse] unaltered. See
  @racket[syntax-parse] for details.

  A @var[mac-patt] is a @var[literal-data] or @var[wildcard-id] as defined for
  @racket[φ], or one of the following forms:

  @specsubform[
    #:literals (quasiquote)
    (quasiquote #,(var qmp))
  ]{

    Introduces a @deftech{quasiquoted macro pattern}, in which identifiers
    match symbols and @racket[unquote] escapes back to normal macro patterns.

    Example:
    @example[
      (define-syntax m (μ `(x ,y) y))
      (m (x #t))
      (eval:error (m (z #t)))
    ]
  }

  @specsubform[variable-id]{

    An identifier whose name begins with a
    @racketlink[char-lower-case?]{lowercase} character.

    Matches anything, and binds the pattern variable to the matching sub-term
    in the @var[body]s. If the identifier is of the form
    @var[id:syntax-class-id], it is an @tech[#:doc '(lib
    "syntax/scribblings/syntax.scrbl")]{annotated pattern variable} and only
    matches forms described by the @stech{syntax class} bound to
    @var[syntax-class-id]. Otherwise, it matches anything.

    Example:
    @example[
      (define-syntax m (μ x:id (identifier? #'x)))
      (m a)
      (eval:error (m 3))
    ]
  }

  @specsubform[#,(racketparenfont "#(" (var mac-patt) " ...)")]{

    Matches @var[mac-patt]s against the elements of a @rtech{vector}.

    Example:
    @example[
      (let-syntax ([m (μ #(a b c) (+ a b c))])
        (m #(1 2 3)))
    ]
  }

  @specsubform[#,(racketparenfont "#&" (var mac-patt))]{

    Matches @var[mac-patt] against the contents of a @rtech{box}.

    Example:
    @example[
      (let-syntax ([m (μ #&x x)])
        (m #&1))
    ]
  }

  @specsubform[(struct-id mac-patt ...)]{

    Matches a sequence of terms, where the first element @var[struct-id] names
    a structure type and subsequent elements match the corresponding
    @var[mac-patt].

    Example:
    @example[
      (struct F (a b c))
      (define-syntax m (μ (F x y z) (+ x y z)))
      (m (F 1 2 3))
    ]
  }

  @specsubform[(mac-patt #:as alias-mac-patt)]{

    First matches @var[mac-patt], then matches @var[alias-mac-patt] against
    the same argument. If either pattern fails, the match fails.

    Example:
    @example[
      (let-syntax ([calc (μ ((x + y) #:as z) `(z = ,(+ x y)))])
        (calc (1 + 2)))
    ]
  }

  @specsubform[(mac-patt #:if condition-expr)]{

    First matches @var[mac-patt], then evaluates the @var[condition-expr] in
    the context of all previous variable bindings. If the value is
    @racket[#f], the match fails.

    Example:
    @example[
      (define-syntax m (μ (x #:if (number? (var x))) (+ x 2)))
      (m 1)
      (eval:error (m #f))
    ]
  }

  @specsubform[(head-mac-patt ooo . tail-mac-pat)]{

    Matches any term that can be decomposed into a list head matching some
    number of repetitions of @var[head-mac-patt] followed by a list tail
    matching @var[tail-mac-patt].

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

  @specsubform[(mac-patt ...)]{

    Matches a parenthesized sequence of @var[mac-patt]s.

    Example:
    @example[
      (data Ss (S))
      (define-syntax swap (μ (a b) (b a)))
      (swap (0 S))
      (product-instance? (swap (0 S)))
    ]
  }

  @specsubform[(mac-patt ...+ . mac-patt)]{

    Matches a term with a list head and a tail separated by a delimited
    @racketparenfont{.}.

    Example:
    @example[
      (define-syntax m (μ (x y . z) (list x y z)))
      (m (1 2 . 3))
    ]
  }

  The following pattern directives may appear any number of times in a macro
  clause:

  @specsubform[(code:line #:do [defn-or-expr ...])]{

    Evaluates a sequence of definitions and expressions in the scope of all
    previous variable bindings.

    Example:
    @example[
      (define-syntax plus
        (μ* args #:do [(define xs (syntax-e #'args))]
          (+ #,@xs)))
      (plus 1 2 3)
    ]
  }

  @specsubform[(code:line #:as alias-mac-patt)]{

    Matches the original argument list against @var[alias-mac-patt].

    Example:
    @example[
      (let-syntax ([calc (μ* (x + y) #:as z `(,@'z = ,(+ x y)))])
        (calc 1 + 2))
    ]
  }

  @specsubform[(code:line #:if condition-expr)]{

    Evaluates the @var[condition-expr] in the context of all previous variable
    bindings. If the value is @racket[#f], the match fails.

    Example:
    @example[#:escape UNSYNTAX
      (define-syntax m-fib
        (macro [n:nat #:if (< (var n) 2) 1]
               [n:nat (+ (m-fib #,(- (var n) 1))
                         (m-fib #,(- (var n) 2)))]))
      (eval:error (let ([a 7]) (m-fib a)))
      (eval-syntax #`(list #,@(for/list ([n 7]) #`(m-fib #,n))))
    ]
  }

  @specsubform[(code:line #:with consequent-mac-patt premise-expr)]{

    Evaluates @var[premise-expr] in the context of all pattern bindings and
    matches the result against @var[consequent-mac-patt]. The
    @var[premise-expr] is implicitly @racket[quasisyntax]ed, so @var[unsyntax]
    and @var[unsyntax-splicing] escape to an expression within the transformer
    environment.

    Example:
    @example[#:escape UNSYNTAX
      (let-syntax ([m (macro
                        [x #:with (a) (list #'10)
                           #:with b #'1
                           (+ x a b)])])
        (m 100))
    ]
  }
}

@deftogether[(
@defform[(μ* parse-option ... mac-formals mac-directive ... body ...+)]
@defform[(mu* parse-option ... mac-formals mac-directive ... body ...+)]
@defform/subs[
  (macro* parse-option ... [mac-formals mac-directive ... body ...+] ...+)
  [(mac-formals (mac-patt ...)
                (mac-patt ...+ . rest-mac-patt)
                rest-mac-patt)]
])]{

  Creates a @tech{macro} with any number of arguments. The @var[mac-formals]
  determine the number of arguments in the same way as for @racket[φ*], except
  with @var[mac-patt]s instead of @var[patt]s. When multiple clauses are
  given, they are attempted in the order specified.

}

@defform[(var id)]{

  Returns the value bound to @var[id] in the transformer environment.

}
