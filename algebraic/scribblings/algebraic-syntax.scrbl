#lang scribble/manual

@title[#:tag "stx"]{The Syntax Monad}

@require{./algebraic-includes.rkt}

@require[
  @for-label[
    (except-in algebraic/racket/base do)
    algebraic/syntax
    algebraic/syntax/list
    racket/contract/base
  ]
]

@define[syntax-eval (algebraic-evaluator)]
@define-syntax[example (algebraic-example/locations syntax-eval)]

@example[#:hidden (require algebraic/syntax
                           algebraic/syntax/list)]

@; #############################################################################

@defmodule[algebraic/syntax]

This module implements a monad and a @racket[do]-notation along with a variety
of other categorical design patterns for building and composing
context-preserving syntax transformations embedded in @tech{syntax lists}.

A syntax transformation is @deftech{context-preserving} when it transfers the
lexical context and source location from its argument to its result. The class
of syntax lists together with all context-preserving syntax transformations
between them forms a category.

@defform/subs[
  #:literals (<-)
  (do block ...+)
  [(block (code:line formals <- expr)
          expr)]
]{

  Example:
  @example[
    (do xs <- #'(1 2)
        (y z) <- #'(3 4)
        (w v . us) <- #'(5 6 7 8 9)
        ($ return (++ xs (list y z) (list* w v us))))
  ]
}

@defproc[
  (bind [f (-> any/c ... syntax-list?)] [xs syntax-list?]) syntax-list?
]{

  Unwraps @var[xs] with @racket[syntax-e] and applies @var[f] to the contents.

  Example:
  @example[
    (define xs #'(1 2 3))
    (bind return xs)
    (bind (.. (>> $ return) reverse list) xs)
  ]
}

@defproc[(return [v any/c] ...) syntax-list?]{

  Wraps the @var[v]s in a @tech{syntax list} with the lexical context and
  source location from the leftmost syntax object in the @var[v]s, if one
  exists.

  Example:
  @example[
    (define x1 (return 1))
    (define x2 (return #'2 #'3))
    (values x1 x2)
  ]

  @example[
    (car (syntax-e x1))
    (car (syntax-e x2))
    (cadr (syntax-e x2))
  ]
}

@defproc[(join [xs syntax-list?]) any]{

  Unwraps @var[xs] and each of its elements, then returns the results as
  separate values.

  Example:
  @example[
    (join #'(1 2 3))
  ]
}

@defproc[(pure [v any/c] ...) syntax-list?]{

  Wraps the @var[v]s in a @tech{syntax list} with no lexical context or source
  location.

  Example:
  @example[
    (define x1 (pure 1))
    (define x2 (pure #'2 #'3))
    (values x1 x2)
  ]

  @example[
    (car (syntax-e x1))
    (car (syntax-e x2))
    (cadr (syntax-e x2))
  ]
}

@defproc[(ap [f syntax-list?] [xs syntax-list?] ...) syntax-list?]{

  Maps the function embedded in @var[f] along the elements of the @var[xs]s,
  taking an argument from each.

  @racket[(ap (pure #,(var f)) #,(var xs) ...)] is roughly equivalent to
  @racket[(fmap #,(var f) #,(var xs) ...)], except @racket[ap] allows @var[f]
  to carry an arbitrary lexical context and source location.

  Example:
  @example[
    (values (ap (pure ::) #'(1) #'(2))
            (ap (->syntax #'here (list ::)) #'(1) #'(2)))
  ]

  @example[
    (ap (pure ::) #'(1 2 3) #'(4 5 6))
  ]
}

@defproc[(fmap [f procedure?] [xs syntax-list?] ...) syntax-list?]{

  Maps @var[f] along the elements of the @var[xs]s, taking an argument from
  each.

  Example:
  @example[
    (fmap :: #'(1 2 3) #'(4 5 6))
  ]

  @example[
    (fmap (.. add1 syntax-e) #'(1 2 3))
  ]
}

@defproc[(mappend [xs syntax-list?] ...) syntax-list?]{

  Concatenates the @var[xs]s into a single @tech{syntax list}.

  Example:
  @example[
    (mappend #'(1 2) #'(3 4) #'(5 6))
  ]
}

@defthing[mempty syntax-list? #:value (pure)]{

  The empty syntax list.

}

@defproc[(->syntax [ctx (or/c syntax? #f)] [v any/c]) syntax?]{

  Returns @racket[(datum->syntax #,(var ctx) #,(var v) #,(var ctx))].

  Example:
  @example[
    (->syntax #f 1)
  ]

  @example[
    (->syntax #'here 1)
  ]
}
