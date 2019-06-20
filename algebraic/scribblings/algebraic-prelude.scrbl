#lang scribble/manual

@title[#:tag "prelude"]{The Prelude}

@require{./algebraic-includes.rkt}

@require[
  @for-label[
    algebraic/racket/base
    racket/contract/base
    racket/function
  ]
]

@define[prelude-eval (algebraic-evaluator)]
@define-syntax[example (algebraic-example prelude-eval)]

@example[#:hidden (require algebraic/prelude)]

@; #############################################################################

@defmodule[algebraic/prelude]

The bindings documented in this section are provided by
@racketmodname[algebraic/prelude] and @racketmodname[algebraic/racket/base].
These functions and function aliases add consistency to the functional Racket
programming experience and encourage compact code. They are used extensively
throughout the collection.

This module re-provides the @racket[const] function.

@defform[(values-> f expr)]{

  Applies @var[f] to the values produced by @var[expr].

  Example:
  @example[
    (values-> + (values 1 2 3))
  ]
}

@defproc[(thunk<- [x any/c] ...) (-> any)]{

  Returns a thunk that produces the values determined by the @var[x]s.

  Example:
  @example[
    ((thunk<- (+ 1 2) (+ 3 4)))
  ]
}

@defproc[(flip [f procedure?]) procedure?]{

  Returns a variadic function that behaves like @var[f] with its first two
  arguments swapped.

  Example:
  @example[
    ((flip -) 10 2 1)
  ]
}

@defproc[(twice [f procedure?]) procedure?]{

  Returns a function that composes @var[f] with itself, equivalent to
  @racket[(.. #,( var f) #,(var f))].

  Example:
  @example[
    ((twice add1) 1)
  ]
}

@defproc[(>> [f procedure?] [x any/c] ...) procedure?]{

  Returns a left-curried version of @var[f].

  The @racket[>>] function does not self-curry. Use @racket[>>*] to 

  Example:
  @example[
    ((>> list 1 2) (list 3 4) 5 6)
  ]
}

@defproc[(<< [f procedure?] [x any/c] ...) procedure?]{

  Returns a right-curried version of @var[f].

  The @racket[<<] function, unlike @racket[curryr], is not self-curried.

  Example:
  @example[
    ((<< list 1 2) (list 3 4) 5 6)
  ]
}

@defproc[(>>* [f procedure?]) (-> any/c ... procedure?)]{

  Explicit self-@racket[curry].

  Equivalent to @racket[(curry f)].

  Example:
  @example[
    (((>>* list) 1 2) 3 4)
  ]
}

@defproc[(<<* [f procedure?]) (-> any/c ... procedure?)]{

  Explicit self-@racket[curryr]. Equivalent to @racket[(curryr f)].

  Example:
  @example[
    (((<<* list) 1 2) 3 4)
  ]
}

@deftogether[(
@defthing[id  procedure? #:value values]
@defthing[::  procedure? #:value cons]
@defthing[::* procedure? #:value list*]
@defthing[++  procedure? #:value append]
@defthing[..  procedure? #:value compose]
@defthing[&&  procedure? #:value conjoin]
@defthing[||  procedure? #:value disjoin]
@defthing[$   procedure? #:value apply]
)]{

  Short names for common functions.

}