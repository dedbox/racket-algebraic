#lang scribble/manual

@title[#:tag "class"]{Run-time Polymorphism}

@require{./algebraic-includes.rkt}

@require[
  @for-label[
    algebraic/class
    (except-in algebraic/racket/base instance instance? struct:instance)
    racket/contract/base
  ]
]

@define[class-eval (algebraic-evaluator)]
@define-syntax[example (algebraic-example/locations class-eval)]

@example[#:hidden (require algebraic/class)]

@; #############################################################################

@defmodule[algebraic/class]

This module provides a simple dynamic dispatch mechanism based on
@rtech{syntax parameters}.

Algebraic Racket's classes are a lot like Haskell's type classes, but without
the typing constraints. They allow abstract patterns like monads with
do-notation to be defined in a generic way that makes sense for Racket.

The @racket[class] form defines a @rtech{syntax parameter} for each member of
the class.

@example[
  (class Eq
    [==]
    [/= (.. not ==)])
]

Invoking an undefined member function is a syntax error.

@example[
  (eval:error ==)
  (eval:error (/= 1 1))
]

The @racket[instance] form overrides the members of a class, and
@racket[with-instance] instantiates them at run time.

@example[
  (define-syntax StringEq (instance Eq [== string=?]))
  (with-instance StringEq
    (and (== "abcde" "abcde")
         (/= "hello" "world")))
]

The @racket[with-instance] form can also add a prefix to the names it binds.

@example[
  (with-instance [S: StringEq]
    (and (S:== "!!!" "!!!")
         (S:/= "abc" "123")))
]

To instantiate many classes at once, use the @racket[with-instances] form.

@example[
  (define-syntax IdentifierEq (instance Eq [== free-identifier=?]))
  (define-syntax EqEq (instance Eq [== eq?]))
  (with-instances (EqEq
                   [S: StringEq]
                   [I: IdentifierEq])
    (and (S:== "???" "???")
         (I:== #'!!! #'!!!)
         (== + +)))
]

@; -----------------------------------------------------------------------------

@defform[
  (class class-id member-decl-or-def ...+)
  #:grammar [(member-decl-or-def [member-id]
                                 [member-id def-expr])]
]{

  Defines an abstract class on the @var[member-id]s, with default values
  determined by any @var[def-expr]s given, and binds it to @var[class-id].

}

@defform[(instance class-id [member-id def-expr] ...+)]{

  Declares a concrete instance of the class bound to @var[class-id] with the
  @var[member-id]s overridden by the @var[def-expr]s.

}

@deftogether[(
@defform[(with-instance instance-id/optional-prefix expr ...+)]
@defform[(with-instances (instance-id/optional-prefix ...) expr ...+)
         #:grammar [(instance-id/optional-prefix instance-id
                                                 [prefix-id instance-id])]]
)]{

  Evaluates the @var[exprs] with the members of the @var[class-id]s
  instantiated according to the instance declarations bound to the
  @var[instance-id]s.

  If @var[prefix-id]s are given, they are prepended to the names of members
  defined by the corresponding @var[instance-id]s.

}
