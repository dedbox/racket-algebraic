#lang scribble/manual

@title[#:tag "class:base" #:style 'quiet]{The Base Classes}

@require{./algebraic-includes.rkt}

@require[
  @for-label[
    algebraic/class/applicative
    algebraic/class/functor
    algebraic/class/monad
    algebraic/class/monoid
    algebraic/class/semigroup
    algebraic/data/box
    algebraic/data/list
    algebraic/data/maybe
    algebraic/data/truthy
    algebraic/data/values
    (except-in algebraic/racket/base do #%module-begin)
    racket/contract/base
    racket/format
  ]
]

@define[class-eval (algebraic-evaluator)]
@define-syntax[example (algebraic-example/locations class-eval)]

@example[#:hidden
  (require algebraic/class/applicative
           algebraic/class/functor
           algebraic/class/monad
           algebraic/data/box
           algebraic/data/list
           algebraic/data/maybe
           algebraic/data/truthy
           algebraic/data/values
           racket/format)
]

@; #############################################################################

@table-of-contents[]

@; -----------------------------------------------------------------------------

@section[#:tag "class:base:applicative"]{Applicative Functor}

@defmodule[algebraic/class/applicative]

A @racket[Functor] with application, providing operations to

@itemlist[

  @item{embed pure expressions (@racket[pure]), and}

  @item{sequence computations and combine their results (@racket[<*>] and
  @racket[liftA2]).}

]

@defidform[#:kind "class" Applicative]{

  A minimal complete definition must include implementations of pure and of
  either @racket[<*>] or @racket[liftA2]. If it defines both, then they must
  behave the same as their default definitions:

  @itemlist[

    @item{@racket[<*>] = @racket[(>> liftA2 id)]}

    @item{@racket[(liftA2 f x y)] = @racket[(<*> (<$> f x) y)]}

  ]

  Further, any definition must satisfy the following:

  @bold{@emph{identity}}

  @racket[(<*> (pure id) v)] = @racket[v]

  @bold{@emph{composition}}

  @racket[(<*> (<*> (<*> (pure ..) u) v) w)] = @racket[(<*> u (<*> v w))]

  @bold{@emph{homomorphism}}

  @racket[(<*> (pure f) (pure x))] = @racket[(pure (f x))]

  @bold{@emph{interchange}}

  @racket[(<*> u (pure y))] = @racket[(<*> (pure (>> $ y)) u)]

  The other members have the following default definitions, which may be
  overridden with equivalent specialized implementations:

  @itemlist[

    @item{@racket[(*> u v)] = @racket[(<*> (<$ id u) v)]}

    @item{@racket[(<* u v)] = @racket[(liftA2 const u v)]}

  ]

  As a consequence of these laws, the @racket[Functor] instance for @racket[f]
  will satisfy

  @itemlist[@item{@racket[(fmap f x)] = @racket[(<*> (pure f) x)]}]

  It may be useful to note that supposing, for all @racket[x] and @racket[y],

  @racket[(>> p (q x y))] = @racket[(.. (>> f x) (>> g y))]

  it follows from the above that

  @racket[(>> liftA2 p (liftA2 q u v))] = @racket[(.. (>> liftA2 f u) (>> liftA2 g v))]

  If @racket[f] is also a @racket[Monad], it should satisfy

  @itemlist[

    @item{@racket[pure] = @racket[return]}

    @item{@racket[<*>] = @racket[ap]}

    @item{@racket[*>] = @racket[>>M]}

  ]

  (which implies that @racket[pure] and @racket[<*>] satisfy the applicative
  functor laws).

}

@; .............................................................................

@subsection[#:tag "class:base:applicative:members"]{Members}

Minimal instance: @racket[pure], (@racket[<*>] or @racket[liftA2])

@defproc[(pure [a any/c]) Applicative]{

  Lift a value.

}

@defproc[(<*> [f Applicative] [a Applicative]) Applicative]{

  Sequential application.

}

@defproc[(liftA2 [f procedure?] [a Applicative] [b Applicative]) Applicative]{

  Lift a binary function to actions.

  Some functors support an implementation of @racket[liftA2] that is more
  efficient than the default one. In particular, if @racket[fmap] is an
  expensive operation, it is likely better to use @racket[liftA2] than to
  @racket[fmap] over the structure and then use @racket[<*>].

}

@defproc[(*> [a Applicative] [b Applicative]) Applicative]{

  Sequence actions, discarding the value of the first argument.

  This is essentially the same as @racket[(>> liftA2 (flip const))], but if
  the @racket[Functor] instance has an optimized @racket[<$], it may be better
  to use that instead.

  @; Before liftA2 became a method, this definition was strictly better, but now
  @; it depends on the functor. For a functor supporting a sharing-enhancing
  @; (<$), this definition may reduce allocation by preventing a1 from ever being
  @; fully realized. In an implementation with a boring (<$) but an optimizing
  @; liftA2, it would likely be better to define (*>) using liftA2.

}

@defproc[(<* [a Applicative] [b Applicative]) Applicative]{

  Sequence ations, discarding the value of the second argument.

}

@; .............................................................................

@subsection[#:tag "class:base:applicative:helpers"]{Helpers}

@defproc[(<**> [a Applicative] [f Applicative] [b Applicative]) Applicative]{

  A variant of @racket[<*>] with the first two arguments reversed.

}

@defproc[(<*>~ [f Applicative] [a Applicative]) Applicative]{

  A lazay variant of @racket[<*>].

  Eager @racket[Applicative] chains get noisy as they grow because Racket
  functions must be curried explicitly.

  Example:
  @example[
    (with-instance BoxApplicative
      (<*> (pure +) (pure 1))
      (<*> (<*> (pure (>>* +)) (pure 1)) (pure 2))
      (<*> (<*> (<*> (pure (>>* (>>* +))) (pure 1)) (pure 2)) (pure 3)))
  ]

  The function (in this case, @racket[+]) must be wrapped in one @racket[>>*]
  for each @racket[Applicative] argument or @racket[<*>] will fail.

  This variant wraps the function with @racket[>>*]. Unless lazy semantics are
  desired, make the final operator in the chain eager (e.g., @racket[<*>]) to
  evaluate the whole chain by force.

  Example:
  @example[
    (with-instance BoxApplicative
      (<*> (pure +) (pure 1))
      (<*> (<*>~ (pure +) (pure 1)) (pure 2))
      (<*> (<*>~ (<*>~ (pure +) (pure 1)) (pure 2)) (pure 3)))
  ]
}

@defproc[(<**>~ [a Applicative] [f Applicative] [b Applicative]) Applicative]{

  A lazay variant of @racket[<**>].

}

@; -----------------------------------------------------------------------------

@section[#:tag "class:base:functor"]{Functor}

@defmodule[algebraic/class/functor]

The @racket[Functor] class is used for types that can be mapped over.

@defidform[#:kind "class" Functor]{

  Instances of @racket[Functor] should satisfy the following laws:

  @itemlist[

    @item{@racket[(>> fmap id)]  =  @racket[id]}

    @item{@racket[(>> fmap (.. f g))]  =  @racket[(.. (>> fmap f) (>> fmap g))]}

  ]
}

@; .............................................................................

@subsection[#:tag "class:base:functor:members"]{Members}

Minimal instance: @racket[fmap]

@defproc[(fmap [f procedure?] [a Functor]) Functor]{

  Maps a function over a @racket[Functor].

}

@defproc[(<$ [a any/c] [b Functor]) any]{

  Replace all locations in the input with the same value. The default
  definition is @racket[(λ (a f-b) (fmap (const a) f-b))], but this may be
  overridden with a more efficient version.

}

@; .............................................................................

@subsection[#:tag "class:base:functor:helpers"]{Helpers}

@defproc[(<$> [f procedure?] [a Functor]) Functor]{

  A synonym for @racket[fmap].

  The name of this operator is an allusion to @racket[$]. Whereas @racket[$]
  is function application, @racket[<$>] is function application lifted over
  a @racket[Functor].

  @bold{Examples:}

  Convert from a Maybe Int to a Maybe String using ~a:

  @example[
    (with-instance MaybeFunctor (<$> ~a Nothing))
    (with-instance MaybeFunctor (<$> ~a (Just 3)))
  ]

@; Convert from an Either Int Int to an Either Int String using show:

@; > show <$> Left 17
@; Left 17
@; > show <$> Right 17
@; Right "17"

  Double each element of a list:

  @example[(with-instance ListFunctor (<$> (>> * 2) '(1 2 3)))]

@; Apply even to the second element of a pair:

@; > even <$> (2,2)
@; (2,True)

}

@defproc[(<$>~ [f procedure?] [a Functor]) Functor]{

  A lazy variant of @racket[<$>].

}

@; -----------------------------------------------------------------------------

@section[#:tag "class:base:monad"]{Monad}

@defmodule[algebraic/class/monad]

Basic operations on monads, and a generic do-notation.

@defidform[#:kind "class" Monad]{

  Instances of @racket[Monad] should satisfy the following laws:

  @itemlist[

    @item{@racket[(>>= (return a) k)] = @racket[(k a)]}

    @item{@racket[(>>= m return)] = @racket[m]}

    @item{@racket[(>>= m (φ x (>>= (k x) h)))] = @racket[(>>= (>>= m k) h)]}

  ]

  Monads and Applicatives should relate as follows:

  @itemlist[

    @item{@racket[pure] = @racket[return]}

    @item{@racket[<*>] = @racket[ap]}

  ]

  The above laws imply:

  @itemlist[

    @item{@racket[(fmap f xs)] = @racket[(>>= xs (.. return f))]}

    @item{@racket[>>] = @racket[*>]}

  ]

  and that @racket[pure] and @racket[<*>] satisfy the applicative functor laws.
}

@; .............................................................................

@subsection[#:tag "class:base:monad:members"]{Members}

Minimal instance: @racket[>>=]

@defproc[(>>= [m Monad] [k (-> any/c ... Monad)]) Monad]{

  Sequentially compose two actions, passing any value produced by the first as
  an argument to the second.

}

@defproc[(>>M [m Monad] [k (-> any/c ... Monad)]) Monad]{

  Sequentially compose two actions, discarding any value produced by the
  first, like sequencing operators (such as the semicolon) in imperative
  languages.

}

@defproc[(return [a any/c] ...) Monad]{

  Inject values into the monadic type.

}

@defproc[(fail [msg string?]) Monad]{

  Fail with a message. This operation is not part of the mathematical
  definition of a monad.
  @; , but it is invoked on pattern-match failure in a do
  @; expression.

}

@; .............................................................................

@subsection[#:tag "class:base:monad:helpers"]{Helpers}

@defproc[(join [m Monad]) Monad]{

  The conventional monad join operator. It is used to remove one level of
  monadic structure, projecting its bound argument into the outer level.

}

@defproc[(=<< [k (-> any/c ... Monad)] [m Monad]) Monad]{

  Same as @racket[>>=], but with the arguments interchanged.

}

@; .............................................................................

@subsection[#:tag "class:base:monad:do-notations"]{Do-Notations}

@deftogether[(
@defform[(do do-expr ... expr)]
@defform[#:literals (let let-values <-)
         (do~ do-expr ... expr)
         #:grammar [(do-expr (code:line formals <- monad-expr)
                             (code:line let id expr)
                             (code:line let-values formals expr)
                             monad-expr)]]
)]{

  Eager (@racket[do]) and lazy (@racket[do~]) notations for monadic
  composition.

  @racket[do] composes a seies of @var[do-expr]s with a final @var[expr] to
  determine its result.

  @racket[do~] is similar, except it expects each @var[do-expr] to be warpped
  in a thunk, and it produces a thunk.

  A @var[do-expr] has one of the following forms:

  @specsubform[#:literals (<-) (code:line formals <- monad-expr)]{

    Evaluates the @racket[monad-expr] and binds the @racket[Monad]'s contents
    according to @var[formals].

    Examples:
    @example[
      (with-instance ListMonad
        (do (x) <- '(1 2 3 4)
            (return (add1 x))))
    ]

    @; @example[
    @;   (with-instance TruthyMonad
    @;     ((do~ (x y . zs) <- (return 1 2 3 4)
    @;           (return x y zs))))
    @; ]
  }

  @specsubform[#:literals (let) (code:line let id expr)]{

    Evaluates the @racket[expr] and binds the result to @var[id].

    Example:
    @; @example[
    @;   (with-instance ListMonad
    @;     (do let x (+ 1 2)
    @;         (return x)))
    @; ]

    @example[
      (with-instance TruthyMonad
        ((do~ let `(a . ,b) '(a 1 2 3)
              (return b))))
    ]
  }

  @specsubform[#:literals (let-values) (code:line let-values formals expr)]{

    Evaluates the @racket[expr] and binds the result according to
    @var[formals].

    Example:
    @; @example[
    @;   (with-instance TruthyMonad
    @;     ((do~ let-values xs '(1 2 3)
    @;           (return ($ + xs)))))
    @; ]
  }

  @specsubform[monad-expr]{

    Evaluates the @var[monad-expr] and discards the result.

    Examples:
    @example[
      (with-instance BoxMonad (do #&1 (return 2)))
    ]

    Examples:
    @; @example[
    @;   (with-instance TruthyMonad
    @;     ((do~ (return 1 2)
    @;           (return 3 4))))
    @; ]
  }
}

@; -----------------------------------------------------------------------------

@section[#:tag "class:base:monoid"]{Monoid}

@defmodule[algebraic/class/monoid]

The class of monoids (types with an associative binary operation that has an
identity).

@defidform[#:kind "class" Monoid]{

  Instances should satisfy the following laws:

  @itemlist[

    @item{@racket[(<> x mempty)] = @racket[x]}

    @item{@racket[(<> mempty x)] = @racket[x]}

    @item{@racket[(<> x (<> y z))] = @racket[(<> (<> x y) z)] (Semigroup law)}

    @item{@racket[mconcat] = @racket[(foldr <> mempty)]}

  ]

  The method names refer to the monoid of lists under concatenation, but there
  are many other instances.

  Some types can be viewed as a @racket[Monoid] in more than one way, e.g.
  both addition and multiplication on numbers.

  @; In such cases we often define newtypes and make those instances of Monoid,
  @; e.g. Sum and Product.

}

@; .............................................................................

@subsection[#:tag "class:base:monoid:members"]{Members}

@defthing[mempty any/c]{

  Identity of @racket[mappend].

}

@defproc[(mappend [a any/c] [b any/c]) any]{

  An associative operation.

  @bold{NOTE:} This method is redundant and has the default implementation
  @racket[mappend] = @racket[<>].

}

@defproc[(mconcat [as list?]) any]{

  Fold a list using the @racket[Monoid].

  For most types, the default definition for @racket[mconcat] will be used,
  but the function is included in the class definition so that an optimized
  version can be provided for specific types.

}

@; -----------------------------------------------------------------------------

@section[#:tag "class:base:semigroup"]{Semigroup}

@defmodule[algebraic/class/semigroup]

The class of semigroups (types with an associative binary operation).


@defidform[#:kind "class" Semigroup]{

  Instances should satisfy the associativity law:

  @itemlist[@item{@racket[(<> x (<> y z))] = @racket[(<> (<> x y) z)]}]

}

@; .............................................................................

@subsection[#:tag "class:base:semigroup:members"]{Members}

@defproc[(<> [a any/c] [b any/c]) any]{

  An associative operation.

}

@defproc[(sconcat [as (non-empty-listof any/c)]) any]{

  Reduce a non-empty list with @racket[<>].

  The default definition should be sufficient, but this can be overridden for
  efficiency.

}

@defproc[(stimes [n exact-positive-integer?] [a any/c]) any]{

  Repeat a value @var[n] times.

  Given that this works on a @racket[Semigroup] it is allowed to fail if you
  request 0 or fewer repetitions, and the default definition wll do so.

  @; By making this a member of the class, idempotent @racket[Semigroup]s and
  @; @racket[Monoid]s can upgrade this to execute in O(1) by picking
  @; @racket[stimes] = @racket[stimesIdempotent] or @racket[stimes] =
  @; stimesIdempotentMonoid respectively.

}

@; .............................................................................

@subsection[#:tag "class:base:semigroup:helpers"]{Helpers}

@defproc[(stimesDefault [b exact-positive-integer?] [a Semigroup]) any]{

  The default implementation of @racket[stimes].

}
