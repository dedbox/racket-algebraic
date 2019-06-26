#lang scribble/manual

@title[#:tag "class:data"]{Instances}

@require{./algebraic-includes.rkt}

@require[
  (except-in algebraic/sum sum)
  @for-label[
    algebraic/class/applicative
    algebraic/class/functor
    algebraic/class/monad
    algebraic/class/monoid
    algebraic/class/semigroup
    algebraic/data/box
    algebraic/data/event
    algebraic/data/list
    algebraic/data/maybe
    algebraic/data/values
    (except-in algebraic/racket/base do #%module-begin)
    racket/contract/base
  ]
]

@define[class-data-eval (algebraic-evaluator)]
@define-syntax[example (algebraic-example/locations class-data-eval)]

@example[#:hidden
  (require algebraic/class/applicative
           algebraic/class/functor
           algebraic/class/monad
           algebraic/class/monoid
           algebraic/class/semigroup
           algebraic/data/box
           algebraic/data/event
           algebraic/data/list
           algebraic/data/maybe
           algebraic/data/values)]

@(define-syntax-rule (definstance name content ...)
   (defidform #:kind "instance" name content ...))

@(define-syntax-rule (defmembers [name contract] ...)
   (nested #:style 'inset
           (deftogether ((defthing #:link-target? #f name contract) ...))))

@(define-syntax (defdata stx)
   (syntax-case stx ()
     [(_ name content ...)
      (with-syntax ([name* (sum-introducer #'name 'add)])
        #'(defform #:kind "sum" #:id [name #'name*] name content ...))]))

@(define-syntax (sum stx)
   (syntax-case stx ()
     [(_ name) #`(racket #,(sum-introducer #'name 'add))]))

@(define-syntax-rule (defproducts name ...)
   (nested #:style 'inset
           (deftogether ((defidform #:kind "product" name) ...))))

@; #############################################################################

@section[#:tag "class:data:box"]{Box}

@defmodule[algebraic/data/box]

@definstance[BoxMonad]{

  extends @racket[BoxApplicative]

  @defmembers[[>>= procedure?]]

  Example:
  @example[
    (with-instance BoxMonad
      (do (`(? ,x)) <- (box '(? 2))
          (return `(! ,(add1 x)))))
  ]
}

@; .............................................................................

@definstance[BoxApplicative]{

  extends @racket[BoxFunctor]

  @defmembers[
    [pure procedure?]
    [<*> procedure?]
    [liftA2 procedure?]
  ]

  Examples:
  @example[
    (with-instance BoxApplicative
      (<*> (pure add1) (pure 2)))
  ]

  @example[
    (with-instance BoxApplicative
      (liftA2 + (pure 4) (pure 5)))
  ]
}

@definstance[BoxFunctor]{
  @defmembers[[fmap procedure?]]

  Example:
  @example[
    (with-instance BoxFunctor
      (fmap (>> :: '!)  #&?))
  ]
}

@; -----------------------------------------------------------------------------

@section[#:tag "class:data:list"]{List}

@defmodule[algebraic/data/list]

@definstance[ListMonad]{

  extends @racket[ListApplicative]

  @defmembers[
    [>>= procedure?]
    [return procedure?]
    [>>M procedure?]
    [fail procedure?]
  ]

  Examples:
  @example[(with-instance ListMonad (>>= (return 2) (.. return add1)))]

  @example[(with-instance ListMonad (>>M (return 1) (return 2)))]

  @example[(with-instance ListMonad (fail '!?))]

  @example[(with-instance ListMonad (join '((1) (2))))]
}

@; .............................................................................

@definstance[ListApplicative]{

  extends @racket[ListFunctor]

  @defmembers[
    [pure procedure?]
    [<*> procedure?]
    [liftA2 procedure?]
    [*> procedure?]
  ]

  Examples:
  @example[(with-instance ListApplicative (pure 1))]

  @example[
    (with-instance ListApplicative
      (<*> (<*> (pure (>>* ::)) '(1)) '(2)))
  ]

  @example[
    (with-instance ListApplicative (liftA2 :: '(?) '(!)))
  ]

  @example[(with-instance ListApplicative (*> '(1) '(2)))]
}

@; .............................................................................

@definstance[ListFunctor]{
  @defmembers[[fmap procedure?]]

  Example:
  @example[(with-instance ListFunctor (fmap add1 '(1 2 3 4)))]
}

@; .............................................................................

@definstance[ListMonoid]{

  extends @racket[ListSemigroup]

  @defmembers[
    [mempty null?]
    [mconcat procedure?]
  ]

  Example:
  @example[(with-instance ListMonoid (mconcat '(1 2) '(3 4) mempty))]
}

@; .............................................................................

@definstance[ListSemigroup]{
  @defmembers[
    [<> procedure?]
    [stimes procedure?]
  ]

  Examples:
  @example[(with-instance ListSemigroup (<> '(1 2) '(3 4)))]

  @example[(with-instance ListSemigroup (stimes 3 '(! ?)))]
}

@; -----------------------------------------------------------------------------

@section[#:tag "class:data:Values"]{Values}

@defmodule[algebraic/data/values]

@definstance[ValuesMonad]{

  extends @racket[ValuesApplicative]

  @defmembers[
    [>>= procedure?]
    [return procedure?]
    [>>M procedure?]
  ]

  Examples:
  @example[(with-instance ValuesMonad (>>= (λ () (return 1 2 3)) +))]

  @example[(with-instance ValuesMonad (>>M (λ () 1) (λ () 2)))]
}

@; .............................................................................

@definstance[ValuesApplicative]{

  extends @racket[ValuesFunctor]

  @defmembers[
    [pure procedure?]
    [liftA2 procedure?]
  ]

  Examples:
  @example[(with-instance ValuesApplicative (pure 1 2 3))]

  @example[
    (with-instance ValuesApplicative
      (liftA2 list
              (λ () (pure 1 2))
              (λ () (pure 3 4))))
  ]
}

@; .............................................................................

@definstance[ValuesFunctor]{
  @defmembers[[fmap procedure?]]

  Example:
  @example[(with-instance ValuesFunctor (fmap list (λ () (id 1 2 3))))]
}

@; -----------------------------------------------------------------------------

@section[#:tag "class:data:event"]{Event}

@defmodule[algebraic/data/event]

@definstance[EventFunctor]{
  @defmembers[[fmap procedure?]]

  Examples:
  @example[
    (with-instance EventFunctor
      (sync (fmap (λ _ (id 1 2 3)) always-evt)))
  ]
}

@; .............................................................................

@definstance[EventMonad]{

  extends @racket[EventFunctor]

  @defmembers[
    [>>= procedure?]
    [return procedure?]
  ]

  Examples:
  @example[(with-instance EventMonad
             (sync (>>= (return 1 2 3) (.. return +))))]
}

@; .............................................................................

@definstance[EventApplicative]{

  extends @racket[EventMonad]

  @defmembers[
    [pure procedure?]
    [liftA2 procedure?]
  ]

  Examples:
  @example[(with-instance EventApplicative (sync (pure 1 2 3)))]

  @example[
    (with-instance EventApplicative
      (sync (liftA2 list (pure 1 2) (pure 3 4))))
  ]
}

@; -----------------------------------------------------------------------------

@section[#:tag "class:data:maybe"]{Maybe}

@defmodule[algebraic/data/maybe]

The @sum[Maybe] @tech{sum}, and associated operations.

@defdata[Maybe]{

The @sum[Maybe] @tech{sum} encapsulates an optional value. A member of
@sum[Maybe] either contains a value @racketid[a] (represented as @racket[(Just
a)]), or it is empty (represented as @racket[Nothing]). Using @sum[Maybe] is a
good way to deal with errors or exceptional cases without resorting to drastic
measures such as @racket[error].

The @sum[Maybe] @tech{sum} is also a @racket[Monad]. It is a simple kind of
error monad, where all errors are represented by @racket[Nothing]. A richer
error @racket[Monad] can be built using the @racket[Either] type.

@defproducts[Nothing Just]

}

@definstance[MaybeMonad]{

  extends @racket[MaybeApplicative]

  @defmembers[
    [>>= procedure?]
    [>>M procedure?]
    [fail procedure?]
  ]

  Examples:
  @example[
    (with-instance MaybeMonad
      (>>= (Just 2) (.. return add1)))
  ]

  @example[(with-instance MaybeMonad (>>= Nothing (.. Just add1)))]

  @example[(with-instance MaybeMonad (>>M (Just 1) (Just 2)))]

  @example[(with-instance MaybeMonad (>>M Nothing (Just 2)))]

  @example[(with-instance MaybeMonad (fail '!!))]
}

@; .............................................................................

@definstance[MaybeApplicative]{

  extends @racket[MaybeFunctor]

  @defmembers[
    [pure procedure?]
    [<*> procedure?]
    [liftA2 procedure?]
    [*> procedure?]
  ]

  Examples:
  @example[(with-instance MaybeApplicative (pure 1))]

  @example[(with-instance MaybeApplicative (<*> (pure add1) (pure 2)))]

  @example[(with-instance MaybeApplicative (<*> (pure add1) Nothing))]

  @example[(with-instance MaybeApplicative (<*> Nothing (pure 2)))]

  @example[(with-instance MaybeApplicative (liftA2 + (pure 1) (pure 2)))]

  @example[(with-instance MaybeApplicative (liftA2 + Nothing (pure 2)))]

  @example[(with-instance MaybeApplicative (liftA2 + (pure 1) Nothing))]

  @example[(with-instance MaybeApplicative (*> (pure 1) (pure 2)))]

  @example[(with-instance MaybeApplicative (*> Nothing (pure 2)))]

  @example[(with-instance MaybeApplicative (*> (pure 1) Nothing))]
}

@; .............................................................................

@definstance[MaybeFunctor]{
  @defmembers[[fmap procedure?]]

  Examples:
  @example[(with-instance MaybeFunctor (fmap add1 (Just 2)))]

  @example[(with-instance MaybeFunctor (fmap add1 Nothing))]
}
