#lang algebraic/racket/base

(require algebraic/control/applicative
         algebraic/control/monad
         algebraic/data/functor)

(provide (all-defined-out)
         (for-syntax (all-defined-out)))

(define-syntax event-functor
  (instance functor
    [fmap (flip handle-evt)]))

(define-syntax event-monad
  (instance monad
    extends (event-functor)
    [>>= replace-evt]
    [return (λ xs (fmap (λ _ ($ id xs)) always-evt))]))

(define-syntax event-applicative
  (instance applicative
    extends (event-monad)
    [pure return]
    [liftA2 (λ (f a b)
              (do xs <- a
                  ys <- b
                  (return ($ f (++ xs ys)))))]))
