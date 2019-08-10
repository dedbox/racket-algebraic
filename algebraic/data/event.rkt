#lang algebraic/racket/base

(require algebraic/control/applicative
         algebraic/control/monad
         algebraic/data/functor)

(provide (all-defined-out)
         (for-syntax (all-defined-out)))

(define-syntax event-Functor
  (instance Functor
    [fmap (flip handle-evt)]))

(define-syntax event-Monad
  (instance Monad
    extends (event-Functor)
    [>>= replace-evt]
    [return (λ xs (fmap (λ _ ($ id xs)) always-evt))]))

(define-syntax event-Applicative
  (instance Applicative
    extends (event-Monad)
    [pure return]
    [liftA2 (λ (f a b)
              (do xs <- a
                  ys <- b
                  (return ($ f (++ xs ys)))))]))
