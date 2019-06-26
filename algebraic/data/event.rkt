#lang algebraic/racket/base

(require algebraic/class/applicative
         algebraic/class/functor
         algebraic/class/monad)

(provide (all-defined-out)
         (for-syntax (all-defined-out)))

(define-syntax EventFunctor
  (instance Functor
    [fmap (flip handle-evt)]))

(define-syntax EventMonad
  (instance Monad
    extends (EventFunctor)
    [>>= replace-evt]
    [return (λ xs (fmap (λ _ ($ id xs)) always-evt))]))

(define-syntax EventApplicative
  (instance Applicative
    extends (EventMonad)
    [pure return]
    [liftA2 (λ (f a b)
              (do xs <- a
                  ys <- b
                  (return ($ f (++ xs ys)))))]))
