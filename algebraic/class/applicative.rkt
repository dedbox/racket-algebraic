#lang algebraic/racket/base

(require algebraic/class/functor)

(provide (all-defined-out))

(class applicative
  [pure]
  [<*> (>> liftA2 (λ (f a) (f a)))]
  [liftA2 (λ (f a b) (<*> (fmap (.. (>> $ >> f) list) a) b))]
  [*> (λ (a b) (<*> (<$ id a) b))]
  [<* (λ (a b) (<*> (fmap const a) b))]
  minimal ([pure <*>]
           [pure liftA2]))

(define-class-helper <**>  (>> liftA2 (λ (a f) (f a))))
(define-class-helper <**>~ (>> liftA2 (λ (a f) ((>>* f) a))))
(define-class-helper <*>~  (>> liftA2 (λ (f a) ((>>* f) a))))
