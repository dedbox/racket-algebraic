#lang algebraic/racket/base

(provide (all-defined-out))

(class Functor
  [fmap]
  [<$ (λ (x y) (fmap (const x) y))]
  minimal ([fmap]))

(define-class-helper <$> fmap)
(define-class-helper (<$>~ f x) (fmap (>>* f) x))
