#lang algebraic/racket/base

(require algebraic/data/semigroup)

(provide (all-defined-out))

(class monoid
  [mempty]
  [mappend <>]
  [mconcat (>> foldr mappend mempty)]
  minimal ([mempty]))
