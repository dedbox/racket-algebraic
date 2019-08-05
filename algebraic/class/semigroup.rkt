#lang algebraic/racket/base

(provide (all-defined-out))

(class semigroup
  [<>]
  [sconcat (φ (a . as)
             (define go (function*
                          [(b (c . cs)) (<> b (go c cs))]
                          [(b ()) b]))
             (go a as))]
  [stimes stimes-default]
  minimal ([<>]))

(define-class-helper stimes-default
  (function*
    [(y _) #:if (<= y 0) (error "stimes: positive multiplier expected")]
    [(y0 x0)
     (define f (function*
                 [(x y) #:if (even? y) (f (<> x x) (quotient y 2))]
                 [(x 1) x]
                 [(x y) (g (<> x x) (quotient y 2) x)]))
     (define g (function*
                 [(x y z) #:if (even? y) (g (<> x x) (quotient y 2) z)]
                 [(x 1 z) (<> x z)]
                 [(x y z) (g (<> x x) (quotient y 2) (<> x z))]))
     (f y0 x0)]))
