#lang algebraic/racket/base

(provide (all-defined-out))

;; The class of semigroups (types with an associative binary operation).
;;
;; Instances should satisfy the associativity law:
;;
;;  *  x <> (y <> z) = (x <> y) <> z

(class Semigroup

  ;; An associative operation
  ;;
  ;; (<>) :: a -> a -> a
  [<>]

  ;; Reduce a non-empty list with <>
  ;;
  ;; The default definition should be sufficient, but this can be
  ;; overridden for efficiency.
  ;;
  ;; sconcat :: NonEmpty a -> a
  [sconcat (φ (a . as)
             (define go (function*
                          [(b (c . cs)) (<> b (go c cs))]
                          [(b ()) b]))
             (go a as))]

  ;; Repeat a value n times
  ;;
  ;; Given that this works on a 'Semigroup' it is allowed to fail if you
  ;; request 0 or fewer repetitions, and the default definition will do so.
  ;;
  ;; By making this a member of the class, idempotent semigroups and monoids
  ;; can upgrade this to execute in O(1) by picking stimes = stimesIdempotent
  ;; or stimes = stimesIdempotentMonoid respectively.
  ;;
  ;; stimes :: Integral b => b -> a -> a
  [stimes stimesDefault]

  minimal ([<>]))

;;; ----------------------------------------------------------------------------

;; stimesDefault :: (Integral b, Semigroup a) => b -> a -> a
(define-syntax stimesDefault
  (class-helper
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
      (f y0 x0)])))
