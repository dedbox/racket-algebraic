#lang algebraic/racket/base

(require algebraic/class/semigroup)

(provide (all-defined-out))

;;; ----------------------------------------------------------------------------
;; The class of monoids (types with an associative binary operation that has
;; an identity).
;; 
;; Instances should satisfy the following laws:
;;
;;  * x <> mempty = x
;;
;;  * mempty <> x = x
;;
;;  * x <> (y <> z) = (x <> y) <> z (Semigroup law)
;;
;;  * mconcat = foldr (<>) mempty
;;
;; The method names refer to the monoid of lists under concatenation, but
;; there are many other instances.
;;
;; Some types can be viewed as a monoid in more than one way, e.g. both
;; addition and multiplication on numbers. In such cases we often define
;; newtypes and make those instances of Monoid, e.g. Sum and Product.
(class Monoid

  ;; Identity of mappend
  ;;
  ;; mempty :: a
  [mempty]

  ;; An associative operation
  ;;
  ;; __NOTE__: This method is redundant and has the default implementation
  ;; mappend = (<>).
  ;;
  ;; mappend :: a -> a -> a
  [mappend <>]

  ;; Fold a list using the monoid.
  ;;
  ;; For most types, the default definition for 'mconcat' will be used, but
  ;; the function is included in the class definition so that an optimized
  ;; version can be provided for specific types.
  ;;
  ;; mconcat :: [a] -> a
  [mconcat (>> foldr mappend mempty)]

  minimal ([mempty]))
