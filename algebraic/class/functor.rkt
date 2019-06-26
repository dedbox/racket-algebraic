#lang algebraic/racket/base

(require algebraic/syntax)

(provide (all-defined-out))

;;; ----------------------------------------------------------------------------
;; The Functor class is used for types that can be mapped over. Instances of
;; Functor should satisfy the following laws:
;;
;;  * fmap id  ==  id
;;  * fmap (f . g)  ==  fmap f . fmap g
(class Functor

  ;; fmap :: (a -> b) -> f a -> f b
  [fmap]

  ;; Replace all locations in the input with the same value. The default
  ;; definition is fmap . const, but this may be overridden with a more
  ;; efficient version.
  ;;
  ;; (<$) :: a -> f b -> f a
  [<$ (λ (a f-b) (fmap (const a) f-b))]

  minimal ([fmap]))

;;; ----------------------------------------------------------------------------
;;; Helpers

;; An infix synonym for fmap.
;;
;; The name of this operator is an allusion to $. Note the similarities
;; between their types:
;;
;;   ($)  ::              (a -> b) ->   a ->   b
;;  (<$>) :: Functor f => (a -> b) -> f a -> f b
;;
;; Whereas $ is function application, <$> is function application lifted over
;; a Functor.
;;
;; === Examples ===
;;
;; Convert from a Maybe Int to a Maybe String using show:
;;
;;  > show <$> Nothing
;;  Nothing
;;  > show <$> Just 3
;;  Just "3"
;;
;; Convert from an Either Int Int to an Either Int String using show:
;;
;; > show <$> Left 17
;; Left 17
;; > show <$> Right 17
;; Right "17"
;;
;; Double each element of a list:
;;
;; > (*2) <$> [1,2,3]
;; [2,4,6]
;;
;; Apply even to the second element of a pair:
;;
;; > even <$> (2,2)
;; (2,True)

;; (<$>) :: Functor f => (a -> b) -> f a -> f b
(define-syntax <$> (μ0 #,(#%rewrite fmap)))

;; A lazy variant of <$>.
(define-syntax <$>~ (μ0 #,(#%rewrite (λ (f x) (fmap (>>* f) x)))))
