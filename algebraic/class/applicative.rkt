#lang algebraic/racket/base

(require algebraic/class/functor)

(provide (all-defined-out))

;; A functor with application, providing operations to
;;
;;  * embed pure expressions (pure), and
;;
;;  * sequence computations and combine their results (<*> and liftA2).
;;
;; A minimal complete definition must include implementations of pure and of
;; either <*> or liftA2. If it defines both, then they must behave the same as
;; their default definitions:
;;
;;      <*> = liftA2 id
;;
;;      liftA2 f x y = f <$> x <*> y
;;
;; Further, any definition must satisfy the following:
;;
;; /identity/
;;
;;      pure id <*> v = v
;;
;; /composition/
;;
;;      pure (.) <*> u <*> v <*> w = u <*> (v <*> w)
;;
;; /homomorphism/
;;
;;      pure f <*> pure x = pure (f x)
;;
;; /interchange/
;;
;;      u <*> pure y = pure ($ y) <*> u
;;
;;
;; The other methods have the following default definitions, which may be
;; overridden with equivalent specialized implementations:
;;
;;  * u *> v = (id <$ u) <*> v
;;
;;  * u <* v = liftA2 const u v
;;
;; As a consequence of these laws, the Functor instance for f will satisfy
;;
;;  * fmap f x = pure f <*> x
;;
;;
;; It may be useful to note that supposing
;;
;;     forall x y. p (q x y) = f x . g y
;;
;; it follows from the above that
;;
;;     liftA2 p (liftA2 q u v) = liftA2 f u . liftA2 g v
;;
;; If f is also a Monad, it should satisfy
;;
;;
;;  * pure = return
;;
;;  * (<*>) = ap
;;
;;  * (*>) = (>>)
;;
;; (which implies that pure and <*> satisfy the applicative functor laws).
;;
;; MINIMAL pure, ((<*>) | liftA2)

(class Applicative

  ;; Lift a value.
  ;;
  ;; pure :: a -> f a
  [pure]

  ;; Sequential application.
  ;;
  ;; A few functors support an implementation of <*> that is more efficient
  ;; than the default one.
  ;;
  ;; (<*>) :: f (a -> b) -> f a -> f b
  [<*> (>> liftA2 (λ (f a) (f a)))]

  ;; Lift a binary function to actions.
  ;;
  ;; Some functors support an implementation of liftA2 that is more efficient
  ;; than the default one. In particular, if fmap is an expensive operation,
  ;; it is likely better to use liftA2 than to fmap over the structure and
  ;; then use <*>.
  ;;
  ;; liftA2 :: (a -> b -> c) -> f a -> f b -> f c
  [liftA2 (λ (f a b) (<*> (fmap (.. (>> $ >> f) list) a) b))]

  ;; Sequence actions, discarding the value of the first argument.
  ;;
  ;; This is essentially the same as liftA2 (flip const), but if the Functor
  ;; instance has an optimized (<$), it may be better to use that instead.
  ;; Before liftA2 became a method, this definition was strictly better, but
  ;; now it depends on the functor. For a functor supporting a
  ;; sharing-enhancing (<$), this definition may reduce allocation by
  ;; preventing a1 from ever being fully realized. In an implementation with a
  ;; boring (<$) but an optimizing liftA2, it would likely be better to define
  ;; (*>) using liftA2.
  ;;
  ;; (*>) :: f a -> f b -> f b
  [*> (λ (a b) (<*> (<$ id a) b))]

  ;; Sequence actions, discarding the value of the second argument.
  ;;
  ;; (<*) :: f a -> f b -> f a
  [<* (λ (a b) (<*> (fmap const a) b))]

  minimal ([pure <*>]
           [pure liftA2]))

;;; ----------------------------------------------------------------------------

;; A variant of <*> with the arguments reversed.
;;
;; (<**>) :: Applicative f => f a -> f (a -> b) -> f b
(define-syntax <**> (class-helper (>> liftA2 (λ (a f) (f a)))))

;; A lazy variant of <*>.
;;
;; Eager applicative chains get noisy as they grow because Racket expects us
;; to curry our own functions.
;;
;; Example:
;;
;;   (<*> (pure +) (pure 1))
;;   (<*> (<*> (pure (>>* +)) (pure 1)) (pure 2))
;;   (<*> (<*> (<*> (pure (>>* (>>* +))) (pure 1)) (pure 2)) (pure 3))
;;
;; The function (in this case, +) must be wrapped in one >>* for each
;; applicative argument or <*> will fail.
;;
;; This variant wraps the function with >>* automatically. Making the final
;; operator in the chain eager (<*>) will force evaluation of the whole chain.
;;
;; Example:
;;
;;   (<*> (<*>~ (pure +) (pure 1)) (pure 2))
;;   (<*> (<*> (<*> (pure (>>* (>>* +))) (pure 1)) (pure 2)) (pure 3))
(define-syntax <*>~ (class-helper (>> liftA2 (λ (f a) ((>>* f) a)))))

;;; A lazy variant of <**>.
(define-syntax <**>~ (class-helper (>> liftA2 (λ (a f) ((>>* f) a)))))
