#lang algebraic/racket/base

(require algebraic/control/applicative
         algebraic/control/monad
         algebraic/data/functor
         algebraic/data/monoid
         algebraic/data/semigroup
         (prefix-in algebraic- algebraic/racket/base/forms))

(provide (all-defined-out))

;;; ----------------------------------------------------------------------------

;; (define (maybe n f m)
;;   (case m
;;     [Nothing n]
;;     [(Just x) (f x)]))

;; (define (from-Maybe d m)
;;   (case m
;;     [Nothing d]
;;     [(Just v) v]))

;; (define (from-Maybe~ d m)
;;   (case m
;;     [Nothing d]
;;     [(Just v) (λ () v)]))

(define maybe
  (function*
    [(n _ Nothing) n]
    [(_ f (Just x)) (f x)]))

(define from-Maybe
  (function*
    [(d Nothing) d]
    [(_ (Just v)) v]))

(define from-Maybe~
  (function*
    [(d Nothing) d]
    [(_ (Just v)) (λ () v)]))

;;; ----------------------------------------------------------------------------

(define-syntax Maybe-Semigroup
  (instance Semigroup
    [<> (function*
          [(Nothing b) b]
          [(a Nothing) a]
          [((Just a) (Just b)) (Just (<> a b))])]
    [stimes stimes-Maybe]))

(define-class-helper stimes-Maybe
  (function*
    [(_ Nothing) Nothing]
    [(n (Just a)) (algebraic-case (compare n 0)
                    [LT (error "stimes: Maybe, negative multiplier")]
                    [EQ Nothing]
                    [GT (Just (stimes n a))])]))

;;; ----------------------------------------------------------------------------

(define-syntax Maybe-Monoid
  (instance Monoid
    extends (Maybe-Semigroup)
    [mempty Nothing]))

;;; ----------------------------------------------------------------------------

(define-syntax Maybe-Functor
  (instance Functor
    [fmap (function*
            [(_ Nothing) Nothing]
            [(f (Just a)) (Just (f a))])]))

;;; ----------------------------------------------------------------------------

(define-syntax Maybe-Applicative
  (instance Applicative
    extends (Maybe-Functor)
    [pure Just]
    [<*> (function*
           [((Just f) m) (fmap f m)]
           [(Nothing _) Nothing])]
    [liftA2 (function*
              [(f (Just x) (Just y)) (Just (f x y))]
              [(_ _ _) Nothing])]
    [*> (function*
          [((Just _) m) m]
          [(Nothing _) Nothing])]))

;;; ----------------------------------------------------------------------------

(define-syntax Maybe-Monad
  (instance Monad
    extends (Maybe-Applicative)
    [>>= (function*
           [((Just x) k) (k x)]
           [(Nothing _) Nothing])]
    [>>M *>]
    [fail (φ _ Nothing)]))

;;; ----------------------------------------------------------------------------

(module+ test
  (require rackunit)

  (test-case "Maybe-Functor"
    (with-instance Maybe-Functor
      (check equal? (fmap add1 Nothing) Nothing)
      (check equal? (fmap add1 (Just 2)) (Just 3))))

  (test-case "Maybe-Applicative"
    (with-instance Maybe-Applicative
      (check equal? (pure 1) (Just 1))
      (check equal? (<*> (pure add1) (pure 2)) (Just 3))
      (check equal? (<*> Nothing (pure 2)) Nothing)
      (check equal? (<*> (pure add1) Nothing) Nothing)
      (check equal? (<*> Nothing Nothing) Nothing)
      (check equal? (liftA2 + (pure 1) (pure 2)) (Just 3))
      (check equal? (liftA2 + Nothing (pure 2)) Nothing)
      (check equal? (liftA2 + (pure 1) Nothing) Nothing)
      (check equal? (liftA2 + Nothing Nothing) Nothing)
      (check equal? (*> (pure 1) (pure 2)) (Just 2))
      (check equal? (*> Nothing (pure 2)) Nothing)
      (check equal? (*> (pure 1) Nothing) Nothing)
      (check equal? (*> Nothing Nothing) Nothing)))

  (test-case "Maybe-Monad"
    (with-instance Maybe-Monad
      (check equal? (>>= (pure 2) (.. return add1)) (pure 3))
      (check equal? (>>= Nothing (.. return add1)) Nothing)
      (check equal? (>>M (pure 1) (pure 2)) (Just 2))
      (check equal? (>>M Nothing (pure 2)) Nothing)
      (check equal? (>>M (pure 1) Nothing) Nothing)
      (check equal? (>>M Nothing Nothing) Nothing)
      (check equal?
             (do (x) <- (pure 1)
                 (y) <- (pure 2)
                 (return (+ x y)))
             (Just 3))
      (check equal?
             (do (x) <- Nothing
                 (y) <- (pure 2)
                 (return (+ x y)))
             Nothing)
      (check equal?
             (do (x) <- (pure 1)
                 (y) <- Nothing
                 (return (+ x y)))
             Nothing))))
