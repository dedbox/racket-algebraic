#lang algebraic/racket/base

(require algebraic/control/applicative
         algebraic/control/monad
         algebraic/data/functor
         algebraic/data/monoid
         algebraic/data/semigroup
         (prefix-in algebraic- algebraic/racket/base/forms))

(provide (all-defined-out))

;;; ----------------------------------------------------------------------------

(define maybe
  (function*
    [(n _ Nothing) n]
    [(_ f (Just x)) (f x)]))

(define from-maybe
  (function*
    [(d Nothing) d]
    [(_ (Just v)) v]))

(define from-maybe~
  (function*
    [(d Nothing) d]
    [(_ (Just v)) (λ () v)]))

;;; ----------------------------------------------------------------------------

(define-syntax maybe-semigroup
  (instance semigroup
    [<> (function*
          [(Nothing b) b]
          [(a Nothing) a]
          [((Just a) (Just b)) (Just (<> a b))])]
    [stimes stimes-maybe]))

(define-class-helper stimes-maybe
  (function*
    [(_ Nothing) Nothing]
    [(n (Just a)) (algebraic-case (compare n 0)
                    [LT (error "stimes: Maybe, negative multiplier")]
                    [EQ Nothing]
                    [GT (Just (stimes n a))])]))

;;; ----------------------------------------------------------------------------

(define-syntax maybe-monoid
  (instance monoid extends (maybe-semigroup) [mempty Nothing]))

;;; ----------------------------------------------------------------------------

(define-syntax maybe-functor
  (instance functor
    [fmap (function*
            [(_ Nothing) Nothing]
            [(f (Just a)) (Just (f a))])]))

;;; ----------------------------------------------------------------------------

(define-syntax maybe-applicative
  (instance applicative
    extends (maybe-functor)
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

(define-syntax maybe-monad
  (instance monad
    extends (maybe-applicative)
    [>>= (function*
           [((Just x) k) (k x)]
           [(Nothing _) Nothing])]
    [>>M *>]
    [fail (φ _ Nothing)]))

;;; ----------------------------------------------------------------------------

(module+ test
  (require rackunit)

  (test-case "maybe-functor"
    (with-instance maybe-functor
      (check equal? (fmap add1 Nothing) Nothing)
      (check equal? (fmap add1 (Just 2)) (Just 3))))

  (test-case "maybe-applicative"
    (with-instance maybe-applicative
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

  (test-case "maybe-monad"
    (with-instance maybe-monad
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
