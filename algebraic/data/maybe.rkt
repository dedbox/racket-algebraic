#lang algebraic/racket/base

(require algebraic/class/applicative
         algebraic/class/functor
         algebraic/class/monad)

(provide (all-defined-out))

(data Maybe (Nothing Just))

(define-syntax MaybeFunctor
  (instance Functor
    [fmap (function*
            [(_ Nothing) Nothing]
            [(f (Just a)) (Just (f a))])]))

(define-syntax MaybeApplicative
  (instance Applicative
    extends (MaybeFunctor)
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

(define-syntax MaybeMonad
  (instance Monad
    extends (MaybeApplicative)
    [>>= (function*
           [((Just x) k) (k x)]
           [(Nothing _) Nothing])]
    [>>M *>]
    [fail (φ _ Nothing)]))

;;; ----------------------------------------------------------------------------

(module+ test
  (require rackunit)

  (test-case "MaybeFunctor"
    (with-instance MaybeFunctor
      (check equal? (fmap add1 Nothing) Nothing)
      (check equal? (fmap add1 (Just 2)) (Just 3))))

  (test-case "MaybeApplicative"
    (with-instance MaybeApplicative
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

  (test-case "MaybeMonad"
    (with-instance MaybeMonad
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
