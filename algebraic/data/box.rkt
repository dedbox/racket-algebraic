#lang algebraic/racket/base

(require algebraic/control/applicative
         algebraic/control/monad
         algebraic/data/functor)

(provide (all-defined-out))

(define-syntax box-functor
  (instance functor
    [fmap (λ (f x) (box (f (unbox x))))]))

(define-syntax box-applicative
  (instance applicative
    extends (box-functor)
    [pure box]
    [<*> (λ (f x) (pure ((unbox f) (unbox x))))]
    [liftA2 (λ (f x y) (pure (f (unbox x) (unbox y))))]))

(define-syntax box-monad
  (instance monad
    extends (box-applicative)
    [>>= (λ (x f) (f (unbox x)))]))

;;; ----------------------------------------------------------------------------

(module+ test
  (require rackunit)

  (test-case "box-functor fmap"
    (with-instance box-functor
      (check = (unbox (fmap add1 #&1)) 2)))

  (test-case "box-functor <$"
    (with-instance box-functor
      (check eq? (unbox (<$ 'X #&1)) 'X)
      (check eq? (unbox (<$ 'X #&2)) 'X)))

  (test-case "box-functor <$>"
    (with-instance box-functor
      (check = (unbox (<$> add1 #&2)) 3)
      (check = (unbox (let ([g <$>]) (g add1 #&3))) 4)
      (check equal?
             (map unbox (map (>> <$> add1) (list #&1 #&2 #&3)))
             '(2 3 4))))

  (test-case "box-applicative"
    (with-instance box-applicative
      (check = (unbox (pure 1)) 1)
      (check = (unbox (<*> (pure add1) (pure 2))) 3)
      (check = (unbox (<*> (<*> (pure (>>* +)) (pure 1)) (pure 2))) 3)
      (check = (unbox (<*> (<*> (<*> (pure (>>* (>>* +)))
                                     (pure 1))
                                (pure 2))
                           (pure 3)))
             6)
      (check = (unbox (<*> (<*> (<*>~ (pure (>>* +)) (pure 1)) (pure 2)) (pure 3))) 6)
      (check = (unbox (<*> (<*>~ (<*>~ (pure +) (pure 1)) (pure 2)) (pure 3))) 6)
      (check = (unbox (liftA2 + (pure 1) (pure 2))) 3)
      (check = (unbox (<**> (pure 2) (pure add1))) 3)))

  (test-case "box-monad"
    (with-instances (box-applicative box-functor box-monad)
      (check = (unbox (>>= #&1 (.. return add1))) 2)
      (check = (unbox (return 1)) 1)
      (check = (unbox (>>M (return 1) (return 2))) 2)
      (check = (unbox (=<< (.. return add1) #&1)) 2))))
