#lang algebraic/racket/base

(require algebraic/control/applicative
         algebraic/control/monad
         algebraic/data/functor)

(provide (all-defined-out))

(define-syntax box-Functor
  (instance Functor
    [fmap (λ (f x) (box (f (unbox x))))]))

(define-syntax box-Applicative
  (instance Applicative
    extends (box-Functor)
    [pure box]
    [<*> (λ (f x) (pure ((unbox f) (unbox x))))]
    [liftA2 (λ (f x y) (pure (f (unbox x) (unbox y))))]))

(define-syntax box-Monad
  (instance Monad
    extends (box-Applicative)
    [>>= (λ (x f) (f (unbox x)))]))

;;; ----------------------------------------------------------------------------

(module+ test
  (require rackunit)

  (test-case "box-Functor fmap"
    (with-instance box-Functor
      (check = (unbox (fmap add1 #&1)) 2)))

  (test-case "box-Functor <$"
    (with-instance box-Functor
      (check eq? (unbox (<$ 'X #&1)) 'X)
      (check eq? (unbox (<$ 'X #&2)) 'X)))

  (test-case "box-Functor <$>"
    (with-instance box-Functor
      (check = (unbox (<$> add1 #&2)) 3)
      (check = (unbox (let ([g <$>]) (g add1 #&3))) 4)
      (check equal?
             (map unbox (map (>> <$> add1) (list #&1 #&2 #&3)))
             '(2 3 4))))

  (test-case "box-Applicative"
    (with-instance box-Applicative
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

  (test-case "box-Monad"
    (with-instances (box-Applicative box-Functor box-Monad)
      (check = (unbox (>>= #&1 (.. return add1))) 2)
      (check = (unbox (return 1)) 1)
      (check = (unbox (>>M (return 1) (return 2))) 2)
      (check = (unbox (=<< (.. return add1) #&1)) 2))))
