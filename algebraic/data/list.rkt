#lang algebraic/racket/base

(require algebraic/control/applicative
         algebraic/control/monad
         algebraic/data/functor
         algebraic/data/monoid
         algebraic/data/semigroup)

(provide (all-defined-out))

(define-syntax list-Semigroup
  (instance Semigroup
    [<> ++]
    [stimes stimes-list]))

(define-syntax stimes-list
  (μ0 (function*
        [(n _) #:if (< n 0) (error "stimes: list, negative multiplier")]
        [(0 _) null]
        [(n x) (letrec ([rep (function [0 null] [i (++ x (rep (- i 1)))])])
                 (rep n))])))

(define-syntax list-Monoid
  (instance Monoid
    extends (list-Semigroup)
    [mempty null]
    [mconcat ++]))

(define-syntax list-Functor (instance Functor [fmap map]))

(define-syntax list-Applicative
  (instance Applicative
    extends (list-Functor)
    [pure list]
    [<*> (λ (fs xs) (map (λ (f x) (f x)) fs xs))]
    [liftA2 (λ (f xs ys) (map (λ (x y) (f x y)) xs ys))]
    [*> (λ (xs ys) (begin xs ys))]))

(define-syntax list-Monad
  (instance Monad
    extends (list-Applicative)
    [>>= (λ (xs f) ($ ++ (map f xs)))]
    [return list]
    [>>M *>]
    [fail (const null)]))

;;; -----------------------------------------------------------------------------

(module+ test
  (require rackunit)

  (test-case "list-Semigroup"
    (with-instance list-Semigroup
      (check equal? (<> '(1 2) '(3 4)) '(1 2 3 4))
      (check equal? (stimes 3 '(1 2)) '(1 2 1 2 1 2))))

  (test-case "list-Monoid"
    (with-instance list-Monoid
      (check equal? (mconcat '(1 2) '(3 4) '(5 6)) '(1 2 3 4 5 6))))

  (test-case "list-Functor"
    (with-instance list-Functor
      (check equal? (fmap add1 '(1 2 3)) '(2 3 4))))

  (test-case "list-Applicative"
    (with-instance list-Applicative
      (check equal? (pure 1) '(1))
      (check equal? (<*> (pure add1) (pure 2)) '(3))
      (check equal? (<*> (list (>> + 1) (>> + 2)) '(3 5)) '(4 7))
      (check equal? (liftA2 + (pure 1) (pure 2)) '(3))
      (check equal? (*> (pure 1) (pure 2)) '(2))))

  (test-case "list-Monad"
    (with-instance list-Monad
      (check equal? (>>= '(1 2 3) (.. return add1)) '(2 3 4))
      (check equal? (=<< (.. return add1) '(3 2 1)) '(4 3 2))
      (check equal? (>>M (return 1) (return 2)) '(2))
      (define (neighbors x dx) (list (- x dx) x (+ x dx)))
      (check equal?
             (do (x) <- (neighbors 0 10)
                 (y) <- (neighbors x 1)
                 (return y))
             '(-11 -10 -9 -1 0 1 9 10 11)))))
