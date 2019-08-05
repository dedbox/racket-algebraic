#lang algebraic/racket/base

(require algebraic/class/applicative
         algebraic/class/functor
         algebraic/class/monoid
         algebraic/class/monad
         algebraic/class/semigroup)

(provide (all-defined-out))

(define-syntax list-semigroup
  (instance semigroup
    [<> ++]
    [stimes stimes-list]))

(define-syntax stimes-list
  (μ0 (function*
        [(n _) #:if (< n 0) (error "stimes: list, negative multiplier")]
        [(0 _) null]
        [(n x) (letrec ([rep (function [0 null] [i (++ x (rep (- i 1)))])])
                 (rep n))])))

(define-syntax list-monoid
  (instance monoid
    extends (list-semigroup)
    [mempty null]
    [mconcat ++]))

(define-syntax list-functor (instance functor [fmap map]))

(define-syntax list-applicative
  (instance applicative
    extends (list-functor)
    [pure list]
    [<*> (λ (fs xs) (map (λ (f x) (f x)) fs xs))]
    [liftA2 (λ (f xs ys) (map (λ (x y) (f x y)) xs ys))]
    [*> (λ (xs ys) (begin xs ys))]))

(define-syntax list-monad
  (instance monad
    extends (list-applicative)
    [>>= (λ (xs f) ($ ++ (map f xs)))]
    [return list]
    [>>M *>]
    [fail (const null)]))

;;; -----------------------------------------------------------------------------

(module+ test
  (require rackunit)

  (test-case "list-semigroup"
    (with-instance list-semigroup
      (check equal? (<> '(1 2) '(3 4)) '(1 2 3 4))
      (check equal? (stimes 3 '(1 2)) '(1 2 1 2 1 2))))

  (test-case "list-monoid"
    (with-instance list-monoid
      (check equal? (mconcat '(1 2) '(3 4) '(5 6)) '(1 2 3 4 5 6))))

  (test-case "list-functor"
    (with-instance list-functor
      (check equal? (fmap add1 '(1 2 3)) '(2 3 4))))

  (test-case "list-applicative"
    (with-instance list-applicative
      (check equal? (pure 1) '(1))
      (check equal? (<*> (pure add1) (pure 2)) '(3))
      (check equal? (<*> (list (>> + 1) (>> + 2)) '(3 5)) '(4 7))
      (check equal? (liftA2 + (pure 1) (pure 2)) '(3))
      (check equal? (*> (pure 1) (pure 2)) '(2))))

  (test-case "list-monad"
    (with-instance list-monad
      (check equal? (>>= '(1 2 3) (.. return add1)) '(2 3 4))
      (check equal? (=<< (.. return add1) '(3 2 1)) '(4 3 2))
      (check equal? (>>M (return 1) (return 2)) '(2))
      (define (neighbors x dx) (list (- x dx) x (+ x dx)))
      (check equal?
             (do (x) <- (neighbors 0 10)
                 (y) <- (neighbors x 1)
                 (return y))
             '(-11 -10 -9 -1 0 1 9 10 11)))))
