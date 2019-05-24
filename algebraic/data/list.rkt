#lang algebraic/racket/base

(require algebraic/class/applicative
         algebraic/class/functor
         algebraic/class/monoid
         algebraic/class/monad
         algebraic/class/semigroup)

(provide (all-defined-out))

(define-syntax ListSemigroup
  (instance Semigroup
    [<> ++]
    [stimes stimesList]))

;; stimesList :: Integral b => b -> [a] -> [a]
(define-syntax stimesList
  (class-helper
   (function*
     [(n _) #:if (< n 0) (error "stimes: list, negative multiplier")]
     [(0 _) null]
     [(n x) (letrec ([rep (function [0 null] [i (++ x (rep (- i 1)))])])
              (rep n))])))

(define-syntax ListMonoid
  (instance Monoid
    extends (ListSemigroup)
    [mempty null]
    [mconcat ++]))

(define-syntax ListFunctor (instance Functor [fmap map]))

(define-syntax ListApplicative
  (instance Applicative
    extends (ListFunctor)
    [pure list]
    [<*> (λ (fs xs) (map (λ (f x) (f x)) fs xs))]
    [liftA2 (λ (f xs ys) (map (λ (x y) (f x y)) xs ys))]
    [*> (λ (xs ys) (begin xs ys))]))

(define-syntax ListMonad
  (instance Monad
    extends (ListApplicative)
    [>>= (λ (xs f) ($ ++ (map f xs)))]
    [return (<< :: null)]
    [>>M *>]
    [fail (const null)]))

;;; -----------------------------------------------------------------------------

(module+ test
  (require rackunit)

  (test-case "ListSemigroup"
    (with-instance ListSemigroup
      (check equal? (<> '(1 2) '(3 4)) '(1 2 3 4))))

  (test-case "ListMonoid"
    (with-instance ListMonoid
      (check equal? (mconcat '(1 2) '(3 4) '(5 6)) '(1 2 3 4 5 6))))

  (test-case "ListFunctor"
    (with-instance ListFunctor
      (check equal? (fmap add1 '(1 2 3)) '(2 3 4))))

  (test-case "ListApplicative"
    (with-instance ListApplicative
      (check equal? (pure 1) '(1))
      (check equal? (<*> (pure add1) (pure 2)) '(3))
      (check equal? (<*> (list (>> + 1) (>> + 2)) '(3 5)) '(4 7))
      (check equal? (liftA2 + (pure 1) (pure 2)) '(3))
      (check equal? (*> (pure 1) (pure 2)) '(2))))

  (test-case "ListMonad"
    (with-instance ListMonad
      (check equal? (>>= '(1 2 3) (.. return add1)) '(2 3 4))
      (check equal? (=<< (.. return add1) '(3 2 1)) '(4 3 2))
      (check equal? (>>M (return 1) (return 2)) '(2))
      (define (neighbors x dx) (list (- x dx) x (+ x dx)))
      (check equal?
             (do (x) <- (neighbors 0 10)
                 (y) <- (neighbors x 1)
                 (return y))
             '(-11 -10 -9 -1 0 1 9 10 11)))))
