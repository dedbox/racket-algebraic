#lang algebraic/racket/base

(require algebraic/class/applicative
         algebraic/class/functor
         algebraic/class/monad
         (for-syntax algebraic/syntax))

(provide (all-defined-out))

;;; ----------------------------------------------------------------------------
;;; Data

(data Truthy (Fail))

(define truthy (φ x (or x Fail)))

;;; ----------------------------------------------------------------------------
;;; Instances

(define-syntax TruthyMonad
  (instance Monad
    [>>= (λ (x~ f) (λ () (let ([x (x~)]) (if (Fail? x) x ((f x))))))]
    [return thunk<-]))

(define-syntax TruthyFunctor
  (instance Functor
    extends (TruthyMonad)
    [fmap (λ (f x~) (>>= x~ (φ x (return (f x)))))]))

(define-syntax TruthyApplicative
  (instance Applicative
    extends (TruthyFunctor)
    [pure return]
    [<*> (λ (f~ x~)
           (do~ (f) <- f~
                (x) <- x~
                (return (f x))))]))

;;; ----------------------------------------------------------------------------
;;; Derived Forms

(splicing-with-instance TruthyMonad
  (define-syntax truthy-do
    (macro*
      #:datum-literals (let let-values <-)

      [(formals <- expr block ...)
       (lazy-do formals <- (truthy expr) (truthy-do block ...))]

      [(let patt expr block ...)
       (lazy-do let patt expr (truthy-do block ...))]

      [(let-values formals expr block ...)
       (lazy-do let-values patt expr (truthy-do block ...))]

      [(expr block ...+)
       (lazy-do (truthy expr) (truthy-do block ...))]

      [(expr)
       (lazy-do (truthy expr))]))

  (define-syntax truthy-and
    (macro*
      [(a ...+) ((do~ (λ () a) ...))]
      [() #t]))

  (define-syntax truthy-or
    (macro*
      [(a b ...) (let ([a* a]) (if (Fail? a*) (truthy-or b ...) a*))]
      [() Fail]))

  (define truthy-not (function [Fail #t] [_ Fail])))

;;; ----------------------------------------------------------------------------

(module+ test
  (require rackunit)

  (test-case "TruthyMonad"
    (with-instance TruthyMonad
      (check = ((return 0)) 0)
      (check = ((>>= (return 1) return)) 1)
      (check = ((>>M (return 2) (return 3))) 3)
      (check equal? ((>>= (λ () Fail) return)) Fail)
      (check equal? ((>>M (λ () Fail) (return 4))) Fail)
      (check equal? ((>>M (return 5) (λ () Fail))) Fail)
      (check equal? ((>>M (λ () Fail) (λ () Fail))) Fail)
      (check = ((do~ (f) <- (return add1)
                     (x) <- (return 2)
                     (return (f x))))
             3)))

  (test-case "TruthyFunctor"
    (with-instance TruthyFunctor
      (check = ((fmap add1 (λ () 2))) 3)
      (check equal? ((fmap add1 (λ () Fail))) Fail)))

  (test-case "TruthyApplicative"
    (with-instance TruthyApplicative
      (check = ((pure 0)) 0)
      (check = ((<*> (pure add1) (pure 1))) 2)
      (check = ((liftA2 + (pure 3) (pure 4))) 7)
      (check equal? ((<*> (λ () Fail) (pure 8))) Fail)
      (check equal? ((<*> (pure 9) (λ () Fail))) Fail)
      (check equal? ((<*> (λ () Fail) (λ () Fail))) Fail)
      (check equal? ((liftA2 + (λ () Fail) (pure 10))) Fail)
      (check equal? ((liftA2 + (pure 11) (λ () Fail))) Fail)
      (check equal? ((liftA2 + (λ () Fail) (λ () Fail))) Fail)))

  (test-case "agents example - do~"
    (splicing-with-instance TruthyApplicative
      (define agents (make-hash))
      (define next-id 1)
      (define (start-agent address)
        ((do~ (λ () (truthy (not (hash-has-key? agents address))))
              (λ () (truthy (hash-set! agents address next-id)))
              (λ () (truthy (begin0 next-id (set! next-id (add1 next-id))))))))
      (check = (start-agent 'A) 1)
      (check = (start-agent 'B) 2)
      (check = (start-agent 'C) 3)
      (check equal? (start-agent 'A) Fail)
      (check equal? (start-agent 'B) Fail)
      (check equal? (start-agent 'C) Fail)
      (check = next-id 4)))

  (test-case "agents example - lazy-do"
    (splicing-with-instance TruthyApplicative
      (define agents (make-hash))
      (define next-id 1)
      (define (start-agent address)
        (lazy-do (truthy (not (hash-has-key? agents address)))
                 (truthy (hash-set! agents address next-id))
                 (truthy (begin0 next-id (set! next-id (add1 next-id))))))
      (check = (start-agent 'A) 1)
      (check = (start-agent 'B) 2)
      (check = (start-agent 'C) 3)
      (check equal? (start-agent 'A) Fail)
      (check equal? (start-agent 'B) Fail)
      (check equal? (start-agent 'C) Fail)
      (check = next-id 4)))

  (test-case "agents example - truthy-do"
    (splicing-with-instance TruthyApplicative
      (define agents (make-hash))
      (define next-id 1)
      (define (start-agent address)
        (truthy-do (not (hash-has-key? agents address))
                   (hash-set! agents address next-id)
                   (begin0 next-id (set! next-id (add1 next-id)))))
      (check = (start-agent 'A) 1)
      (check = (start-agent 'B) 2)
      (check = (start-agent 'C) 3)
      (check equal? (start-agent 'A) Fail)
      (check equal? (start-agent 'B) Fail)
      (check equal? (start-agent 'C) Fail)
      (check = next-id 4)))

  (test-case "agents example - truthy-*"
    (splicing-with-instance TruthyApplicative
      (define agents (make-hash))
      (define next-id 1)
      (define (start-agent address)
        (truthy-and (truthy-not (truthy (hash-has-key? agents address)))
                    (hash-set! agents address next-id)
                    (begin0 next-id (set! next-id (add1 next-id)))))
      (check = (start-agent 'A) 1)
      (check = (start-agent 'B) 2)
      (check = (start-agent 'C) 3)
      (check equal? (start-agent 'A) Fail)
      (check equal? (start-agent 'B) Fail)
      (check equal? (start-agent 'C) Fail)
      (check = next-id 4))))
