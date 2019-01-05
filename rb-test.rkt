#lang algebraic/racket/base

(define f-power
  (function*
    [(0 _) 1]
    [(n x) (* x (f-power (- n 1) x))]))

(require racket/function)

(define f-power2 (curryr f-power 2))

(map f-power2 '(0 1 2 3 4 5 6))

(require (for-syntax algebraic/racket/macro))

;; (define-syntax m-power
;;   (macro*
;;    [(0 _) 1]
;;    [(1 x) x]
;;    [(n x) '#,(macro-expand #`(* x (m-power #,(- (var n) 1) x)))]))

(define-syntax m-power
  (macro*
    [(0 _) 1]
    [(1 x) x]
    [(n x) (* x (m-power #,(- (var n) 1) x))]))

;; (define-syntax m-power5 (μ y '#,(macro-expand #'(m-power 5 y))))

(define-syntax m-power5 (μ y (m-power 5 y)))

(m-power5 2)
