#lang algebraic/racket/base

(require (for-syntax algebraic/racket/macro))

(provide (all-defined-out))

(define-syntax apply-values
  (μ* (f xs ...)
    (call-with-values (λ () xs ...) f)))

(define-syntax define-step
  (μ* (step:id pattern:fun-patt result)
    (define step (function [pattern result] [_ #f]))))

(define-syntax define-steps
  (μ* ([step:id pattern:fun-patt result] ...+)
    (begin (define-step step pattern result) ...)))

(define-syntax define-composite-step
  (μ* (step:id pattern:fun-patt t:id t*:id result)
    #:with t** #,(syntax-local-introduce #'t*)
    #:with r** #,(syntax-local-introduce #'result)
    (define-step step pattern (let ([t** (step t)]) (and t** r**)))))

(define-syntax define-composite-steps
  (μ* ([step pattern:fun-patt t:id t*:id result] ...+)
    (begin (define-composite-step step pattern t t* result) ...)))
