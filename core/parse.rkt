#lang racket/base

(require algebraic/core/syntax
         racket/match)

(provide (all-defined-out))

;;; S-expressions -> ASTs

(define-syntax-rule (app f xs ...)
  (call-with-values (λ () xs ...) f))

(define (parse t)
  (define term
    (match-lambda
      [`(,t1 ,t2) (TApp (term t1) (term t2))]
      [`($ ,t1 ,t2) (TSeq (term t1) (term t2))]
      [`(μ ,p1 ,t2) (app TMac (α-rename (patt p1) (term t2)))]
      [`(φ ,p1 ,t2) (app TFun (α-rename (patt p1) (term t2)))]
      ['◊ (TNul)]
      [(? symbol? s) (symbol->term s)]))

  (define patt
    (match-lambda
      [`(,p1 ,p2) (PApp (patt p1) (patt p2))]
      [`($ ,p1 ,p2) (PSeq (patt p1) (patt p2))]
      ['_ (PWil)]
      ['◊ (PNul)]
      [(? symbol? s) (symbol->patt s)]))

  (term t))

(define symbol->term
  (match-lambda
    [(? lcfirst-symbol? x) (TVar x)]
    [(? ucfirst-symbol? δ) (TCon δ)]))

(define symbol->patt
  (match-lambda
    [(? lcfirst-symbol? x) (PVar x)]
    [(? ucfirst-symbol? δ) (PCon δ)]))
