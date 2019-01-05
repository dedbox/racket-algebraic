#lang racket/base

(require algebraic/core/syntax
         (only-in algebraic/core/parse app symbol->term symbol->patt)
         racket/match)

(provide (all-defined-out))

(define (parse t)
  (define term
    (match-lambda
      ['fix (term '(φ f
                     (φ x f φ y (x x) y)
                     (φ x f φ y (x x) y)))]
      [`(mac [,p . ,ts]) (term `(μ ,p ,@ts))]
      [`(fun [,p . ,ts]) (term `(φ ,p ,@ts))]
      [`(mac ,c . ,cs) (TSeq (term `(mac ,c)) (term `(mac ,@cs)))]
      [`(fun ,c . ,cs) (TSeq (term `(fun ,c)) (term `(fun ,@cs)))]
      [`(let () . ,body) (term body)]
      [`(let ([,p . ,ts] . ,cs) . ,body) (term `((φ ,p let ,cs ,@body) ,@ts))]
      [`(letrec () . ,body) (term body)]
      [`(letrec ([,p . ,ts] . ,cs) . ,body)
       (term `((φ ,p letrec ,cs ,@body) fix φ ,p ,@ts))]
      [`($ ,t1 ,t2 . ,ts) (TSeq (term t1) (term `($ ,t2 ,@ts)))]
      [`($ ,t1) (term t1)]
      [`(μ ,p1 . ,t2) (app TMac (α-rename (patt p1) (term t2)))]
      [`(φ ,p1 . ,t2) (app TFun (α-rename (patt p1) (term t2)))]
      [`(,t1 ,t2 . ,ts) (TApp (term t1) (term `(,t2 ,@ts)))]
      [`(,t1) (term t1)]
      ['◊ (TNul)]
      [(? symbol? s) (symbol->term s)]))

  (define patt
    (match-lambda
      [`($ ,p1) (patt p1)]
      [`($ ,p1 ,p2 . ,ps) (PSeq (patt p1) (patt `($ ,p2 ,@ps)))]
      [`(,p1 ,p2 . ,ps) (PApp (patt p1) (patt `(,p2 ,@ps)))]
      [`(,p1) (patt p1)]
      ['_ (PWil)]
      ['◊ (PNul)]
      [(? symbol? s) (symbol->patt s)]))

  (term t))
