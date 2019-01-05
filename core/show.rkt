#lang racket/base

(require algebraic/core/syntax
         racket/match)

(provide (all-defined-out))

;;; ASTs -> S-expressions

(define (show t)
  (define term
    (match-lambda
      [(TApp t1 t2) `(,(term t1) ,(term t2))]
      [(TSeq t1 t2) `($ ,(term t1) ,(term t2))]
      [(TMac p1 t2) `(μ ,(patt p1) ,(term t2))]
      [(TFun p1 t2) `(φ ,(patt p1) ,(term t2))]
      [(TVar x1) x1]
      [(TCon δ1) δ1]
      [(TNul) '◊]))

  (define patt
    (match-lambda
      [(PApp p1 p2) `(,(patt p1) ,(patt p2))]
      [(PSeq p1 p2) `($ ,(patt p1) ,(patt p2))]
      [(PVar x1) x1]
      [(PCon δ1) δ1]
      [(PWil) '_]
      [(PNul) '◊]))

  (term t))
