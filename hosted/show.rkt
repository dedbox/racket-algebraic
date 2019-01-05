#lang racket/base

(require algebraic/hosted/syntax
         racket/match)

(provide (all-defined-out))

;;; Terms

(define show
  (match-lambda
    [(TSeq (? TMac? t1) _) `(mac ,@(show-mac t1))]
    [(TSeq (? TFun? t1) _) `(fun ,@(show-fun t1))]
    [(? TSeq? t1) `($ ,@(TSeq->list t1))]
    [(? TApp? t1) (TApp->list t1)]
    [(TMac p1 t2) `(μ ,(patt p1) ,@(TApp->list t2))]
    [(TFun p1 t2) `(φ ,(patt p1) ,@(TApp->list t2))]
    [(TPro f1 _) f1]
    [(TVar x1) x1]
    [(TCon δ1) δ1]
    [(TLit ℓ1) ℓ1]
    [(TNul) '◊]))

(define show-mac
  (match-lambda
    [(TMac p1 t2) `([,(patt p1) ,@(TApp->list t2)])]
    [(TSeq (? TMac? t1) t2) (append (show-mac t1) (show-mac t2))]))

(define show-fun
  (match-lambda
    [(TFun p1 t2) `([,(patt p1) ,@(TApp->list t2)])]
    [(TSeq (? TFun? t1) t2) (append (show-fun t1) (show-fun t2))]))

;;; Patterns

(define patt
  (match-lambda
    [(PGua p1 t2) `(,(patt p1) if ,(show t2))]
    [(? PApp? p1) (PApp->list p1)]
    [(? PSeq? p1) `($ ,@(PSeq->list p1))]
    [(? PWil?) '_]
    [(? PNul?) '◊]
    [(PVar x1) x1]
    [(PCon δ1) δ1]
    [(PLit ℓ1) ℓ1]
    [(PPro f1) f1]))

;;; Helpers

(define TApp->list
  (match-lambda
    [(TApp t1 t2) (cons (show t1) (TApp->list t2))]
    [t1 (list (show t1))]))

(define TSeq->list
  (match-lambda
    [(TSeq t1 t2) (cons (show t1) (TSeq->list t2))]
    [t1 (list (show t1))]))

(define PApp->list
  (match-lambda
    [(PApp p1 p2) (cons (patt p1) (PApp->list p2))]
    [p1 (list (patt p1))]))

(define PSeq->list
  (match-lambda
    [(PSeq p1 p2) (cons (patt p1) (PSeq->list p2))]
    [p1 (list (patt p1))]))
