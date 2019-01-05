#lang racket/base

(require (only-in algebraic/core/parse app)
         algebraic/hosted/parse
         algebraic/hosted/show
         algebraic/hosted/syntax
         racket/function
         racket/hash
         racket/match)

(provide (all-defined-out))

(define (interpret t)
  ;; (displayln (show t))
  (if (value? t)
      t
      (let ([t* (step t)])
        (if t*
            (interpret t*)
            (error (format "stuck at ~a" (show t)))))))

(define (step t)
  (or (seq1 t) (seq2 t) (app1 t) (appM t) (app2 t) (appF t) (mac t) (fun t)
      (pro t)))

(define-syntax-rule (define-rule name pattern target)
  (define (name t)
    (match t [pattern target] [_ #f])))

(define-rule seq1 (TSeq t1 t2)
  (let ([t1* (step t1)]) (and t1* (TSeq t1* t2))))

(define-rule seq2 (TSeq v1 t2)
  (let ([t2* (step t2)]) (and t2* (TSeq v1 t2*))))

(define-rule app1 (TApp t1 t2)
  (let ([t1* (step t1)]) (and t1* (TApp t1* t2))))

(define-rule appM (TApp (TMac p1 t2) t3)
  (let ([σ (× p1 t3)]) (and σ (subst σ t2))))

(define-rule app2 (TApp v1 t2)
  (let ([t2* (step t2)]) (and t2* (TApp v1 t2*))))

(define-rule appF (TApp (TFun p1 t2) v3)
  (let ([σ (× p1 v3)]) (and σ (subst σ t2))))

(define-rule mac (TApp (TSeq (TMac p1 t2) v3) t4)
  (or (step (TApp (TMac p1 t2) t4))
      (TApp v3 t4)))

(define-rule fun (TApp (TSeq (TFun p1 t2) v3) v4)
  (or (step (TApp (TFun p1 t2) v4))
      (TApp v3 v4)))

(define-rule pro (TApp (TPro _ proc) (? literal? ℓ2))
  (literals->term (app list (apply proc (term->literals ℓ2)))))

;;; Pattern Matching

(define ×
  (match-lambda**
   [((PApp p1 p2) (TApp t1 t2)) (let ([σ1 (× p1 t1)]
                                      [σ2 (× p2 t2)])
                                  (and σ1 σ2 (hash-union σ1 σ2)))]
   [((PSeq p1 p2) (TSeq t1 t2)) (let ([σ1 (× p1 t1)]
                                      [σ2 (× p2 t2)])
                                  (and σ1 σ2 (hash-union σ1 σ2)))]
   [((PGua p1 t2) t3) (let ([σ1 (× p1 t3)])
                        (and σ1 (not (equal? (interpret (subst σ1 t2)) (TLit #f)))
                             σ1))]
   [((PPro f1) (TPro f2 _)) #:when (eq? f1 f2) (make-immutable-hasheq)]
   [((PCon δ1) (TCon δ2)) #:when (eq? δ1 δ2) (make-immutable-hasheq)]
   [((PLit ℓ1) (TLit ℓ2)) #:when (equal? ℓ1 ℓ2) (make-immutable-hasheq)]
   [((PVar x1) t2) (make-immutable-hasheq `([,x1 . ,t2]))]
   [((PWil) _) (make-immutable-hasheq)]
   [((PNul) (TNul)) (make-immutable-hasheq)]
   [(_ _) #f]))
