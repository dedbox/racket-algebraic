#lang racket/base

(module core-imports racket/base
  (require (prefix-in core: algebraic/core/syntax))
  (provide (all-from-out algebraic/core/syntax)))

(require (except-in algebraic/core/syntax
                    value? vars α-rename α-rename-term α-rename-patt subst)
         'core-imports
         racket/match
         racket/set)

(provide (all-from-out algebraic/core/syntax)
         (all-defined-out))

;;; Terms

(struct TLit Term (ℓ1) #:transparent)
(struct TPro Term (f1 proc) #:transparent)

(define (literal? t)
  (or (TLit? t)
      (and (TApp? t)
           (TLit? (TApp-t1 t))
           (literal? (TApp-t2 t)))))

(define (data? t)
  (or (TCon? t)
      (and (TApp? t)
           (TCon? (TApp-t1 t))
           (instance? (TApp-t2 t)))))

(define (instance? t)
  (or (value? t)
      (and (TSeq? t)
           (value? (TSeq-t1 t))
           (instance? (TSeq-t2 t)))))

(define (value? t)
  (or (core:value? t) (data? t) (literal? t)))

(define term->literals
  (match-lambda
    [(TLit ℓ1) (list ℓ1)]
    [(TApp (TLit ℓ1) t2) (cons ℓ1 (term->literals t2))]))

(define literals->term
  (match-lambda
    [(list ℓ1) (TLit ℓ1)]
    [(cons ℓ1 ℓs) (TApp (TLit ℓ1) (literals->term ℓs))]))

;;; Patterns

(struct PLit Patt (ℓ1) #:transparent)
(struct PPro Patt (f1) #:transparent)
(struct PGua Patt (p1 t2) #:transparent)

(define vars
  (match-lambda
    [(PApp p1 p2) (set-union (vars p1) (vars p2))]
    [(PSeq p1 p2) (set-union (vars p1) (vars p2))]
    [(PGua p1 _) (vars p1)]
    [(PVar x) (seteq x)]
    [(? PWil?) (seteq)]
    [(? PCon?) (seteq)]
    [(? PNul?) (seteq)]
    [(? PLit?) (seteq)]
    [(? PPro?) (seteq)]))

;;; Transformations

(define (α-rename p t)
  (define p-xs (set->list (vars p)))
  (let loop ([xs p-xs]
             [ys (map genvar p-xs)]
             [p* p]
             [t* t])
    (if (null? xs)
        (values p* t*)
        (let ([p** (α-rename-patt (car xs) (car ys) p*)]
              [t** (α-rename-term (car xs) (car ys) t*)])
          (loop (cdr xs) (cdr ys) p** t**)))))

(define (α-rename-term x y t)
  (match t
    [(TApp t1 t2) (TApp (α-rename-term x y t1) (α-rename-term x y t2))]
    [(TSeq t1 t2) (TSeq (α-rename-term x y t1) (α-rename-term x y t2))]
    [(TMac p1 t2) (TMac p1 (α-rename-term x y t2))]
    [(TFun p1 t2) (TFun p1 (α-rename-term x y t2))]
    [(TVar x1) (if (eq? x1 x) (TVar y) t)]
    [(? TCon?) t]
    [(? TNul?) t]
    [(? TLit?) t]
    [(? TPro?) t]))

(define (α-rename-patt x y p)
  (match p
    [(PApp p1 p2) (PApp (α-rename-patt x y p1) (α-rename-patt x y p2))]
    [(PSeq p1 p2) (PSeq (α-rename-patt x y p1) (α-rename-patt x y p2))]
    [(PGua p1 t2) (PGua (α-rename-patt x y p1) (α-rename-term x y t2))]
    [(PVar x1) (if (equal? x1 x) (PVar y) p)]
    [(? PWil?) p]
    [(? PCon?) p]
    [(? PNul?) p]
    [(? PLit?) p]
    [(? PPro?) p]))

(define (subst σ t [mask (seteq)])
  (match t
    [(TApp t1 t2) (TApp (subst σ t1 mask) (subst σ t2 mask))]
    [(TSeq t1 t2) (TSeq (subst σ t1 mask) (subst σ t2 mask))]
    [(TMac p1 t2) (TMac p1 (subst σ t2 (set-union mask (vars p1))))]
    [(TFun p1 t2) (TFun p1 (subst σ t2 (set-union mask (vars p1))))]
    [(TVar x1) (if (set-member? mask x1) t (hash-ref σ x1))]
    [(? TCon?) t]
    [(? TNul?) t]
    [(? TLit?) t]
    [(? TPro?) t]))
