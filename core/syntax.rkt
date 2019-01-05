#lang racket/base

(require racket/hash
         racket/match
         racket/set)

(provide (all-defined-out))

;;; Names

(define (lcfirst-symbol? s)
  (and (symbol? s) (char-lower-case? (string-ref (symbol->string s) 0))))

(define (ucfirst-symbol? s)
  (and (symbol? s) (char-upper-case? (string-ref (symbol->string s) 0))))

;;; Terms

(struct Term () #:transparent)
(struct TApp Term (t1 t2) #:transparent)
(struct TSeq Term (t1 t2) #:transparent)
(struct TMac Term (p1 t2) #:transparent)
(struct TFun Term (p1 t2) #:transparent)
(struct TVar Term (x1) #:transparent)
(struct TCon Term (δ1) #:transparent)
(struct TNul Term () #:transparent)

(define (macro? t)
  (or (TMac? t)
      (and (TSeq? t)
           (TMac? (TSeq-t1 t))
           (macro? (TSeq-t2 t)))))

(define (function? t)
  (or (TFun? t)
      (and (TSeq? t)
           (TFun? (TSeq-t1 t))
           (function? (TSeq-t2 t)))))


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
  (or (TNul? t) (function? t) (macro? t) (data? t)))

;;; Patterns

(struct Patt () #:transparent)
(struct PApp Patt (p1 p2) #:transparent)
(struct PSeq Patt (p1 p2) #:transparent)
(struct PVar Patt (x1) #:transparent)
(struct PCon Patt (δ1) #:transparent)
(struct PWil Patt () #:transparent)
(struct PNul Patt () #:transparent)

(define vars
  (match-lambda
    [(PApp p1 p2) (set-union (vars p1) (vars p2))]
    [(PSeq p1 p2) (set-union (vars p1) (vars p2))]
    [(PVar x) (seteq x)]
    [(? PWil?) (seteq)]
    [(? PCon?) (seteq)]
    [(? PNul?) (seteq)]))

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
    [(? TNul?) t]))

(define (α-rename-patt x y p)
  (match p
    [(PApp p1 p2) (PApp (α-rename-patt x y p1) (α-rename-patt x y p2))]
    [(PSeq p1 p2) (PSeq (α-rename-patt x y p1) (α-rename-patt x y p2))]
    [(PVar x1) (if (equal? x1 x) (PVar y) p)]
    [(? PWil?) p]
    [(? PCon?) p]
    [(? PNul?) p]))

(define genvar (compose string->uninterned-symbol symbol->string))

(define (subst σ t [mask (seteq)])
  (match t
    [(TApp t1 t2) (TApp (subst σ t1 mask) (subst σ t2 mask))]
    [(TSeq t1 t2) (TSeq (subst σ t1 mask) (subst σ t2 mask))]
    [(TMac p1 t2) (TMac p1 (subst σ t2 (set-union mask (vars p1))))]
    [(TFun p1 t2) (TFun p1 (subst σ t2 (set-union mask (vars p1))))]
    [(TVar x1) (if (set-member? mask x1) t (hash-ref σ x1))]
    [(? TCon?) t]
    [(? TNul?) t]))

;;; Pattern Matching

(define ×
  (match-lambda**
   [((PApp p1 p2) (TApp t1 t2)) (let ([σ1 (× p1 t1)]
                                      [σ2 (× p2 t2)])
                                  (and σ1 σ2 (hash-union σ1 σ2)))]
   [((PSeq p1 p2) (TSeq t1 t2)) (let ([σ1 (× p1 t1)]
                                      [σ2 (× p2 t2)])
                                  (and σ1 σ2 (hash-union σ1 σ2)))]
   [((PCon δ1) (TCon δ2)) #:when (eq? δ1 δ2) (make-immutable-hasheq)]
   [((PVar x1) t2) (make-immutable-hasheq `([,x1 . ,t2]))]
   [((PWil) _) (make-immutable-hasheq)]
   [((PNul) (TNul)) (make-immutable-hasheq)]
   [(_ _) #f]))
