#lang algebraic/racket/base

(require (for-syntax algebraic/racket/macro)
         algebraic/interpreter
         racket/contract/base
         racket/hash
         racket/set)

(provide algebraic)

(define-syntax algebraic
  (μ t (show (interpret (parse 't)))))

;;; ----------------------------------------------------------------------------
;;; Abstract Syntax

(data Term (TApp TSeq TFun TMac TVar TCon TUni))

(data Patt (PApp PSeq PWil PVar PCon PUni))

(define-syntax define-uniform-seq-pred
  (μ* (name? δ)
    (define name?
      (function
        [(TSeq (δ . _) t) (name? t)]
        [(δ . _) #t]
        [_ #f]))))

(define-uniform-seq-pred function? TFun)
(define-uniform-seq-pred macro? TMac)

(define data?
  (function
    [(TApp (TCon _) t) (instance? t)]
    [(TCon _) #t]
    [_ #f]))

(define instance?
  (function
    [(TSeq t1 t2) (and (value? t1) (instance? t2))]
    [t #:if (value? t) #t]
    [_ #f]))

(define (value? t)
  (or (eq? TUni t) ((or/c function? macro? data?) t)))

;;; ----------------------------------------------------------------------------
;;; Concrete Syntax

(define (parse t)
  (define term
    (function
      [(  t1 t2) (TApp (term t1) (term t2))]
      [($ t1 t2) (TSeq (term t1) (term t2))]
      [`(φ ,p1 ,t2) (apply-values TFun (α-rename-clause (patt p1) (term t2)))]
      [`(μ ,p1 ,t2) (apply-values TMac (α-rename-clause (patt p1) (term t2)))]
      [`,x #:if (char-lower-case? (first-char x)) (TVar x)]
      [`,x #:if (char-upper-case? (first-char x)) (TCon x)]
      [◊ TUni]))
  (define patt
    (function
      [(  p1 p2) (PApp (patt p1) (patt p2))]
      [($ p1 p2) (PSeq (patt p1) (patt p2))]
      [`,x #:if (char-lower-case? (first-char x)) (PVar x)]
      [`,x #:if (char-upper-case? (first-char x)) (PCon x)]
      ['_ PWil]
      [ ◊ PUni]))
  (term t))

(define (first-char s)
  (string-ref (symbol->string s) 0))

(define (α-rename-clause p t)
  (define p-vars (set->list (vars p)))
  (let loop ([xs p-vars]
             [ys (map genvar p-vars)]
             [p* p]
             [t* t])
    (if (null? xs)
        (values p* t*)
        (let ([p** ((α-rename-patt (car xs) (car ys)) p*)]
              [t** ((α-rename-term (car xs) (car ys)) t*)])
          (loop (cdr xs) (cdr ys) p** t**)))))

(define (α-rename-term x y)
  (function
    [(TApp t1 t2) (TApp ((α-rename-term x y) t1) ((α-rename-term x y) t2))]
    [(TSeq t1 t2) (TSeq ((α-rename-term x y) t1) ((α-rename-term x y) t2))]
    [(TFun p1 t2) (TFun p1 ((α-rename-term x y) t2))]
    [(TMac p1 t2) (TMac p1 ((α-rename-term x y) t2))]
    [(TVar x1) (TVar (if (eq? x1 x) y x1))]
    [(TCon δ1) (TCon δ1)]
    [TUni TUni]))

(define (α-rename-patt x y)
  (function
    [(PApp p1 p2) (PApp ((α-rename-patt x y) p1) ((α-rename-patt x y) p2))]
    [(PSeq p1 p2) (PSeq ((α-rename-patt x y) p1) ((α-rename-patt x y) p2))]
    [(PVar x1) (PVar (if (eq? x1 x) y x1))]
    [(PCon δ1) (PCon δ1)]
    [PWil PWil]
    [PUni PUni]))

(define vars
  (function
    [(PApp p1 p2) (set-union (vars p1) (vars p2))]
    [(PSeq p1 p2) (set-union (vars p1) (vars p2))]
    [(PVar x1) (seteq x1)]
    [(PCon _) (seteq)]
    [PWil (seteq)]
    [PUni (seteq)]))

(define (show t)
  (define term
    (function
      [(TApp t1 t2) `(  ,(term t1) ,(term t2))]
      [(TSeq t1 t2) `($ ,(term t1) ,(term t2))]
      [(TMac p1 t2) `(μ ,(patt p1) ,(term t2))]
      [(TFun p1 t2) `(φ ,(patt p1) ,(term t2))]
      [(TVar x1) x1]
      [(TCon δ1) δ1]
      [TUni '◊]))
  (define patt
    (function
      [(PApp p1 p2) `(  ,(patt p1) ,(patt p2))]
      [(PSeq p1 p2) `($ ,(patt p1) ,(patt p2))]
      [(PVar x1) x1]
      [(PCon δ1) δ1]
      [PWil '_]
      [PUni '◊]))
  (term t))

;;; ----------------------------------------------------------------------------
;;; Semantics

(define interpret
  (function
    [v #:if (value? v) v]
    [t
     ;; (writeln (show t))
     (interpret
      (or (step t)
          (error 'interpret "stuck at ~v" (show t))))]))

(define step
  (φ t (ormap (φ f (f t)) (list seq1 seq2 app1 appM app2 appF red))))

(define-steps
  [seq1 (TSeq t1 t2) (let ([t1* (step t1)]) (and t1* (TSeq t1* t2 )))]
  [seq2 (TSeq v1 t2) (let ([t2* (step t2)]) (and t2* (TSeq v1  t2*)))]
  [app1 (TApp t1 t2) (let ([t1* (step t1)]) (and t1* (TApp t1* t2 )))]
  [app2 (TApp v1 t2) (let ([t2* (step t2)]) (and t2* (TApp v1  t2*)))]
  [appF (TApp (TFun p11 t12) v2) (let ([σ (× p11 v2)]) (and σ (subst σ t12)))]
  [appM (TApp (TMac p11 t12) t2) (let ([σ (× p11 t2)]) (and σ (subst σ t12)))]
  [red  (TApp (TSeq v11 v12) t2) (or (step (TApp v11 t2)) (TApp v12 t2))])

;;; ----------------------------------------------------------------------------
;;; Pattern Matching

(define genvar (φ x (string->uninterned-symbol (symbol->string x))))

(define subst
  ;; normal function def with default ``mask'' arg works better here
  (function*
    [(σ t) (subst σ t (seteq))]
    [(σ t mask)
     ((function
        [(TApp t1 t2) (TApp (subst σ t1 mask) (subst σ t2 mask))]
        [(TSeq t1 t2) (TSeq (subst σ t1 mask) (subst σ t2 mask))]
        [(TFun p1 t2) (TFun p1 (subst σ t2 (set-union mask (vars p1))))]
        [(TMac p1 t2) (TMac p1 (subst σ t2 (set-union mask (vars p1))))]
        [(TVar x1) (if (set-member? mask x1) t (hash-ref σ x1))]
        [(TCon _) t]
        [TUni t])
      t)]))

(define ×
  (function*
    [((PApp p1 p2) (TApp t1 t2)) (let ([σ1 (× p1 t1)]
                                       [σ2 (× p2 t2)])
                                   (and σ1 σ2 (hash-union σ1 σ2)))]
    [((PSeq p1 p2) (TSeq t1 t2)) (let ([σ1 (× p1 t1)]
                                       [σ2 (× p2 t2)])
                                   (and σ1 σ2 (hash-union σ1 σ2)))]
    [((PCon δ) (TCon δ)) (make-immutable-hasheq)]
    [((PVar x1) t2) (make-immutable-hasheq `([,x1 . ,t2]))]
    [(PWil _) (make-immutable-hasheq)]
    [(PUni TUni) (make-immutable-hasheq)]
    [(_ _) #f]))

;;; ----------------------------------------------------------------------------

(module+ test
  (require rackunit)

  ;; makes equal? work again
  (define scrub
    (function
      [(TApp t1 t2) (TApp (scrub t1) (scrub t2))]
      [(TSeq t1 t2) (TSeq (scrub t1) (scrub t2))]
      [(TFun p1 t2) (TFun (scrub p1) (scrub t2))]
      [(TMac p1 t2) (TMac (scrub p1) (scrub t2))]
      [(TVar x1) (TVar (string->symbol (symbol->string x1)))]
      [(TCon δ1) (TCon δ1)]
      [TUni TUni]
      [(PApp p1 p2) (PApp (scrub p1) (scrub p2))]
      [(PSeq p1 p2) (PSeq (scrub p1) (scrub p2))]
      [(PVar x1) (PVar (string->symbol (symbol->string x1)))]
      [(PCon δ1) (PCon δ1)]
      [PWil PWil]
      [PUni PUni]))

  ;; Syntax

  (define-syntax-rule (check-parse-terms [sexp ast] ...)
    (begin (check equal? (scrub (parse 'sexp)) ast) ...))

  (define-syntax-rule (check-parse-patterns [sexp ast] ...)
    (begin (check equal? (scrub (parse '(φ sexp z))) (TFun ast (TVar 'z))) ...))

  (test-case "parse term"
    (check-parse-terms
     [(  x y) (TApp (TVar 'x) (TVar 'y))]
     [($ x y) (TSeq (TVar 'x) (TVar 'y))]
     [(φ x y) (TFun (PVar 'x) (TVar 'y))]
     [(μ x y) (TMac (PVar 'x) (TVar 'y))]
     [x (TVar 'x)]
     [X (TCon 'X)]
     [◊ TUni]))

  (test-case "parse pattern"
    (check-parse-patterns
     [(  x y) (PApp (PVar 'x) (PVar 'y))]
     [($ x y) (PSeq (PVar 'x) (PVar 'y))]
     [x (PVar 'x)]
     [X (PCon 'X)]
     [_ PWil]
     [◊ PUni]))

  (define-syntax-rule (check-show-terms [ast sexp] ...)
    (begin (check equal? (show ast) 'sexp) ...))

  (define-syntax-rule (check-show-patterns [ast sexp] ...)
    (begin (check equal? (show (TFun ast (TVar 'z))) '(φ sexp z)) ...))

  (test-case "show term"
    (check-show-terms
     [(TApp (TVar 'x) (TVar 'y)) (  x y)]
     [(TSeq (TVar 'x) (TVar 'y)) ($ x y)]
     [(TFun (PVar 'x) (TVar 'y)) (φ x y)]
     [(TMac (PVar 'x) (TVar 'y)) (μ x y)]
     [(TVar 'x) x]
     [(TCon 'X) X]
     [TUni ◊]))

  (test-case "show pattern"
    (check-show-patterns
     [(PApp (PVar 'x) (PVar 'y)) (  x y)]
     [(PSeq (PVar 'x) (PVar 'y)) ($ x y)]
     [(PVar 'x) x]
     [(PCon 'X) X]
     [PWil _]
     [PUni ◊]))

  (define-simple-check (check-step-term f-step t sexp)
    (equal? sexp (show (scrub (f-step (parse t))))))

  (define-syntax-rule (check-step-terms [step t #:~> sexp] ...)
    (begin (check-step-term step 't 'sexp) ...))

  (test-case "step"
    (check-step-terms
     [seq1 ($ ((φ x x) (φ y y)) (φ z z)) #:~> ($ (φ y y) (φ z z))]
     [seq2 ($ (φ x x) ((φ y y) (φ z z))) #:~> ($ (φ x x) (φ z z))]
     [app1 (((φ x x) (φ y y)) (φ z z)) #:~> ((φ y y) (φ z z))]
     [app2 ((φ x x) ((φ y y) (φ z z))) #:~> ((φ x x) (φ z z))]
     [appM ((μ x x) (μ y y)) #:~> (μ y y)]
     [appF ((φ x x) (φ y y)) #:~> (φ y y)]
     [red (($ (φ x x) (φ y y)) (φ z z)) #:~> (φ z z)]
     [red (($ (μ x x) (μ y y)) (μ z z)) #:~> (μ z z)]))

  (define-simple-check (check-pattern-match p t)
    (× ((φ (TFun p* _) p*) (parse `(φ ,p ◊)))
       (parse t)))

  (define-syntax-rule (check-pattern-matches [p t] ...)
    (begin (check-pattern-match 'p 't) ...))

  (test-case "pattern match"
    (check-pattern-matches
     [_ ◊]
     [_ (φ y y)]
     [_ ($ ◊ ◊)]
     [◊ ◊]
     [x ◊]
     [x (φ y y)]
     [x ($ ◊ ◊)]
     [X X]
     [(_ _) (◊ ◊)]
     [($ _ _) ($ (φ x x) (φ y y))]
     [($ x y) ($ (φ z z) (φ w w))]))

  (test-case "2 + 1 = 3"
    (check equal? (algebraic
                   ((φ fix
                      ((φ add
                         (add ((Succ (Succ Zero)) (Succ Zero))))
                       (fix (φ add ($ (μ (a Zero) a)
                                      (μ (a (Succ b)) (Succ (add (a b)))))))))
                    (φ f ((φ x (f (φ y ((x x) y))))
                          (φ x (f (φ y ((x x) y))))))))
           '(Succ (Succ (Succ Zero)))))

  (test-case "2 * 3 = 6"
    (check
     equal? (algebraic
             ((φ fix
                ((φ add
                   ((φ mul
                      (mul ((Succ (Succ Zero)) (Succ (Succ (Succ Zero))))))
                    (fix (φ mul ($ (φ (a Zero) Zero)
                                   (φ (a (Succ b)) (add (a (mul (a b))))))))))
                 (fix (φ add ($ (μ (a Zero) a)
                                (μ (a (Succ b)) (Succ (add (a b)))))))))
              (φ f ((φ x (f (φ y ((x x) y))))
                    (φ x (f (φ y ((x x) y))))))))
     '(Succ (Succ (Succ (Succ (Succ (Succ Zero))))))))

  (test-case "booleans"
    (check
     equal? (algebraic
             ((φ fix
                ((φ not
                   ((φ and
                      ((φ or
                         ((φ xor
                            (or ((not True) (and ((xor (True True)) True)))))
                          (fix (φ xor (μ (a b) (($ (φ False b) (φ x (and ((not b) x)))) a))))))
                       (fix (φ or (μ (a b) (($ (φ False b) (φ x x)) a))))))
                    (fix (φ and (μ (a b) (($ (φ False False) (φ _ b)) a))))))
                 (fix (φ not ($ (φ False True) (φ _ False))))))
              (φ f ((φ x (f (φ y ((x x) y))))
                    (φ x (f (φ y ((x x) y))))))))
     'False))

  (test-case "list 1 2 3 ◊"
    (check
     equal? (algebraic
             ((φ fix
                ((φ list
                   (list ((Succ Zero)
                          ((Succ (Succ Zero)) ((Succ (Succ (Succ Zero))) ◊)))))
                 (fix (φ list ($ (μ (x ◊) (Cons ($ x Nil)))
                                 (μ (x xs) (Cons ($ x (list xs)))))))))
              (φ f ((φ x (f (φ y ((x x) y))))
                    (φ x (f (φ y ((x x) y))))))))
     '(Cons ($ (Succ Zero)
               (Cons ($ (Succ (Succ Zero))
                        (Cons ($ (Succ (Succ (Succ Zero))) Nil))))))))

  (test-case "reverse list 1 2 3 ◊"
    (check
     equal? (algebraic
             ((φ fix
                ((φ rev
                   ((φ reverse
                      (reverse
                       (Cons ($ (Succ Zero)
                                (Cons ($ (Succ (Succ Zero))
                                         (Cons ($ (Succ (Succ (Succ Zero)))
                                                  Nil))))))))
                    (fix (φ reverse (φ xs (rev (xs Nil)))))))
                 (fix
                  (φ rev
                    ($ (φ (Nil a) a)
                       (φ ((Cons ($ y ys)) a) (rev (ys (Cons ($ y a))))))))))
              (φ f ((φ x (f (φ y ((x x) y))))
                    (φ x (f (φ y ((x x) y))))))))
     '(Cons ($ (Succ (Succ (Succ Zero)))
               (Cons ($ (Succ (Succ Zero))
                        (Cons ($ (Succ Zero) Nil))))))))

  (test-case "append (list 1 2 ◊) list 3 4 ◊"
    (check
     equal? (algebraic
             ((φ fix
                ((φ append
                   (append
                    ((Cons ($ (Succ Zero) (Cons ($ (Succ (Succ Zero)) Nil))))
                     (Cons ($ (Succ (Succ (Succ Zero)))
                              (Cons ($ (Succ (Succ (Succ (Succ Zero))))
                                       Nil)))))))
                 (fix
                  (φ append
                    ($ (φ (Nil ys) ys)
                       (φ ((Cons ($ x xs)) ys) (Cons ($ x (append (xs ys))))))))))
              (φ f ((φ x (f (φ y ((x x) y))))
                    (φ x (f (φ y ((x x) y))))))))
     '(Cons ($ (Succ Zero)
               (Cons ($ (Succ (Succ Zero))
                        (Cons ($ (Succ (Succ (Succ Zero)))
                                 (Cons ($ (Succ (Succ (Succ (Succ Zero))))
                                          Nil))))))))))

  (test-case "map Succ list 3 2 1 ◊"
    (check
     equal? (algebraic
             ((φ fix
                ((φ map
                   (map (Succ
                         (Cons ($ (Succ (Succ (Succ Zero)))
                                  (Cons ($ (Succ (Succ Zero))
                                           (Cons ($ (Succ Zero) Nil)))))))))
                 (fix
                  (φ map
                    ($ (φ (_ Nil) Nil)
                       (φ (f (Cons ($ x xs))) (Cons ($ (f x) (map (f xs))))))))))
              (φ f ((φ x (f (φ y ((x x) y))))
                    (φ x (f (φ y ((x x) y))))))))
     '(Cons ($ (Succ (Succ (Succ (Succ Zero))))
               (Cons ($ (Succ (Succ (Succ Zero)))
                        (Cons ($ (Succ (Succ Zero)) Nil)))))))))
