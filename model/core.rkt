#lang algebraic/racket/base

(require racket/contract/base
         racket/hash
         racket/pretty
         racket/set
         (for-syntax syntax/parse))

(provide #%app #%datum
         (rename-out [module-begin #%module-begin]
                     [top-interaction #%top-interaction])
         (all-defined-out)
         (for-syntax (all-defined-out)))

(define-syntax-rule (module-begin form ...)
  (#%plain-module-begin (pretty-write (algebraic form)) ...))

(define-syntax-rule (top-interaction . form)
  (#%top-interaction . (algebraic form)))

;;; ----------------------------------------------------------------------------
;;; Syntax

(data Term (TApp TSeq TFun TMac TVar TCon TUni)
      Patt (PApp PSeq PWil PVar PCon PUni))

;;; The Parser

(define (parse t)
  (define term
    (function
      [(  t1 t2) (TApp (term t1) (term t2))]
      [($ t1 t2) (TSeq (term t1) (term t2))]
      [('φ p1 t2) (values-> TFun (α-rename (patt p1) (term t2)))]
      [('μ p1 t2) (values-> TMac (α-rename (patt p1) (term t2)))]
      [x #:if (con-name? x) (TCon x)]
      [x #:if (var-name? x) (TVar x)]
      [◊ TUni]))
  (define patt
    (function
      [(  p1 p2) (PApp (patt p1) (patt p2))]
      [($ p1 p2) (PSeq (patt p1) (patt p2))]
      [x #:if (con-name? x) (PCon x)]
      [x #:if (var-name? x) (PVar x)]
      ['_ PWil]
      [◊ PUni]))
  (term t))

(define-syntax values->
  (μ* (f xs-expr)
    (call-with-values (λ () xs-expr) f)))

(define (con-name? x)
  (and (symbol? x) (char-upper-case? (first-char x))))

(define (var-name? x)
  (and (symbol? x) (char-lower-case? (first-char x))))

(define (first-char s)
  (string-ref (symbol->string s) 0))

;;; The Printer

(define (show t)
  (define term
    (function
      [(TApp t1 t2) `(  ,(term t1) ,(term t2))]
      [(TSeq t1 t2) `($ ,(term t1) ,(term t2))]
      [(TMac p1 t2) `(μ ,(patt p1) ,(term t2))]
      [(TFun p1 t2) `(φ ,(patt p1) ,(term t2))]
      [(TVar x1) (α-restore x1)]
      [(TCon δ1) δ1]
      [TUni '◊]))
  (define patt
    (function
      [(PApp p1 p2) `(  ,(patt p1) ,(patt p2))]
      [(PSeq p1 p2) `($ ,(patt p1) ,(patt p2))]
      [(PVar x1) (α-restore x1)]
      [(PCon δ1) δ1]
      [PWil '_]
      [PUni '◊]))
  (term t))

;;; Values

(define-syntax define-uniform-sequence-pred
  (μ* (name? kind?)
    (define name?
      (function
        [(TSeq t1 t2) #:if (kind? t1) (name? t2)]
        [t (kind? t)]))))

(define-uniform-sequence-pred fun? TFun?)
(define-uniform-sequence-pred mac? TMac?)
(define-uniform-sequence-pred dat? value?)

(define ins? (function [(TApp (TCon _) t) (dat? t)] [_ #f]))

(define value? (or/c fun? mac? TCon? ins? TUni?))

;;; ----------------------------------------------------------------------------
;;; Evaluation Semantics

(define-syntax algebraic
  (μ t (show (interpret (parse 't)))))

(define-syntax define-interpreter
  (μ* ((name:id t:id) step-expr ...+)
    (define name
      (function
        [v #:if (value? v) v]
        [t (name
            (or (begin step-expr ...)
                (error 'name "stuck at ~v" (show t))))]))))

(define-interpreter (interpret t)
  (step t))

(define-interpreter (trace t)
  (writeln (show t))
  (step t))

(define-syntax define-stepper
  (μ* (stepper-name:id
       (order:id ...+)
       [step-name:id pattern:fun-patt result] ...+)
    (begin
      (define (stepper-name t) (or (order t) ...))
      (define step-name (function [pattern result] [_ #f]))
      ...)))

(define-syntax sub-step
  (μ* (t*-expr:expr t*:id result:expr)
    (let ([t* t*-expr]) (and t* result))))

(define-syntax alt-step
  (μ* ((abs:expr val:expr) (alt:expr alt-val:expr))
    (or (step (TApp abs val)) (TApp alt alt-val))))

(define-stepper step
  (seq1 seq2 app1 appM redM app2 appF redF)
  [app1 (TApp t1 t2) (sub-step (step t1) t1* (TApp t1* t2 ))]
  [app2 (TApp v1 t2) (sub-step (step t2) t2* (TApp v1  t2*))]
  [seq1 (TSeq t1 t2) (sub-step (step t1) t1* (TSeq t1* t2 ))]
  [seq2 (TSeq v1 t2) (sub-step (step t2) t2* (TSeq v1  t2*))]
  [appF (TApp (TFun p11 t12) v2) (sub-step (× p11 v2) σ (subst σ t12))]
  [appM (TApp (TMac p11 t12) t2) (sub-step (× p11 t2) σ (subst σ t12))]
  [redF (TApp (TSeq (TFun x t) v12) v2) (alt-step ((TFun x t) v2) (v12 v2))]
  [redM (TApp (TSeq (TMac x t) v12) v2) (alt-step ((TMac x t) v2) (v12 v2))])

(define (subst σ t [mask (seteq)])
  ((function
     [(TApp t1 t2) (TApp (subst σ t1 mask) (subst σ t2 mask))]
     [(TSeq t1 t2) (TSeq (subst σ t1 mask) (subst σ t2 mask))]
     [(TFun p1 t2) (TFun p1 (subst σ t2 (set-union mask (vars p1))))]
     [(TMac p1 t2) (TMac p1 (subst σ t2 (set-union mask (vars p1))))]
     [(TVar x1) (if (set-member? mask x1) t (hash-ref σ x1))]
     [(TCon _) t]
     [TUni t])
   t))

(define vars
  (function
    [(PApp p1 p2) (set-union (vars p1) (vars p2))]
    [(PSeq p1 p2) (set-union (vars p1) (vars p2))]
    [(PVar x1) (seteq x1)]
    [(PCon _) (seteq)]
    [PWil (seteq)]
    [PUni (seteq)]))

;;; ----------------------------------------------------------------------------
;;; Pattern Matching Semantics

(define ×
  (function*
    [((PApp p11 p12) (TApp t21 t22))
     (let ([σ1 (× p11 t21)]
           [σ2 (× p12 t22)])
       (and σ1 σ2 (hash-union σ1 σ2)))]
    [((PSeq p11 p12) (TSeq t21 t22))
     (let ([σ1 (× p11 t21)]
           [σ2 (× p12 t22)])
       (and σ1 σ2 (hash-union σ1 σ2)))]
    [((PCon δ) (TCon δ)) (make-immutable-hasheq)]
    [((PVar x1) t2) (make-immutable-hasheq `([,x1 . ,t2]))]
    [(PWil _) (make-immutable-hasheq)]
    [(PUni TUni) (make-immutable-hasheq)]
    [(_ _) #f]))

;;; ----------------------------------------------------------------------------
;;; Pragmatics

(define (α-rename p t)
  (define (term x y)
    (function
      [(TApp t1 t2) (TApp ((term x y) t1) ((term x y) t2))]
      [(TSeq t1 t2) (TSeq ((term x y) t1) ((term x y) t2))]
      [(TFun p1 t2) (TFun p1 ((term x y) t2))]
      [(TMac p1 t2) (TMac p1 ((term x y) t2))]
      [(TVar x1) (TVar (if (equal? x1 x) y x1))]
      [(TCon δ1) (TCon δ1)]
      [TUni TUni]))
  (define (patt x y)
    (function
      [(PApp p1 p2) (PApp ((patt x y) p1) ((patt x y) p2))]
      [(PSeq p1 p2) (PSeq ((patt x y) p1) ((patt x y) p2))]
      [(PVar x1) (PVar (if (equal? x1 x) y x1))]
      [(PCon δ1) (PCon δ1)]
      [PWil PWil]
      [PUni PUni]))
  (define p-vars (set->list (vars p)))
  (let loop ([xs p-vars]
             [ys (map genvar p-vars)]
             [p* p]
             [t* t])
    (if (null? xs)
        (values p* t*)
        (let ([p** ((patt (car xs) (car ys)) p*)]
              [t** ((term (car xs) (car ys)) t*)])
          (loop (cdr xs) (cdr ys) p** t**)))))

(define genvar (φ x (string->uninterned-symbol (symbol->string x))))

(define (α-restore x)
  (string->symbol (symbol->string x)))

;;; ============================================================================

(module+ test
  (require rackunit
           syntax/parse/define)

  (define-syntax-parser check-algebraic
    #:datum-literals (=> ~>)
    [(_ (~seq term (~and arrow (~or => ~>)) want) ...)
     #:with (cmd ...)
     (for/list ([t (syntax-e #'(term ...))]
                [a (syntax-e #'(arrow ...))])
       (cond [(eq? (syntax->datum a) '=>) #`(algebraic #,t)]
             [(eq? (syntax->datum a) '~>) #`(show (step (parse '#,t)))]
             [else (error 'unhandled)]))
     #'(begin (check-algebraic* 'term 'arrow (λ () cmd) 'want) ...)])

  (define-check (check-algebraic* in arr thunk want)
    (with-check-info (['argument in]
                      ['expected want])
      (let ([got (with-handlers
                   ([exn:fail? (λ (ex)
                                 (with-check-info (['exception ex])
                                   (fail-check "got stuck")))])
                   (thunk))])
        (with-check-info (['actual got])
          (or (equal? got want)
              (fail-check "bad result"))))))

  (test-case "value"
    (check-algebraic
     ◊ => ◊                             ;unit
     A => A                             ;constructor
     (φ x x) => (φ x x)                 ;function
     (μ x x) => (μ x x)))               ;macro

  (test-case "variable"
    (check-algebraic
     ((φ x x) A) => A
     ((μ x x) A) => A
     ((μ x x) ((φ y y) A)) ~> ((φ y y) A)
     ((μ x x) ((φ y y) A)) => A))

  (test-case "application"
    (check-algebraic
     (((φ x x) (φ y y)) A) ~> ((φ y y) A)
     (((φ x x) (φ y y)) A) => A
     ((φ x x) ((φ y y) A)) ~> ((φ x x) A)
     ((φ x x) ((φ y y) A)) => A))

  (test-case "constructor"
    (check-algebraic
     (A ◊)           => (A ◊)
     (A ((φ x x) B)) => (A B)))

  (test-case "sequence"
    (check-algebraic
     ($ ((φ x x) A) B) ~> ($ A B)
     ($ A ((φ x x) B)) ~> ($ A B)
     (A ($ B C))           => (A ($ B C))
     (A ($ ((φ x x) B) C)) => (A ($ B C))
     (A ($ B ((φ x x) C))) => (A ($ B C))
     (A ($ ((φ x x) B) ((φ y y) C))) ~> (A ($ B ((φ y y) C)))))

  (test-case "function"
    (check-algebraic
     (($ (φ A Z) (φ B Y)) A) => Z
     (($ (φ A Z) (φ B Y)) B) => Y
     (($ (φ (A a) (Z a)) (φ (B b) (Y b))) (A ◊)) => (Z ◊)
     (($ (φ (A a) (Z a)) (φ (B b) (Y b))) (B ◊)) => (Y ◊)))

  (test-case "macro"
    (check-algebraic
     (($ (μ A Z) (μ B Y)) A) => Z
     (($ (μ A Z) (μ B Y)) B) => Y
     (($ (μ (A a) (Z a)) (μ (B b) (Y b))) (A ◊)) => (Z ◊)
     (($ (μ (A a) (Z a)) (μ (B b) (Y b))) (B ◊)) => (Y ◊)
     (($ (μ (A a) (Z a)) (μ (B b) (Y b))) (A ((φ x x) ◊))) ~> (Z ((φ x x) ◊))))

  (test-case "1 + 2 = 3"
    (check-algebraic
     ((φ fix
        ((φ add (add ((Succ Zero) (Succ (Succ Zero)))))
         (fix
          (φ add ($ (φ (a Zero) a) (φ (a (Succ b)) (Succ (add (a b)))))))))
      (φ f ((φ x (f (φ y ((x x) y)))) (φ x (f (φ y ((x x) y)))))))
     => (Succ (Succ (Succ Zero)))))

  (test-case "2 * 3 = 6"
    (check-algebraic
     ((φ fix
        ((φ add
           ((φ mul
              (mul ((Succ (Succ Zero)) (Succ (Succ (Succ Zero))))))
            (fix (φ mul ($ (φ (a Zero) Zero)
                           (φ (a (Succ b)) (add (a (mul (a b))))))))))
         (fix (φ add ($ (φ (a Zero) a)
                        (φ (a (Succ b)) (Succ (add (a b)))))))))
      (φ f ((φ x (f (φ y ((x x) y))))
            (φ x (f (φ y ((x x) y)))))))
     =>
     (Succ (Succ (Succ (Succ (Succ (Succ Zero))))))))

  (test-case "booleans"
    (check-algebraic
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
            (φ x (f (φ y ((x x) y)))))))
     => False))

  (test-case "list 1 2 3 ◊"
    (check-algebraic
     ((φ fix
        ((φ list
           (list ((Succ Zero)
                  ((Succ (Succ Zero)) ((Succ (Succ (Succ Zero))) ◊)))))
         (fix (φ list ($ (μ (x ◊) (Cons ($ x Nil)))
                         (μ (x xs) (Cons ($ x (list xs)))))))))
      (φ f ((φ x (f (φ y ((x x) y))))
            (φ x (f (φ y ((x x) y)))))))
     =>
     (Cons ($ (Succ Zero)
              (Cons ($ (Succ (Succ Zero))
                       (Cons ($ (Succ (Succ (Succ Zero))) Nil))))))))

  (test-case "reverse list 1 2 3 ◊"
    (check-algebraic
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
            (φ x (f (φ y ((x x) y)))))))
     =>
     (Cons ($ (Succ (Succ (Succ Zero)))
              (Cons ($ (Succ (Succ Zero))
                       (Cons ($ (Succ Zero) Nil))))))))

  (test-case "append (list 1 2 ◊) list 3 4 ◊"
    (check-algebraic
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
            (φ x (f (φ y ((x x) y)))))))
     =>
     (Cons ($ (Succ Zero)
              (Cons ($ (Succ (Succ Zero))
                       (Cons ($ (Succ (Succ (Succ Zero)))
                                (Cons ($ (Succ (Succ (Succ (Succ Zero))))
                                         Nil))))))))))

  (test-case "map Succ list 3 2 1 ◊"
    (check-algebraic
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
            (φ x (f (φ y ((x x) y)))))))
     =>
     (Cons ($ (Succ (Succ (Succ (Succ Zero))))
              (Cons ($ (Succ (Succ (Succ Zero)))
                       (Cons ($ (Succ (Succ Zero)) Nil)))))))))
