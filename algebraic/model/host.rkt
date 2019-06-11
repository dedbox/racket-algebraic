#lang algebraic/racket/base

(require (only-in algebraic/model/core
                  define-interpreter define-stepper values-> first-char genvar)
         racket/contract/base
         racket/hash
         racket/set
         racket/string
         (for-syntax syntax/parse))

(provide (all-defined-out)
         (for-syntax (all-defined-out))
         #%app #%datum
         (rename-out
          [host-module-begin #%module-begin]
          [host-top-interaction #%top-interaction]))

(define-syntax host-module-begin
  (μ* (form ...)
    (#%module-begin (algebraic form) ...)))

(define-syntax host-top-interaction
  (μ* form
    (#%top-interaction . (algebraic form))))

(module reader syntax/module-reader
  algebraic/model/host)

;;; ----------------------------------------------------------------------------
;;; Syntax

(data Term (TApp TSeq TFun TMac TVar TCon TUni TLit TPro)
      Patt (PApp PSeq PWil PVar PCon PUni PLit PPro PGua))

;;; parse : s-expression -> term
(define (parse t)
  (define term
    (function
      ['fix (term '(φ f (φ x f φ y (x x) y) (φ x f φ y (x x) y)))]
      [('fun [p . ts]) (term `(φ ,p ,@ts))]
      [('mac [p . ts]) (term `(μ ,p ,@ts))]
      [('fun c . cs) (TSeq (term `(fun ,c)) (term `(fun ,@cs)))]
      [('mac c . cs) (TSeq (term `(mac ,c)) (term `(mac ,@cs)))]
      [('let () . body) (term body)]
      [('let ([p . ts] . cs) . body) (term `((φ ,p let ,cs ,@body) ,@ts))]
      [('letrec () . body) (term body)]
      [('letrec ([p . ts] . cs) . body) (term `((φ ,p letrec ,cs ,@body) fix φ ,p ,@ts))]
      [('$ t1 t2 . ts) (TSeq (term t1) (term `($ ,t2 ,@ts)))]
      [('$ t1) (term t1)]
      [('φ p1 . t2) (values-> TFun (α-rename (patt p1) (term t2)))]
      [('μ p1 . t2) (values-> TMac (α-rename (patt p1) (term t2)))]
      [(t1 t2 . ts) (TApp (term t1) (term (cons t2 ts)))]
      [(t1) (term t1)]
      ['◊ TUni]
      ['= (TPro '= =)]
      ['> (TPro '> >)]
      ['< (TPro '< <)]
      ['+ (TPro '+ +)]
      ['- (TPro '- -)]
      ['* (TPro '* *)]
      ['/ (TPro '/ /)]
      ['== (TPro '== equal?)]
      ['++ (TPro '++ string-append)]
      ['// (TPro '// string-split)]
      ['not (TPro 'not not)]
      ['and (TPro 'and (λ xs (andmap values xs)))]
      ['or (TPro 'or (λ xs  (ormap values xs)))]
      [x #:if (and (symbol? x) (char-lower-case? (first-char x))) (TVar x)]
      [x #:if (and (symbol? x) (char-upper-case? (first-char x))) (TCon x)]
      [n #:if (number? n) (TLit n)]
      [s #:if (string? s) (TLit s)]
      [b #:if (boolean? b) (TLit b)]))

  (define patt
    (function
      [('$ p1 p2 . ps) (PSeq (patt p1) (patt `($ ,p2 ,@ps)))]
      [('$ p1) (patt p1)]
      [(p1 'if . t2) (PGua (patt p1) (term t2))]
      [(p1  p2 . ps) (PApp (patt p1) (patt `(,p2 ,@ps)))]
      [(p1) (patt p1)]
      ['_ PWil]
      ['◊ PUni]
      ['+ (PPro '+)]
      ['- (PPro '-)]
      ['* (PPro '*)]
      ['/ (PPro '/)]
      [x #:if (and (symbol? x) (char-lower-case? (first-char x))) (PVar x)]
      [x #:if (and (symbol? x) (char-upper-case? (first-char x))) (PCon x)]
      [n #:if (number? n) (PLit n)]
      [s #:if (string? s) (PLit s)]
      [b #:if (boolean? b) (PLit b)]))

  (term t))

;;; show : term -> s-expression
(define (show a)
  (define term
    (function
      [(TSeq (TFun _ _) _) #:as t `(fun ,@(show-fun t))]
      [(TSeq (TMac _ _) _) #:as t `(mac ,@(show-mac t))]
      [(TSeq _ _) #:as t `($ ,@(seq->list t))]
      [(TApp _ _) #:as t (app->list t)]
      [(TFun p1 t2) `(φ ,(patt p1) ,@(app->list t2))]
      [(TMac p1 t2) `(μ ,(patt p1) ,@(app->list t2))]
      [(TPro x1 _ ) x1]
      [(TVar x1) (string->symbol (symbol->string x1))]
      [(TCon δ1) δ1]
      [(TLit ℓ1) ℓ1]
      [TUni '◊]))

  (define patt
    (function
      [(PGua p1 t2) `(,(patt p1) if ,(term t2))]
      [(PApp _ _) #:as p (app->list p)]
      [(PSeq _ _) #:as p (seq->list p)]
      [(PVar x1) (string->symbol (symbol->string x1))]
      [(PCon δ1) δ1]
      [(PLit ℓ1) ℓ1]
      [(PPro f1) f1]
      [PWil '_]
      [PUni '◊]))

  (define show-mac
    (function
      [(TMac p1 t2) `([,(patt p1) ,@(app->list t2)])]
      [(TSeq t1 t2) (append (show-mac t1) (show-mac t2))]))

  (define show-fun
    (function
      [(TFun p1 t2) `([,(patt p1) ,@(app->list t2)])]
      [(TSeq t1 t2) (append (show-fun t1) (show-fun t2))]))
  
  (cond [((sum Term?) a) (term a)]
        [((sum Patt?) a) (patt a)]))

(define app->list
  (function
    [(TApp t1 t2) (cons (show t1) (app->list t2))]
    [(PApp p1 p2) (cons (show p1) (app->list p2))]
    [t1 (list (show t1))]))

(define seq->list
  (function
    [(TSeq t1 t2) (cons (show t1) (seq->list t2))]
    [(PSeq p1 p2) (cons (show p1) (seq->list p2))]
    [t1 (list (show t1))]))

(define term->literals
  (function
    [(TLit ℓ1) (list ℓ1)]
    [(TApp (TLit ℓ1) t2) (cons ℓ1 (term->literals t2))]))

(define literals->term
  (function
    [(ℓ1) (TLit ℓ1)]
    [(ℓ1 . ℓs) (TApp (TLit ℓ1) (literals->term ℓs))]))

;;; ----------------------------------------------------------------------------
;;; Semantics

(define-syntax algebraic (μ t (show (interpret (parse 't)))))

(define-interpreter (interpret t) value?
  (step t))

(define (value? t)
  (or (equal? TUni t) ((or/c φ? μ? data? literal?) t)))

(define-syntax define-uniform-seq-pred
  (μ* (name? δ)
    (define name?
      (function
        [(TSeq (δ . _) t) (name? t)]
        [(δ . _) #t]
        [_ #f]))))

(define-uniform-seq-pred φ? TFun)
(define-uniform-seq-pred μ? TMac)

(define data?
  (function
    [(TApp (TCon _) t) (instance-data? t)]
    [(TCon _) #t]
    [_ #f]))

(define instance-data?
  (function
    [(TSeq t1 t2) (and (value? t1) (instance-data? t2))]
    [t (value? t)]))

(define literal?
  (function
    [(TApp (TLit _) t) (literal? t)]
    [(TLit _) #t]
    [_ #f]))

(define-stepper step (seq1 seq2 app1 appM redM app2 appF redF appP)
  [seq1 (TSeq t1 t2) (let ([t1* (step t1)]) (and t1* (TSeq t1* t2 )))]
  [seq2 (TSeq v1 t2) (let ([t2* (step t2)]) (and t2* (TSeq v1  t2*)))]
  [app1 (TApp t1 t2) (let ([t1* (step t1)]) (and t1* (TApp t1* t2 )))]
  [app2 (TApp v1 t2) (let ([t2* (step t2)]) (and t2* (TApp v1  t2*)))]
  [appF (TApp (TFun p11 t12) v2) (let ([σ (× p11 v2)]) (and σ (subst σ t12)))]
  [appM (TApp (TMac p11 t12) t2) (let ([σ (× p11 t2)]) (and σ (subst σ t12)))]
  [redF (TApp (TSeq (TFun x t) v12) v2) (or (step (TApp (TFun x t) v2)) (TApp v12 v2))]
  [redM (TApp (TSeq (TMac x t) v12) t2) (or (step (TApp (TMac x t) t2)) (TApp v12 t2))]
  [appP ((TApp (TPro _ f1) ℓ2) #:if (literal? ℓ2))
        (literals->term (values-> list (apply f1 (term->literals ℓ2))))])

;;; ----------------------------------------------------------------------------
;;; Pragmatics

(define vars
  (function
    [(PApp p1 p2) (set-union (vars p1) (vars p2))]
    [(PSeq p1 p2) (set-union (vars p1) (vars p2))]
    [(PGua p1 _ ) (vars p1)]
    [(PVar x1) (seteq x1)]
    [(PCon _ ) (seteq)]
    [(PLit _ ) (seteq)]
    [(PPro _ ) (seteq)]
    [PWil (seteq)]
    [PUni (seteq)]))

(define (α-rename p t)
  (define (term x y)
    (function
      [(TApp t1 t2) (TApp ((term x y) t1) ((term x y) t2))]
      [(TSeq t1 t2) (TSeq ((term x y) t1) ((term x y) t2))]
      [(TFun p1 t2) (TFun p1 ((term x y) t2))]
      [(TMac p1 t2) (TMac p1 ((term x y) t2))]
      [(TPro x1 f2) (TPro x1 f2)]
      [(TVar x1) (TVar (if (equal? x1 x) y x1))]
      [(TCon δ1) (TCon δ1)]
      [(TLit ℓ1) (TLit ℓ1)]
      [TUni TUni]))
  (define (patt x y)
    (function
      [(PApp p1 p2) (PApp ((patt x y) p1) ((patt x y) p2))]
      [(PSeq p1 p2) (PSeq ((patt x y) p1) ((patt x y) p2))]
      [(PGua p1 t2) (PGua ((patt x y) p1) ((term x y) t2))]
      [(PVar x1) (PVar (if (equal? x1 x) y x1))]
      [(PCon δ1) (PCon δ1)]
      [(PLit ℓ1) (PLit ℓ1)]
      [(PPro f1) (PPro f1)]
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

(define (subst σ t [mask (seteq)])
  ((function
     [(TApp t1 t2) (TApp (subst σ t1 mask) (subst σ t2 mask))]
     [(TSeq t1 t2) (TSeq (subst σ t1 mask) (subst σ t2 mask))]
     [(TFun p1 t2) (TFun p1 (subst σ t2 (set-union mask (vars p1))))]
     [(TMac p1 t2) (TMac p1 (subst σ t2 (set-union mask (vars p1))))]
     [(TPro _  _ ) t]
     [(TVar x1) (if (set-member? mask x1) t (hash-ref σ x1))]
     [(TCon _ ) t]
     [(TLit _ ) t]
     [TUni t])
   t))

(define ×
  (function*
    [((PApp p1 p2) (TApp t1 t2)) (let ([σ1 (× p1 t1)]
                                       [σ2 (× p2 t2)])
                                   (and σ1 σ2 (hash-union σ1 σ2)))]
    [((PSeq p1 p2) (TSeq t1 t2)) (let ([σ1 (× p1 t1)]
                                       [σ2 (× p2 t2)])
                                   (and σ1 σ2 (hash-union σ1 σ2)))]
    [((PGua p1 t2) t3) (let ([σ1 (× p1 t3)])
                         (and σ1 (not (equal? (interpret (subst σ1 t2)) (TLit #f)))
                              σ1))]
    [((PPro x) (TPro x _)) (make-immutable-hasheq)]
    [((PCon δ) (TCon δ)) (make-immutable-hasheq)]
    [((PLit ℓ) (TLit ℓ)) (make-immutable-hasheq)]
    [((PVar x1) t2) (make-immutable-hasheq `([,x1 . ,t2]))]
    [(PUni TUni) (make-immutable-hasheq)]
    [(PWil _) (make-immutable-hasheq)]
    [(_ _) #f]))

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
     ((fun [A Z] [B Y]) A) => Z
     ((fun [A Z] [B Y]) B) => Y
     ((fun [(A a) Z a] [(B b) Y b]) A ◊) => (Z ◊)
     ((fun [(A a) Z a] [(B b) Y b]) B ◊) => (Y ◊)))

  (test-case "macro"
    (check-algebraic
     ((mac [A Z] [B Y]) A) => Z
     ((mac [A Z] [B Y]) B) => Y
     ((mac [(A a) Z a] [(B b) Y b]) A ◊) => (Z ◊)
     ((mac [(A a) Z a] [(B b) Y b]) B ◊) => (Y ◊)
     ((mac [(A a) Z a] [(B b) Y b]) A (φ x x) ◊) ~> (Z (φ x x) ◊)))

  (test-case "1 + 2 = 3"
    (check-algebraic (+ 1 2) => 3))

  (test-case "2 * 3 = 6"
    (check-algebraic (* 2 3) => 6))

  (test-case "booleans"
    (check-algebraic
     (letrec ([xor μ (a b) (fun [#f b] [x and (not b) x]) a])
       or (not #t) and (xor #t #t) #t)
     =>
     #f))

  (test-case "list 1 2 3 ◊"
    (check-algebraic
     (letrec ([list fun [(x ◊) Cons $ x Nil] [(x xs) Cons $ x (list xs)]])
       list 1 2 3 ◊)
     =>
     (Cons ($ 1 (Cons ($ 2 (Cons ($ 3 Nil))))))))

  (test-case "reverse list 1 2 3 ◊"
    (check-algebraic
     (letrec ([list fun [(x ◊) Cons $ x Nil] [(x xs) Cons $ x (list xs)]]
              [rev fun [(Nil a) a] [((Cons ($ y ys)) a) rev ys Cons $ y a]]
              [reverse φ xs rev xs Nil])
       reverse list 1 2 3 ◊)
     =>
     (Cons ($ 3 (Cons ($ 2 (Cons ($ 1 Nil))))))))

  (test-case "append (list 1 2 ◊) list 3 4 ◊"
    (check-algebraic
     (letrec ([list fun [(x ◊) Cons $ x Nil] [(x xs) Cons $ x (list xs)]]
              [append fun [(Nil ys) ys] [((Cons ($ x xs)) ys) Cons $ x (append xs ys)]])
       (append (list 1 2 ◊) list 3 4 ◊))
     =>
     (Cons ($ 1 (Cons ($ 2 (Cons ($ 3 (Cons ($ 4 Nil))))))))))

  (test-case "map Succ list 3 2 1 ◊"
    (check-algebraic
     (letrec ([in  fun [0 Zero] [n Succ in - n 1]]
              [out fun [Zero 0] [(Succ n) + 1 (out n)]]
              [list fun [(x ◊) Cons $ x Nil] [(x xs) Cons $ x (list xs)]]
              [map fun [(_ Nil) Nil] [(f (Cons ($ x xs))) Cons $ (f x) (map f xs)]])
       map out map Succ list (in 3) (in 2) (in 1) ◊)
     =>
     (Cons ($ 4 (Cons ($ 3 (Cons ($ 2 Nil)))))))))
