#lang racket/base

(require (for-syntax algebraic/racket/literal
                     racket/base
                     racket/match
                     racket/syntax)
         racket/base
         racket/contract/base
         racket/format
         racket/function
         racket/match
         racket/string
         syntax/parse/define)

(provide
 (all-from-out racket/base)
 data φ phi function function*
 (contract-out
  [constructor? predicate/c]
  [instance? predicate/c]
  [function? predicate/c]))

;;; Constructors

(struct con (δ)
  #:transparent
  #:property prop:procedure (λ (c . args) (ins c args))
  #:methods gen:custom-write
  [(define (write-proc c port mode)
     (case mode
       [(#t #f) (write (con-δ c) port)]
       [else (display (con-δ c) port)]))])

(struct ins (con vs)
  #:methods gen:custom-write
  [(define (write-proc i port mode)
     (case mode
       [(#t #f) (write `(,(con-δ (ins-con i)) ,@(ins-vs i)) port)]
       [else (display `(,(con-δ (ins-con i)) ,@(map ~v (ins-vs i))) port)]))])

(define constructor? con?)
(define instance? ins?)

;;; Data

(define-simple-macro (data δ:id ...+)
  (begin (begin (define-for-syntax δ (scon 'δ)) (define δ (con 'δ))) ...))

;;; Functions

(begin-for-syntax
  (define (first-char id-stx)
    (string-ref (symbol->string (syntax->datum id-stx)) 0))

  (define-syntax-class wildcard
    #:description "wildcard pattern"
    #:attributes (match-pat)
    (pattern x:id #:when (char=? (first-char #'x) #\_) #:attr match-pat #'_))

  (define-syntax-class variable
    #:description "pattern variable"
    #:attributes (match-pat)
    (pattern x:id #:when (char-lower-case? (first-char #'x)) #:attr match-pat #'x))

  (struct scon (δ)
    #:transparent
    #:property prop:procedure
    (λ (c stx)
      (syntax-parse stx
        [(_ v ...) #`(ins (con '#,(scon-δ c)) (list v ...))]
        [_ #`(con '#,(scon-δ c))])))

  (define-syntax-class con-id
    #:description "constructor pattern"
    #:attributes (match-pat)
    (pattern δ:id #:when (and (identifier-transformer-binding #'δ)
                              (scon? (syntax-local-eval #'δ)))
             #:attr match-pat #'(con 'δ)))

  (define-syntax-class reference
    #:description "reference pattern"
    #:attributes (match-pat)
    (pattern a:id #:when (identifier-binding #'a)
             #:attr match-pat #'(app (λ (b) (eq? b a)) #t)))

  (define-syntax-class instance
    #:description "instance pattern"
    #:attributes (match-pat)
    (pattern (δ:con-id p:patt ...)
             #:attr match-pat #'(ins δ.match-pat (list p.match-pat ...))))

  (define-syntax-class conditional
    #:description "conditional pattern"
    #:attributes (match-pat)
    (pattern (p:patt #:if t:expr) #:attr match-pat #'(if-pat p t)))

  (define-syntax-class patt
    #:description "pattern"
    #:opaque
    #:attributes (match-pat)
    (pattern ℓ:host-literal #:attr match-pat #'ℓ)
    (pattern w:wildcard #:attr match-pat #'w.match-pat)
    (pattern x:variable #:attr match-pat #'x.match-pat)
    (pattern δ:con-id #:attr match-pat #'δ.match-pat)
    (pattern v:reference #:attr match-pat #'v.match-pat)
    (pattern c:conditional #:attr match-pat #'c.match-pat)
    (pattern i:instance #:attr match-pat #'i.match-pat)))

(define-match-expander if-pat
  (syntax-parser
    [(_ p:patt t:expr)
     #'(and p.match-pat (app (match-lambda [p.match-pat t]) (not #f)))]))

(struct fun (matcher)
  #:reflection-name 'function
  #:property prop:procedure (λ (f . args) (apply (fun-matcher f) args)))

(define function? fun?)

(define-simple-macro (function* [(p:patt ...) t:expr ...+] ...+)
  (fun (λ args (match args
                 [(list p.match-pat ...) t ...]
                 ...
                 [_ (error 'function* "no matching clause for (~a)"
                           (string-join (map ~v args)))]))))

(define-simple-macro (function [p:patt t:expr ...+] ...+)
  (fun (λ (arg) (match arg
                  [p.match-pat t ...]
                  ...
                  [_ (error 'function "no matching clause for ~v" arg)]))))

(define-simple-macro (phi p:patt t:expr ...+)
  (fun (λ (arg) (match arg
                  [p.match-pat t ...]
                  [_ (error 'phi "no matching clause for ~v" arg)]))))

(define-simple-macro (φ p:patt t:expr ...+)
  (fun (λ (arg) (match arg
                  [p.match-pat t ...]
                  [_ (error 'φ "no matching clause for ~v" arg)]))))

;;; ----------------------------------------------------------------------------

(module+ test
  (require (for-syntax algebraic/racket/macro)
           rackunit)

  (test-case "Numbers"
    (define fib
      (function
        [(n #:if (< n 2)) 1]
        [n (+ (fib (- n 1)) (fib (- n 2)))]))
    (check equal? (map fib '(0 1 2 3 4 5 6)) '(1 1 2 3 5 8 13)))

  (test-case "MSP f-power"
    (define f-power
      (function*
        [(0 _) 1]
        [(n x) (* x (f-power (- n 1) x))]))
    (define f-power2 (curryr f-power 2))
    (check-equal? (map f-power2 '(0 1 2 3 4 5 6)) '(1 2 4 8 16 32 64)))

  (test-case "MSP m-power"
    (define-syntax m-power
      (macro*
        [(0 _) 1]
        [(1 x) x]
        [(n x) (* x (m-power #,(- (var n) 1) x))]))
    (define-syntax m-power3 (μ y (m-power 3 y)))
    (check = (m-power3 2) 8))

  (test-case "MSP q-power"
    (define-syntax q-power
      (macro*
        [(0 _) 1]
        [(1 x) x]
        [(n x) '#,(macro-expand #`(* x (q-power #,(- (var n) 1) x)))]))
    (define-syntax q-power3 (μ y #,(macro-expand #'(q-power 3 y))))
    (check-equal? (q-power3 2) '(#%app * '2 '(#%app * '2 '2)))))
