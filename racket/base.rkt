#lang racket/base

(require (for-syntax algebraic/racket/literal
                     racket/base
                     racket/match
                     racket/syntax
                     syntax/stx)
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

  (define-syntax-class regexp
    #:description "regex pattern"
    #:attributes (match-pat)
    (pattern x:expr
             #:when (regexp? (syntax->datum #'x))
             #:attr match-pat
             #'(app (λ (input) (regexp-match (syntax->datum #'x) input))
                    (not #f)))
    (pattern (x:expr p:patt ...+)
             #:when (regexp? (syntax->datum #'x))
             #:attr match-pat
             #'(app (λ (input) (regexp-match (syntax->datum #'x) input))
                    (or (? (λ (vs) (null? (cdr vs)))
                           (list p.match-pat ...))
                        (list _ p.match-pat ...)))))

  (define-syntax-class pair
    #:description "pair pattern"
    #:attributes (match-pat)
    (pattern (p1:patt . p2:patt)
             #:attr match-pat #'(cons p1.match-pat p2.match-pat)))

  (define-syntax-class vector
    #:description "vector pattern"
    #:attributes (match-pat)
    (pattern x:expr #:when (vector? (syntax->datum #'x))
             #:with (p:patt ...)
             (map (λ (v) (datum->syntax this-syntax v this-syntax))
                  (vector->list (syntax->datum #'x)))
             #:attr match-pat #'(vector p.match-pat ...)))

  (define-syntax-class box
    #:description "box pattern"
    #:attributes (match-pat)
    (pattern x:expr #:when (box? (syntax->datum #'x))
             #:with p:patt
             (datum->syntax this-syntax (unbox (syntax->datum #'x)) this-syntax)
             #:attr match-pat #'(box p.match-pat)))

  (define-syntax-class key
    #:description #f
    #:attributes (match-pat)
    (pattern k:id #:attr match-pat #''k)
    (pattern match-pat))

  (define-syntax-class hash
    #:description "hash pattern"
    #:attributes (match-pat)
    (pattern x:expr #:when (hash? (syntax->datum #'x))
             #:with (k:key ...) (hash-keys (syntax->datum #'x))
             #:with (v:patt ...) (hash-values (syntax-e #'x))
             #:attr match-pat
             #'(hash-table (k.match-pat v.match-pat) ... (_ _) (... ...))))

  (define-syntax-class void
    #:description "void pattern"
    #:attributes (match-pat)
    #:literals (void)
    (pattern (void) #:attr match-pat #'(? void?)))

  (define-syntax-class struct
    #:description "struct pattern"
    #:attributes (match-pat)
    (pattern (struct-id:id [f:id p:patt] ...)
             #:attr match-pat #'(struct* struct-id ([f p.match-pat] ...))))

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
    (pattern i:instance #:attr match-pat #'i.match-pat)
    (pattern r:regexp #:attr match-pat #'r.match-pat)
    (pattern p:pair #:attr match-pat #'p.match-pat)
    (pattern v:vector #:attr match-pat #'v.match-pat)
    (pattern b:box #:attr match-pat #'b.match-pat)
    (pattern h:hash #:attr match-pat #'h.match-pat)
    (pattern v:void #:attr match-pat #'v.match-pat)
    (pattern s:struct #:attr match-pat #'s.match-pat))

  (define-splicing-syntax-class maybe-if
    #:description #f
    #:attributes (e)
    (pattern (~seq #:if e:expr))))

(define-match-expander if-pat
  (syntax-parser
    [(_ p:patt t:expr)
     #'(and p.match-pat (app (match-lambda [p.match-pat t]) (not #f)))]))

(struct fun (matcher)
  #:reflection-name 'function
  #:property prop:procedure (λ (f . args) (apply (fun-matcher f) args)))

(define function? fun?)

(define-simple-macro (function* [(~or (p:patt ...)
                                      (p:patt ...+ . rest-p:patt)
                                      rest-p:patt)
                                 ifs:maybe-if ... t:expr ...+] ...+)
  (fun (λ args (match args
                 [(~? (~@ (list-rest p.match-pat ... rest-p.match-pat))
                      (~? (~@ (list p.match-pat ...))
                          (~@ (list-rest rest-p.match-pat))))
                  (~? (~@ #:when (and ifs.e ...))) t ...]
                 ...
                 [_ (error 'function* "no matching clause for (~a)"
                           (string-join (map ~v args)))]))))

(define-simple-macro (function [p:patt ifs:maybe-if ... t:expr ...+] ...+)
  (fun (λ (arg) (match arg
                  [p.match-pat (~? (~@ #:when (and ifs.e ...))) t ...]
                  ...
                  [_ (error 'function "no matching clause for ~v" arg)]))))

(define-simple-macro (phi p:patt ifs:maybe-if ... t:expr ...+)
  (fun (λ (arg) (match arg
                  [p.match-pat (~? (~@ #:when (and ifs.e ...)))
                               t ...]
                  [_ (error 'phi "no matching clause for ~v" arg)]))))

(define-simple-macro (φ p:patt ifs:maybe-if ... t:expr ...+)
  (fun (λ (arg) (match arg
                  [p.match-pat (~? (~@ #:when (and ifs.e ...))) t ...]
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
