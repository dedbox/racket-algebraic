#lang racket/base

(require (for-syntax algebraic/racket/literal
                     racket/base
                     racket/function
                     racket/match
                     racket/syntax
                     syntax/parse/define
                     syntax/stx)
         algebraic/racket/macro
         racket/base
         racket/contract/base
         racket/format
         racket/function
         racket/match
         racket/string
         syntax/parse
         syntax/parse/define)

(provide
 (all-from-out racket/base)
 data φ phi function function*
 (contract-out
  [constructor? predicate/c]
  [instance? predicate/c]
  [function? predicate/c]))

;;; Abstract Syntax

(begin-for-syntax
  (struct scon (δ)
    #:transparent
    #:property prop:procedure
    (λ (c stx)
      (syntax-parse stx
        [(_ v ...) #`(ins (con '#,(scon-δ c)) (list v ...))]
        [_ #`(con '#,(scon-δ c))]))))

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

(struct fun (matcher ps-stx ts-stx)
  #:property prop:procedure (λ (f . args) (apply (fun-matcher f) args))
  #:methods gen:custom-write
  [(define (write-proc f port mode)
     (define (f-clause ps ts)
       `[,(car ps) ,@ts])
     (define (f*-clause ps ts)
       `[,ps ,@ts])
     (define (lone-clause? pss)
       (null? (cdr pss)))
     (define (lone-patt? ps)
       (and (not (null? ps)) (null? (cdr ps))))
     (case mode
       [(#t #f) (write-string "#<function>" port)]
       [else
        (let* ([pss (syntax->list (fun-ps-stx f))]
               [tss (syntax->list (fun-ts-stx f))])
          (print
           (cond
             [(and (lone-clause? pss) (lone-patt? (car pss)))
              `(φ ,(car pss) ,@(car tss))]
             [(andmap lone-patt? pss #`(function ,@(map f-clause pss tss)))]
             [else `(function* ,@(map f*-clause pss tss))])))]))])

(define constructor? con?)
(define instance? ins?)
(define function? fun?)

;;; Data

(define-simple-macro (data δ:id ...+)
  (begin (begin (define-for-syntax δ (scon 'δ)) (define δ (con 'δ))) ...))

;;; Functions

(define-match-expander match-if
  (λ (stx)
    (syntax-parse stx
      [(_ p t) #'(and p (app (match-lambda [p t] [_ #f]) (not #f)))])))

(begin-for-syntax
  (define (pmatch stx)
    (syntax-parse stx
      [ℓ:racket-literal #'ℓ]
      [x:id
       (let* ([name (symbol->string (syntax->datum #'x))]
              [first-char (string-ref name 0)])
         (cond
           [(char=? first-char #\_) #'_]
           [(char-lower-case? first-char) #'x]
           [(and (identifier-transformer-binding #'x)
                 (scon? (syntax-local-eval #'x))) #'(con 'x)]
           [(identifier-binding #'x) #'(app (λ (y) (eq? y x)) #t)]))]
      [(p #:if t) #'(match-if p t)]
      [(x:id ps ...)
       #`(ins #,(pmatch #'x) (list #,@(stx-map pmatch #'(ps ...))))])))

(define-simple-macro (function* [(p ...) t ...+] ...+)
  #:with ((p* ...) ...) (stx-map (curry stx-map pmatch) #'((p ...) ...))
  (fun (λ args (match args [(list p* ...) t ...] ...))
       #'((p ...) ...)
       #'((t ...) ...)))

(define-simple-macro (function [p t ...+] ...+)
  (function* [(p) t ...] ...))

(define-simple-macro (phi p t ...+)
  (function* [(p) t ...]))

(define-simple-macro (φ p t ...+)
  (function* [(p) t ...]))

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
