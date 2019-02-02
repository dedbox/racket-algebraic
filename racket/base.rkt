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
  [(define (write-proc f port mode) (print-fun f port mode))])

(define constructor? con?)
(define instance? ins?)
(define function? fun?)

;;; Printer

(define ((print-obj short long get-ps get-ts) o port mode)
  (define (clause ps ts)
    `[,(if (and (not (null? ps)) (null? (cdr ps))) (car ps) ps) ,@ts])
  (case mode
    [(#t #f) (write-string (format "#<~a>" long) port)]
    [else (let ([clauses (map clause
                              (syntax->datum (get-ps o))
                              (syntax->datum (get-ts o)))])
            (print (if (null? (cdr clauses))
                       `(,short ,@(car clauses))
                       `(,long ,@clauses))
                   port mode))]))

(define print-fun (print-obj 'φ 'function fun-ps-stx fun-ts-stx))

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
  (require rackunit)

  (test-case "Numbers"
    (define fib
      (function
        [(n #:if (< n 2)) 1]
        [n (+ (fib (- n 1)) (fib (- n 2)))]))
    (check equal? (map fib '(0 1 2 3 4 5 6)) '(1 1 2 3 5 8 13))))
