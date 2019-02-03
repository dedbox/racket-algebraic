#lang racket/base

(require (for-syntax algebraic/racket/literal
                     racket/base
                     racket/function
                     syntax/stx)
         racket/base
         racket/contract/base
         racket/syntax
         syntax/parse
         syntax/parse/define)

(provide (all-from-out racket/base syntax/parse)
         μ mu macro macro* var
         (contract-out [macro-expand (-> syntax? syntax?)]))

(define-for-syntax parse
  (syntax-parser
    [ℓ:racket-literal #'ℓ]
    [(~literal ...) #'(... ...)]
    [x:id (let* ([name (symbol->string (syntax->datum #'x))]
                 [first-char (string-ref name 0)])
            (cond
              [(char=? first-char #\_) #'_]
              [(char-lower-case? first-char) this-syntax]
              [else #'(~literal x)]))]
    [(p ps ...) #`(#,@(stx-map parse #'(p ps ...)))]))

(define-for-syntax simplify
  (syntax-parser
    [(t) #'t]
    [(t ts ...+) #'(begin t ts ...)]))

(define-simple-macro (macro* [(p ...) (~alt (~seq #:with with-p with-stx)
                                            (~seq #:when condition))
                                      ... t ...+] ...+)
  #:with ((with-p* ...) ...) (stx-map (curry stx-map parse) #'((with-p ...) ...))
  #:with ((p* ...) ...) (stx-map (curry stx-map parse) #'((p ...) ...))
  #:with (t* ...) (stx-map simplify #'((t ...) ...))
  (syntax-parser
    [(_ p* ...) (~? (~@ #:with with-p* #`with-stx)) ...
                (~? (~@ #:when condition)) ...
                #`t*]
    ...))

(define-simple-macro (macro [p (~alt (~seq #:with with-p with-stx)
                                     (~seq #:when condition)) ... t ...+] ...+)
  (macro* [(p) (~? (~@ #:with with-p with-stx)) ...
               (~? (~@ #:when condition)) ...
               t ...] ...))

(define-simple-macro (mu p (~alt (~seq #:with with-p with-stx)
                                 (~seq #:when condition)) ... t ...+)
  (macro* [(p) (~? (~@ #:with with-p with-stx)) ...
               (~? (~@ #:when condition)) ...
               t ...]))

(define-simple-macro (μ p (~alt (~seq #:with with-p with-stx)
                                (~seq #:when condition)) ... t ...+)
  (macro* [(p) (~? (~@ #:with with-p with-stx)) ...
               (~? (~@ #:when condition)) ...
               t ...]))

(define-simple-macro (var id)
  (syntax-local-eval #'id))

(define (macro-expand stx)
  (local-expand stx 'expression null))
