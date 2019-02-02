#lang racket/base

(require (for-syntax algebraic/racket/literal
                     racket/base
                     racket/function
                     syntax/parse
                     syntax/stx)
         racket/base
         racket/contract/base
         racket/syntax
         syntax/parse)

(provide
 (all-from-out racket/base syntax/parse)
 μ mu macro macro* var
 (contract-out
  [macro-expand (-> syntax? syntax?)]))

(begin-for-syntax
  (define (parse stx)
    (syntax-parse stx
      [ℓ:racket-literal #'ℓ]
      [(~literal ...) #'(... ...)]
      [x:id (let* ([name (symbol->string (syntax->datum #'x))]
                   [first-char (string-ref name 0)])
              (cond
                [(char=? first-char #\_) #'_]
                [(char-lower-case? first-char) this-syntax]
                [else #'(~literal x)]))]
      [(p ps ...) #`(#,@(stx-map parse #'(p ps ...)))]))

  (define (simplify stx)
    (syntax-parse stx
      [(t) #'t]
      [(t ts ...+) #'(begin t ts ...)])))

(define-syntax (macro* stx)
  (syntax-parse stx
    [(_ [(p ...)
         (~alt (~seq #:with with-p with-stx)
               (~seq #:when condition))
         ... t ...+] ...+)
     #:with ((with-p* ...) ...) (stx-map (curry stx-map parse) #'((with-p ...) ...))
     #:with ((p* ...) ...) (stx-map (curry stx-map parse) #'((p ...) ...))
     #:with (t* ...) (stx-map simplify #'((t ...) ...))
     #`(syntax-parser
         [(_ p* ...) (~? (~@ #:with with-p* #`with-stx)) ...
                     (~? (~@ #:when condition)) ...
                     #`t*]
         ...)]))

(define-syntax (macro stx)
  (syntax-parse stx
    [(_ [p (~alt (~seq #:with with-p with-stx)
                 (~seq #:when condition)) ... t ...+] ...+)
     #'(macro* [(p) (~? (~@ #:with with-p with-stx)) ...
                    (~? (~@ #:when condition)) ...
                    t ...] ...)]))

(define-syntax (mu stx)
  (syntax-parse stx
    [(_ p (~alt (~seq #:with with-p with-stx)
                (~seq #:when condition)) ... t ...+)
     #'(macro* [(p) (~? (~@ #:with with-p with-stx)) ...
                    (~? (~@ #:when condition)) ...
                    t ...])]))

(define-syntax (μ stx)
  (syntax-parse stx
    [(_ p (~alt (~seq #:with with-p with-stx)
                (~seq #:when condition)) ... t ...+)
     #'(macro* [(p) (~? (~@ #:with with-p with-stx)) ...
                    (~? (~@ #:when condition)) ...
                    t ...])]))

(define-syntax (var stx)
  (syntax-parse stx
    [(_ id) #'(syntax-local-eval #'id)]))

(define (macro-expand stx)
  (local-expand stx 'expression null))
