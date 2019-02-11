#lang racket/base

(require (for-syntax algebraic/racket/base/syntax
                     (except-in racket/base
                                hash void vector regexp quasiquote struct box)
                     racket/function
                     syntax/stx)
         racket/base
         racket/contract/base
         racket/syntax
         syntax/parse
         syntax/parse/define)

(provide (all-from-out racket/base syntax/parse)
         μ mu macro μ* mu* macro* var
         (contract-out [macro-expand (-> syntax? syntax?)]))

(begin-for-syntax
  (define (first-char id-stx)
    (string-ref (symbol->string (syntax->datum id-stx)) 0))

  (define-syntax-class wildcard
    #:description "wildcard macro pattern"
    #:attributes (parse-pat)
    (pattern x:id #:when (char=? (first-char #'x) #\_) #:attr parse-pat #'_))

  (define-syntax-class variable
    #:description "macro pattern variable"
    #:attributes (parse-pat)
    (pattern x:id #:when (char-lower-case? (first-char #'x)) #:attr parse-pat #'x))

  (define-syntax-class id-literal
    #:description "identifier macro pattern"
    #:attributes (parse-pat)
    (pattern x:id #:attr parse-pat #'(~literal x)))

  (define-syntax-class sequence
    #:description #f
    #:attributes (parse-pat)
    (pattern () #:attr parse-pat #'())
    (pattern (p1:macro-patt . p2:macro-patt)
             #:attr parse-pat #'(p1.parse-pat . p2.parse-pat)))

  (define-syntax-class macro-patt
    #:description "macro pattern"
    #:attributes (parse-pat)
    (pattern (~literal ...) #:attr parse-pat this-syntax)
    (pattern (~literal ...+) #:attr parse-pat this-syntax)
    (pattern ℓ:literal-value #:attr parse-pat this-syntax)
    (pattern w:wildcard #:attr parse-pat #'w.parse-pat)
    (pattern x:variable #:attr parse-pat #'x.parse-pat)
    (pattern x:id-literal #:attr parse-pat #'x.parse-pat)
    (pattern s:sequence #:attr parse-pat #'s.parse-pat))

  (define simplify
    (syntax-parser
      [(t) #'t]
      [(t ts ...+) #'(begin t ts ...)])))

(define-simple-macro (macro* [(p:macro-patt ...)
                              (~alt (~seq #:with with-p:macro-patt with-stx:expr)
                                    (~seq #:when condition:expr)) ...
                              t:expr ...+] ...+)
  #:with (t* ...) (stx-map simplify #'((t ...) ...))
  (syntax-parser
    [(_ p.parse-pat ...) (~? (~@ #:with with-p.parse-pat #`with-stx)) ...
                         (~? (~@ #:when condition)) ...
                         #`t*]
    ...))

(define-simple-macro (mu* (p:macro-patt ...)
                          (~alt (~seq #:with with-p:macro-patt with-stx:expr)
                                (~seq #:when condition:expr)) ...
                          t:expr ...+)
  #:with t* (simplify #'(t ...))
  (syntax-parser
    [(_ p.parse-pat ...) (~? (~@ #:with with-p.parse-pat #`with-stx)) ...
                         (~? (~@ #:when condition)) ...
                         #`t*]))

(define-simple-macro (μ* (p:macro-patt ...)
                          (~alt (~seq #:with with-p:macro-patt with-stx:expr)
                                (~seq #:when condition:expr)) ...
                          t:expr ...+)
  #:with t* (simplify #'(t ...))
  (syntax-parser
    [(_ p.parse-pat ...) (~? (~@ #:with with-p.parse-pat #`with-stx)) ...
                         (~? (~@ #:when condition)) ...
                         #`t*]))

(define-simple-macro (macro [p:macro-patt
                             (~alt (~seq #:with with-p:macro-patt with-stx:expr)
                                   (~seq #:when condition:expr)) ...
                             t:expr ...+] ...+)
  #:with (t* ...) (stx-map simplify #'((t ...) ...))
  (syntax-parser
    [(_ p.parse-pat) (~? (~@ #:with with-p.parse-pat #`with-stx)) ...
                     (~? (~@ #:when condition)) ...
                     #`t*]
    ...))

(define-simple-macro (mu p:macro-patt
                       (~alt (~seq #:with with-p:macro-patt with-stx:expr)
                             (~seq #:when condition:expr)) ...
                       t:expr ...+)
  #:with t* (simplify #'(t ...))
  (syntax-parser
    [(_ p.parse-pat) (~? (~@ #:with with-p.parse-pat #`with-stx)) ...
                     (~? (~@ #:when condition)) ...
                     #`t*]))

(define-simple-macro (μ p:macro-patt
                       (~alt (~seq #:with with-p:macro-patt with-stx:expr)
                             (~seq #:when condition:expr)) ...
                       t:expr ...+)
  #:with t* (simplify #'(t ...))
  (syntax-parser
    [(_ p.parse-pat) (~? (~@ #:with with-p.parse-pat #`with-stx)) ...
                     (~? (~@ #:when condition)) ...
                     #`t*]))

(define-simple-macro (var id)
  (syntax-local-eval #'id))

(define (macro-expand stx)
  (local-expand stx 'expression null))
