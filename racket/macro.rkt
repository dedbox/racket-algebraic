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
  (define-syntax-class mac-wildcard
    #:description "wildcard"
    #:attributes (compiled)
    (pattern :wildcard #:attr compiled #'_))

  (define-syntax-class mac-variable
    #:description "metavariable"
    #:attributes (compiled)
    (pattern compiled:variable))

  (define-syntax-class mac-identifier
    #:description "identifier"
    #:attributes (compiled)
    (pattern x:id #:attr compiled #'(~literal x)))

  (define-syntax-class mac-void
    #:description "void macro pattern"
    #:attributes (compiled)
    #:literals (void)
    (pattern (void) #:attr compiled this-syntax))

  (define-syntax-class mac-pair
    #:description "macro pattern sequence"
    #:attributes (compiled)
    (pattern () #:attr compiled this-syntax)
    (pattern :pair
             #:with pcar:mac-patt (attribute car)
             #:with pcdr:mac-patt (attribute cdr)
             #:attr compiled #'(pcar.compiled . pcdr.compiled)))

  (define-syntax-class mac-vector
    #:description "vector macro pattern"
    #:attributes (compiled)
    (pattern v:vector
             #:with (p:mac-patt ...) #'(v.item ...)
             #:attr compiled #'#(p.compiled ...)))

  (define-syntax-class mac-box
    #:description "box macro pattern"
    #:attributes (compiled)
    (pattern b:box
             #:with p:mac-patt #'b.item
             #:attr compiled #'#&p.compiled))

  (define-syntax-class hash-key
    #:description "hash macro pattern key"
    #:commit
    (pattern :id)
    (pattern :literal-value))

  ;; (define-syntax-class mac-hash
  ;;   #:description "hash macro pattern"
  ;;   #:attributes (compiled)
  ;;   (pattern h:hash
  ;;            #:with (k:hash-key ...) (syntax-e #'(h.k ...))
  ;;            #:with (v:mac-patt ...) (syntax-e #'(h.v ...))
  ;;            #:attr compiled #'))

  (define-syntax-class mac-struct
    #:description "struct macro pattern"
    #:attributes (compiled)
    ;; (pattern (id:struct-id ([field:id p:mac-patt] ...))
    ;;          #:attr compiled #'())
    (pattern (id:struct-id p:mac-patt ...)
             #:attr compiled #'(id p.compiled ...)))

  (define-syntax-class mac-quasiquoted
    #:attributes (compiled)
    #:literals (unquote)
    ;; (pattern x:id #:attr compiled #'(~literal x))
    (pattern (unquote p:mac-patt) #:attr compiled #'(unquote p.compiled))
    (pattern (q1:mac-quasiquoted . q2:mac-quasiquoted)
             #:attr compiled #'(q1.compiled . q2.compiled))
    (pattern compiled))

  (define-syntax-class mac-quasiquote
    #:description "quasiquote macro pattern"
    #:attributes (compiled)
    #:literals (quasiquote)
    (pattern (quasiquote datum:mac-quasiquoted)
             #:attr compiled #'(quasiquote datum.compiled)))

  (define-syntax-class mac-patt
    #:description "macro pattern"
    #:attributes (compiled)
    (pattern (~literal ...) #:attr compiled this-syntax)
    (pattern (~literal ...+) #:attr compiled this-syntax)
    (pattern compiled:literal-value)
    (pattern (~or p:mac-wildcard
                  p:mac-variable
                  p:mac-identifier
                  p:mac-void
                  p:mac-struct
                  p:mac-quasiquote
                  p:mac-pair
                  p:mac-vector
                  p:mac-box)
             #:attr compiled #'p.compiled))

  (define simplify
    (syntax-parser
      [(t) #'t]
      [(t ts ...+) #'(begin t ts ...)])))

(define-simple-macro (macro* [(p:mac-patt ...)
                              (~alt (~seq #:with with-p:mac-patt with-stx:expr)
                                    (~seq #:if condition:expr)) ...
                              t:expr ...+] ...+)
  #:with (t* ...) (stx-map simplify #'((t ...) ...))
  (syntax-parser
    [(_ p.compiled ...) (~? (~@ #:with with-p.compiled #`with-stx)) ...
                        (~? (~@ #:when condition)) ...
                        #`t*]
    ...))

(define-simple-macro (mu* (p:mac-patt ...)
                       (~alt (~seq #:with with-p:mac-patt with-stx:expr)
                             (~seq #:if condition:expr)) ...
                       t:expr ...+)
  #:with t* (simplify #'(t ...))
  (syntax-parser
    [(_ p.compiled ...) (~? (~@ #:with with-p.compiled #`with-stx)) ...
                        (~? (~@ #:when condition)) ...
                        #`t*]))

(define-simple-macro (μ* (p:mac-patt ...)
                       (~alt (~seq #:with with-p:mac-patt with-stx:expr)
                             (~seq #:if condition:expr)) ...
                       t:expr ...+)
  #:with t* (simplify #'(t ...))
  (syntax-parser
    [(_ p.compiled ...) (~? (~@ #:with with-p.compiled #`with-stx)) ...
                        (~? (~@ #:when condition)) ...
                        #`t*]))

(define-simple-macro (macro [p:mac-patt
                             (~alt (~seq #:with with-p:mac-patt with-stx:expr)
                                   (~seq #:if condition:expr)) ...
                             t:expr ...+] ...+)
  #:with (t* ...) (stx-map simplify #'((t ...) ...))
  (syntax-parser
    [(_ p.compiled) (~? (~@ #:with with-p.compiled #`with-stx)) ...
                    (~? (~@ #:when condition)) ...
                    #`t*]
    ...))

(define-simple-macro (mu p:mac-patt
                       (~alt (~seq #:with with-p:mac-patt with-stx:expr)
                             (~seq #:if condition:expr)) ...
                       t:expr ...+)
  #:with t* (simplify #'(t ...))
  (syntax-parser
    [(_ p.compiled) (~? (~@ #:with with-p.compiled #`with-stx)) ...
                    (~? (~@ #:when condition)) ...
                    #`t*]))

(define-simple-macro (μ p:mac-patt
                       (~alt (~seq #:with with-p:mac-patt with-stx:expr)
                             (~seq #:if condition:expr)) ...
                       t:expr ...+)
  #:with t* (simplify #'(t ...))
  (syntax-parser
    [(_ p.compiled) (~? (~@ #:with with-p.compiled #`with-stx)) ...
                    (~? (~@ #:when condition)) ...
                    #`t*]))

(define-simple-macro (var id)
  (syntax-local-eval #'id))

(define (macro-expand stx)
  (local-expand stx 'expression null))
