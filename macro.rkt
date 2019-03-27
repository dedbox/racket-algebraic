#lang racket/base

(require algebraic/data
         racket/syntax
         syntax/parse
         syntax/parse/define
         (for-syntax algebraic/syntax
                     racket/syntax
                     syntax/parse
                     syntax/parse/define))

(provide (all-defined-out))

(begin-for-syntax
  (define-syntax-class mac-quote
    #:description "quoted data"
    #:attributes (compiled)
    #:literals (quote)
    (pattern (quote datum) #:attr compiled #'(quote (~datum datum))))

  (define-syntax-class mac-wildcard
    #:description "wildcard"
    #:attributes (compiled)
    #:literals (_)
    (pattern x:wildcard #:attr compiled #'_))

  (define-syntax-class mac-variable
    #:description "variable"
    #:attributes (compiled)
    (pattern (~and compiled:variable (~not :wildcard))))

  (define-syntax-class mac-product
    #:description "product pattern"
    #:attributes (compiled)
    (pattern (~and Π1:id (~not (~or :wildcard :variable)))
             #:when (product-id? #'Π1)
             #:with product-id?* (syntax-shift-phase-level #'product-id? -1)
             #:attr compiled
             #'(~and Π2:id
                     (~fail #:unless (and (product-id?* #'Π2)
                                          (equal? (syntax-local-value #'Π1)
                                                  (syntax-local-value #'Π2)))
                            (format "expected the product ~a" 'Π1)))))

  (define-syntax-class mac-identifier
    #:description "identifier"
    #:attributes (compiled)
    (pattern (~and x:id (~not (~or :wildcard :variable :mac-product)))
             #:when (identifier-template-binding #'x)
             #:attr compiled #`(~literal #,this-syntax)))

  (define-syntax-class mac-symbol
    #:description "literal symbol"
    #:attributes (compiled)
    (pattern (~and x:id (~not (~or :variable :wildcard :mac-product :mac-identifier)))
             #:attr compiled #`(~datum #,this-syntax)))

  (define-syntax-class mac-instance
    #:description "instance pattern"
    #:attributes (compiled)
    (pattern (Π:mac-product ~! arg:mac-patt ...)
             #:attr compiled
             #'(~describe (format "an instance of product ~a" (syntax->datum #'Π))
                          (Π.compiled arg.compiled ...))))

  (define-syntax-class mac-pair
    #:description "macro pattern sequence"
    #:attributes (compiled)
    (pattern () #:attr compiled #'())
    (pattern (car:mac-patt . cdr:mac-patt)
             #:attr compiled #'(car.compiled . cdr.compiled)))

  (define-syntax-class mac-vector
    #:description "vector macro pattern"
    #:attributes (compiled)
    (pattern e:expr
             #:do [(define V (syntax-e #'e))]
             #:when (vector? V)
             #:with (p:mac-patt ...) (vector->list V)
             #:attr compiled #'(~describe "vector" #(p.compiled ...))))

  (define-syntax-class mac-box
    #:description "box macro pattern"
    #:attributes (compiled)
    (pattern e:expr
             #:do [(define B (syntax-e #'e))]
             #:when (box? B)
             #:with p:mac-patt (unbox B)
             #:attr compiled #'(~describe "box" #&p.compiled)))

  ;; (define-syntax-class hash-key
  ;;   #:description "hash macro pattern key"
  ;;   #:commit
  ;;   (pattern :id)
  ;;   (pattern :literal-value))

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
    (pattern (s:id p:mac-patt ...)
             #:when (identifier-template-binding (format-id #'s "struct:~a" #'s))
             #:attr compiled
             #'(~describe (format "an instance of struct ~a" 's)
                          ((~describe "struct name" (~literal s))
                           (~describe "struct field" p.compiled) ...))))

  (define-syntax-class mac-quasiquoted
    #:attributes (compiled)
    #:literals (unquote)
    (pattern x:id #:attr compiled #`(~datum x))
    (pattern (unquote ~! p:mac-patt) #:attr compiled #'p.compiled)
    (pattern () #:attr compiled #'())
    (pattern (q1:mac-quasiquoted . q2:mac-quasiquoted)
             #:attr compiled #'(q1.compiled . q2.compiled))
    (pattern compiled))

  (define-syntax-class mac-quasiquote
    #:description "quasiquoted macro pattern"
    #:attributes (compiled)
    #:literals (quasiquote)
    (pattern (quasiquote datum:mac-quasiquoted)
             #:attr compiled #'datum.compiled))

  (define-syntax-class mac-patt
    #:description "macro pattern"
    #:attributes (compiled)
    (pattern (~literal ...) #:attr compiled this-syntax)
    (pattern (~literal ...+) #:attr compiled this-syntax)
    (pattern compiled:literal-value)
    (pattern (~or p:mac-quote
                  p:mac-wildcard
                  p:mac-variable
                  p:mac-product
                  p:mac-identifier
                  p:mac-symbol
                  p:mac-instance
                  p:mac-struct
                  p:mac-quasiquote
                  p:mac-pair
                  p:mac-vector
                  p:mac-box)
             #:attr compiled #'p.compiled))

  (define-syntax-class mac-arg
    #:description "macro argument"
    #:attributes (compiled)
    (pattern p:mac-patt #:attr compiled #'p.compiled))

  (define-syntax-class mac-rest-arg
    #:description "macro rest-argument"
    #:attributes (compiled)
    (pattern p:mac-patt #:attr compiled #'p.compiled))

  (define-syntax-class mac-body
    #:description "macro body"
    #:attributes ()
    (pattern :expr))

  (define-syntax-class if-expr
    #:description "condition"
    #:attributes ()
    (pattern :expr))

  (define (simplify ts)
    (if (null? (cdr ts))
        (car ts)
        (datum->syntax (car ts) (list* 'begin ts)))))

;; ---------------------------------------------------------------------------

(define-simple-macro (macro* (~describe
                              "macro clause"
                              [(~or (~and rest-id:id (~or :variable :wildcard))
                                    (p:mac-arg ...)
                                    (p:mac-arg ...+ . rest-p:mac-rest-arg))
                               (~alt (~seq #:with with-p:mac-patt with-stx:expr)
                                     (~seq #:if condition:if-expr)
                                     (~seq #:as as-p:mac-patt))
                               ...
                               body:mac-body ...+])
                             ...+)
  #:with (body* ...) (map simplify (attribute body))
  (syntax-parser
    [(~? (_ . (~? (~and rest-id as-p.compiled ...) rest-id))
         (~? (~? (~and (_ p.compiled ... . rest-p.compiled)
                       (_ . as-p.compiled)
                       ...)
                 (_ p.compiled ... . rest-p.compiled))
             (~? (~and (_ p.compiled ...)
                       (_ . as-p.compiled)
                       ...)
                 (_ p.compiled ...))))
     (~? (~@ #:with with-p.compiled #`with-stx)) ...
     (~? (~@ #:post (~fail #:unless condition
                           (format "condition failed: ~a" 'condition)))) ...
     #`body*]
    ...))

(define-simple-macro (mu* (~or (~and rest-id:id (~or :variable :wildcard))
                               (p:mac-arg ...)
                               (p:mac-arg ...+ . rest-p:mac-rest-arg))
                       (~alt (~seq #:with with-p:mac-patt with-stx:expr)
                             (~seq #:if condition:if-expr)
                             (~seq #:as as-p:mac-patt))
                       ...
                       body:mac-body ...+)
  #:with body* (simplify (attribute body))
  (syntax-parser
    [(~? (_ . (~? (~and rest-id as-p.compiled ...) rest-id))
         (~? (~? (~and (_ p.compiled ... . rest-p.compiled)
                       (_ . as-p.compiled)
                       ...)
                 (_ p.compiled ... . rest-p.compiled))
             (~? (~and (_ p.compiled ...)
                       (_ . as-p.compiled)
                       ...)
                 (_ p.compiled ...))))
     (~? (~@ #:with with-p.compiled #`with-stx)) ...
     (~? (~@ #:post (~fail #:unless condition
                           (format "condition failed: ~a" 'condition)))) ...
     #`body*]))

(define-simple-macro (μ* (~or (~and rest-id:id (~or :variable :wildcard))
                              (p:mac-arg ...)
                              (p:mac-arg ...+ . rest-p:mac-rest-arg))
                       (~alt (~seq #:with with-p:mac-patt with-stx:expr)
                             (~seq #:if condition:if-expr)
                             (~seq #:as as-p:mac-patt))
                       ...
                       body:mac-body ...+)
  #:with body* (simplify (attribute body))
  (syntax-parser
    [(~? (_ . (~? (~and rest-id as-p.compiled ...) rest-id))
         (~? (~? (~and (_ p.compiled ... . rest-p.compiled)
                       (_ . as-p.compiled)
                       ...)
                 (_ p.compiled ... . rest-p.compiled))
             (~? (~and (_ p.compiled ...)
                       (_ . as-p.compiled)
                       ...)
                 (_ p.compiled ...))))
     (~? (~@ #:with with-p.compiled #`with-stx)) ...
     (~? (~@ #:post (~fail #:unless condition
                           (format "condition failed: ~a" 'condition)))) ...
     #`body*]))

(define-simple-macro (macro [p:mac-arg
                             (~alt (~seq #:with with-p:mac-patt with-stx:expr)
                                   (~seq #:if condition:if-expr)
                                   (~seq #:as as-p:mac-patt))
                             ...
                             body:mac-body ...+] ...+)
  #:with (body* ...) (map simplify (attribute body))
  (syntax-parser
    [(_ (~describe "macro argument" (~? (~and p.compiled as-p.compiled ...)
                                        p.compiled)))
     (~? (~@ #:with with-p.compiled #`with-stx)) ...
     (~? (~@ #:post (~fail #:unless condition
                           (format "condition failed: ~a" 'condition)))) ...
     #`body*]
    ...))

(define-simple-macro (mu p:mac-arg
                       (~alt (~seq #:with with-p:mac-patt with-stx:expr)
                             (~seq #:if condition:if-expr)
                             (~seq #:as as-p:mac-patt))
                       ...
                       body:mac-body ...+)
  #:with body* (simplify (attribute body))
  (syntax-parser
    [(_ (~describe "macro argument" (~? (~and p.compiled as-p.compiled ...)
                                        p.compiled)))
     (~? (~@ #:with with-p.compiled #`with-stx)) ...
     (~? (~@ #:post (~fail #:unless condition
                           (format "condition failed: ~a" 'condition)))) ...
     #`body*]))

(define-simple-macro (μ p:mac-arg
                       (~alt (~seq #:with with-p:mac-patt with-stx:expr)
                             (~seq #:if condition:if-expr)
                             (~seq #:as as-p:mac-patt))
                       ...
                       body:mac-body ...+)
  #:with body* (simplify (attribute body))
  (syntax-parser
    [(_ (~describe "macro argument" (~? (~and p.compiled as-p.compiled ...)
                                        p.compiled)))
     (~? (~@ #:with with-p.compiled #`with-stx)) ...
     (~? (~@ #:post (~fail #:unless condition
                           (format "condition failed: ~a" 'condition)))) ...
     #`body*]))

(define-simple-macro (var id)
  (syntax-local-eval #'id))
