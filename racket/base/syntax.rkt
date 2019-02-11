#lang racket/base

(require (for-template racket/base)
         racket/syntax
         syntax/parse)

(provide (all-defined-out))

(struct scon (δ)
  #:transparent
  #:property prop:procedure
  (λ (c stx)
    (syntax-parse stx
      [(_ v ...) #`(ins (con '#,(scon-δ c)) (list v ...))]
      [_ #`(con '#,(scon-δ c))])))

(define (first-char id-stx)
  (string-ref (symbol->string (syntax->datum id-stx)) 0))

(define-syntax-class literal-value
  #:description "literal value"
  #:attributes ()
  #:literals (quote)
  [pattern :boolean]
  [pattern :char]
  [pattern :number]
  [pattern :string]
  [pattern :bytes]
  [pattern (quote datum)])

(define-syntax-class wildcard
  (pattern x:id
           #:when (and (not (eq? (syntax->datum #'x) '||))
                       (char=? (first-char #'x) #\_))))

(define-syntax-class variable
  (pattern x:id
           #:when (and (not (eq? (syntax->datum #'x) '||))
                       (char-lower-case? (first-char #'x)))))

(define-syntax-class void
  #:literals (void)
  (pattern (void)))

(define-syntax-class constructor
  (pattern δ:id
           #:when (and (identifier-transformer-binding #'δ)
                       (namespace-variable-value (syntax->datum this-syntax) #t (λ _ #f))
                       (scon? (syntax-local-eval #'δ)))))

(define-syntax-class reference
  (pattern :id
           #:do [(define s (syntax->datum this-syntax))]
           #:when (and
                   (or (eq? s '||)
                       (and (not (char-lower-case? (first-char this-syntax)))
                            (not (char=? #\_ (first-char this-syntax)))))
                   (namespace-variable-value s #t (λ _ #f)))))

(define-syntax-class conditional
  (pattern (:patt #:if :expr)))

(define-syntax-class instance
  (pattern (:constructor :patt ...))
  (pattern (:constructor . :patt)))

(define-syntax-class regexp
  (pattern x:expr #:when (regexp? (syntax->datum #'x)))
  (pattern (x:expr p:patt ...+) #:when (regexp? (syntax->datum #'x))))

(define maybe-quote
  (syntax-parser
    [(a ...) #''(a ...)]
    [x:id #''x]
    [_ this-syntax]))

(define maybe-quote/ids
  (syntax-parser
    #:literals (quote)
    [(quote ~! _) this-syntax]
    [(a ...) #'(quote (a ...))]
    [_ this-syntax]))

(define-syntax-class pair
  #:attributes (car cdr)
  (pattern :expr
           #:when (null? (syntax-e this-syntax))
           #:with car #'#f
           #:with cdr #'#f)
  (pattern :expr
           #:when (pair? (syntax-e this-syntax))
           #:with car:patt (car (syntax-e this-syntax))
           #:with cdr:patt (cdr (syntax-e this-syntax))))

(define-syntax-class vector
  (pattern :expr
           #:when (vector? (syntax-e this-syntax))
           #:with (p:patt ...) (map maybe-quote/ids
                                    (vector->list (syntax-e this-syntax)))))

(define-syntax-class box
  (pattern :expr
           #:when (box? (syntax-e this-syntax))
           #:with p:patt (maybe-quote/ids (unbox (syntax-e this-syntax)))))

(define-syntax-class hash-key
  (pattern :id)
  (pattern :literal-value))

(define-syntax-class hash
  (pattern :expr
           #:do [(define H (syntax-e this-syntax))]
           #:when (hash? H)
           #:with ([k:hash-key . v:patt] ...)
           (let ([ks (hash-keys H)])
                (map cons
                     (map maybe-quote/ids ks)
                     (map (λ (k) (maybe-quote/ids (hash-ref H k))) ks)))))

(define-syntax-class struct-id
  (pattern :id
           #:when (identifier-binding
                   (format-id this-syntax "struct:~a" this-syntax))))

(define-syntax-class struct
  (pattern (:struct-id ([:id :patt] ...)))
  (pattern (:struct-id :patt ...)))

(define-syntax-class patt
  #:description "function pattern"
  #:commit
  (pattern :literal-value)
  (pattern :wildcard)
  (pattern :variable)
  (pattern :void)
  (pattern :constructor)
  (pattern :reference)
  (pattern :conditional)
  (pattern :instance)
  (pattern :regexp)
  (pattern :struct)
  (pattern :vector)
  (pattern :box)
  (pattern :hash)
  (pattern :pair))
