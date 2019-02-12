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

(define-syntax-class regexp
  (pattern x:expr #:when (regexp? (syntax->datum #'x))))

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
  (pattern :expr
           #:do [(define p (syntax-e this-syntax))]
           #:when (pair? p)
           #:with car (car p)
           #:with cdr (cdr p)))

(define-syntax-class vector
  (pattern :expr
           #:do [(define V (syntax-e this-syntax))]
           #:when (vector? V)
           #:with (item:expr ...) (vector->list V)))

(define-syntax-class box
  (pattern :expr
           #:do [(define B (syntax-e this-syntax))]
           #:when (box? B)
           #:with item (unbox B)))

(define-syntax-class hash
  (pattern :expr
           #:do [(define H (syntax-e this-syntax))]
           #:when (hash? H)
           #:do [(define ks (hash-keys H))]
           #:with (k ...) ks
           #:with (v ...) (map (λ (k) (hash-ref H k)) ks)))

(define-syntax-class struct-id
  (pattern :id
           #:when (identifier-binding
                   (format-id this-syntax "struct:~a" this-syntax))))

(define-syntax-class prefab-struct
  (pattern #s(key:id item ...)))
