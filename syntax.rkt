#lang racket/base

(require racket/syntax
         syntax/parse
         (for-template racket/base))

(provide (all-defined-out))

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
  [pattern :bytes])

(define-syntax-class wildcard
  (pattern x:id
           #:when (and (not (eq? (syntax->datum #'x) '||))
                       (char=? (first-char #'x) #\_))))

(define-syntax-class variable
  (pattern x:id
           #:when (and (not (eq? (syntax->datum #'x) '||))
                       (char-lower-case? (first-char #'x)))))

(define-syntax-class regex
  (pattern :expr #:when (regexp? (syntax->datum this-syntax))))

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

(define-syntax-class struct-id
  (pattern :id
           #:when (identifier-binding
                   (format-id this-syntax "struct:~a" this-syntax))))

(define-syntax-class prefab-struct
  (pattern #s(key:id item ...)))
