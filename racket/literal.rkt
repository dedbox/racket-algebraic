#lang racket/base

(require (for-template racket/base)
         syntax/parse)

(provide host-literal)

(define-syntax-class host-literal
  #:description "literal"
  #:attributes ()
  #:literals (quote)
  [pattern :boolean]
  [pattern :char]
  [pattern :number]
  [pattern :string]
  [pattern :bytes]
  [pattern :char]
  [pattern (quote datum)])
