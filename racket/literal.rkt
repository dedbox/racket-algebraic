#lang racket/base

(require syntax/parse)

(provide host-literal)

(define-syntax-class host-literal
  #:description "literal"
  #:attributes ()
  [pattern :boolean]
  [pattern :char]
  [pattern :number]
  [pattern :string]
  [pattern :bytes])
