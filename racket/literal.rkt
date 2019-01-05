#lang racket/base

(require syntax/parse)

(provide racket-literal)

(define-syntax-class racket-literal
  [pattern :boolean]
  [pattern :char]
  [pattern :number]
  [pattern :string]
  [pattern :bytes])
