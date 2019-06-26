#lang racket/base

(require (only-in syntax/parse this-syntax)
         syntax/strip-context
         (for-syntax racket/base))

(provide #%rewrite)

(define-syntax (#%rewrite stx)
  (syntax-case stx ()
    [(_ expr) #'(replace-context this-syntax #'expr)]))
