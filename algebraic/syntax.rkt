#lang racket/base

(require (only-in syntax/parse this-syntax)
         (for-syntax racket/base))

(provide #%rewrite this-syntax)

(define-syntax (#%rewrite stx)
  (syntax-case stx ()
    [(_ src expr)
     (datum->syntax stx #'(datum->syntax src expr src src) stx stx)]))
