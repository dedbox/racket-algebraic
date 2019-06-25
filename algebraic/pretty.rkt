#lang racket/base

(require racket/pretty)

(provide (all-defined-out))

(define algebraic-pretty-print-style-table
  (pretty-print-extend-style-table
   (pretty-print-current-style-table)
   '(class
      φ phi φ* phi* function function*
      μ mu μ* mu* macro macro*
      data class instance)
   '(case
        lambda lambda lambda lambda case-lambda case-lambda
        lambda lambda lambda lambda case-lambda case-lambda
        case case case)))
