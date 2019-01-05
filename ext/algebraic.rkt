#lang racket/base

(require algebraic/core/interpret
         algebraic/ext/parse
         algebraic/ext/show
         syntax/parse/define)

(provide (all-defined-out))

(define-simple-macro (algebraic form)
  (show (interpret (parse 'form))))
