#lang racket/base

(require algebraic/core/interpret
         algebraic/core/parse
         algebraic/core/show
         syntax/parse/define)

(provide algebraic)

(define-simple-macro (algebraic form)
  (show (interpret (parse 'form))))
