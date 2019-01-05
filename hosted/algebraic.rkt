#lang racket/base

(require algebraic/hosted/interpret
         algebraic/hosted/parse
         algebraic/hosted/show
         syntax/parse/define)

(provide (all-defined-out))

(define-simple-macro (algebraic form)
  (show (interpret (parse 'form))))
