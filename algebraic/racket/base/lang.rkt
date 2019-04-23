#lang racket/base

(require algebraic/racket/base)

(provide
 (all-from-out algebraic/racket/base)
 (except-out (all-from-out racket/base) #%module-begin)
 (rename-out [algebraic-module-begin #%module-begin]))

(define-syntax (algebraic-module-begin stx)
  (syntax-case stx ()
    [(_ form ...)
     (begin
       ;; (println (local-expand #'(#%plain-module-begin form ...) 'module-begin #f))
       #'(#%module-begin form ...))]))
