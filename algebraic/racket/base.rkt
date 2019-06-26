#lang racket/base

(require algebraic/class
         algebraic/data
         algebraic/function
         algebraic/prelude
         (for-syntax algebraic/macro
                     algebraic/prelude
                     racket/base)
         (for-meta 2 racket/base))

(provide (all-from-out algebraic/class
                       algebraic/data
                       algebraic/function
                       algebraic/prelude)
         (except-out (all-from-out racket/base) #%module-begin)
         (for-syntax (all-from-out algebraic/macro
                                   algebraic/prelude
                                   racket/base))
         (for-meta 2 (all-from-out racket/base))
         (rename-out [module-begin #%module-begin]))

;;; ----------------------------------------------------------------------------

(require (for-syntax algebraic/syntax
                     (except-in syntax/parse id)
                     (rename-in syntax/parse [id id~])))

(define-syntax module-begin
  (μ* (form ...+)
    #:with (form* ...) (do-instantiate #'(form ...))
    (#%module-begin form* ...)))

(define-for-syntax (do-instantiate forms)
  (syntax-parse forms
    #:literals (instantiate)
    [((instantiate prefix:id~ instance-id:id~) form0 form ...)
     (with-syntax ([(form* ...) (do-instantiate #'(form0 form ...))])
       (#%rewrite ((splicing-with-instance [prefix instance-id] form* ...))))]
    [((instantiate instance-id:id~) form0 form ...)
     (with-syntax ([(form* ...) (do-instantiate #'(form0 form ...))])
       (#%rewrite ((splicing-with-instance instance-id form* ...))))]
    [((instantiate _ ...))
     (raise-syntax-error #f "Cannot be the last form of a module" this-syntax)]
    [(form0 form ...)
     (with-syntax ([(form* ...) (do-instantiate #'(form ...))])
       (#%rewrite (form0 form* ...)))]
    [() (#%rewrite ())]))

;;; ----------------------------------------------------------------------------

(module reader syntax/module-reader
  algebraic/racket/base)
