#lang racket/base

(require algebraic/base
         algebraic/class
         algebraic/data
         algebraic/function
         algebraic/prelude
         racket/base
         (for-syntax algebraic/macro
                     algebraic/prelude
                     algebraic/syntax
                     racket/base)
         (for-meta 2 racket/base))

(provide (all-from-out algebraic/base
                       algebraic/class
                       algebraic/data
                       algebraic/function
                       algebraic/prelude)
         (except-out (all-from-out racket/base) #%module-begin)
         (for-syntax (all-from-out algebraic/macro
                                   algebraic/prelude
                                   algebraic/syntax
                                   racket/base))
         (for-meta 2 (all-from-out racket/base))
         (rename-out [module-begin #%module-begin]))

;;; ----------------------------------------------------------------------------

(define-syntax module-begin
  (μ* (form ...)
    #:with (form* ...) (do-instantiate #'(form ...))
    (#%module-begin form* ...)))

(define-for-syntax do-instantiate
  (macro-parser
   #:literals (instantiate)

   [((instantiate prefix instance-id) form ...+)
    #:with (form* ...) (do-instantiate #'(form ...))
    (#%rewrite this-syntax
      (map (λ (form*)
             `(splicing-with-instance [,#'prefix ,#'instance-id] ,form*))
           (syntax-e #'(form* ...))))]

   [((instantiate instance-id) form ...+)
    #:with (form* ...) (do-instantiate #'(form ...))
    (#%rewrite this-syntax
      (map (λ (form*) `(splicing-with-instance ,#'instance-id ,form*))
           (syntax-e #'(form* ...))))]

   [((instantiate _ ...)) #'()]

   [(form0 form ...)
    #:with (form* ...) (do-instantiate #'(form ...))
    #'(form0 form* ...)]

   [() #'()]

   [_ (println `(XXX ,this-syntax)) #'()]))

;;; ----------------------------------------------------------------------------

(module reader syntax/module-reader
  algebraic/racket/base)
