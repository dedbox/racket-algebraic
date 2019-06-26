#lang algebraic/racket/base

(require (only-in racket/function curry curryr conjoin disjoin)
         (for-syntax algebraic/syntax-list
                     racket/exn
                     syntax/id-table
                     syntax/parse))

(provide (all-defined-out)
         (all-from-out racket/function))

;;; ----------------------------------------------------------------------------
;;; Helpers

(define-for-syntax (print-warning ex)
  (display (exn->string (exn:fail:syntax (exn-message ex)
                                         (continuation-marks #f)
                                         (exn:fail:syntax-exprs ex)))
           (current-error-port)))

(define-for-syntax (warn ctx msg . vs)
  (with-handlers ([exn? print-warning])
    (raise-syntax-error 'linter ($ format msg vs) ctx)))

(define-for-syntax prelude-functions
  (make-immutable-free-id-table
   (list (:: #'values   'id)
         (:: #'cons     '::)
         (:: #'list*    '::*)
         (:: #'append   '++)
         (:: #'compose  '..)
         (:: #'conjoin  '&&)
         (:: #'disjion  '||)
         (:: #'apply    '$)
         (:: #'curry    '>>)
         (:: #'curryr   '<<))))

;;; ----------------------------------------------------------------------------
;;; The Linter

(define-for-syntax lint-form
  (macro-parser
   #:literals (λ #%expression quote provide require)

   [(head:id (x:id) y:id)
    #:if (member #'head (list #'φ* #'phi* #'λ #'lambda) free-identifier=?)
    #:if (bound-identifier=? #'x #'y)
    (warn this-syntax "Anonymous identity function: use `id'")]

   [(head:id x:id y:id)
    #:if (member #'head (list #'φ #'phi) free-identifier=?)
    #:if (bound-identifier=? #'x #'y)
    (warn this-syntax "Anonymous identity function: use `id'")]

   [(head:id (_) _ ...)
    #:if (member #'head (list #'φ* #'phi* #'λ #'lambda) free-identifier=?)
    (warn this-syntax "Univariate ~a: use φ" (syntax-e #'head))]

   [(head:id (:id ...+) _ ...)
    #:if (member #'head (list #'φ* #'phi*) free-identifier=?)
    (warn this-syntax "Simple ~a: use λ" (syntax-e #'head))]

   [x:id
    #:if (free-id-table-ref prelude-functions #'x (λ _ #f))
    (warn #'x "Prelude function: use ~a"
          (free-id-table-ref prelude-functions #'x))]

   [(#%expression expr) (lint-form #'expr)]
   [(quote _) (void)]
   [(provide _ ...) (void)]
   [(require _ ...) (void)]
   [(x ...) (syntax-map lint-form this-syntax)]

   [_ (void)]))

(define-syntax linter-module-begin
  (μ* (form ...)
    #:do [(for-each lint-form (syntax-e #'(form ...)))]
    (#%module-begin form ...)))
