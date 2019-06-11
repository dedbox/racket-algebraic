#lang algebraic/racket/base

(require (only-in racket/function curry curryr conjoin disjoin)
         (for-syntax algebraic/macro
                     (except-in algebraic/prelude id)
                     algebraic/private
                     algebraic/syntax-list
                     racket/exn
                     syntax/id-table
                     syntax/parse
                     (only-in syntax/parse this-syntax))
         (for-meta 2 algebraic/macro))

(provide (all-defined-out)
         (all-from-out racket/function))

(define-syntax linter-module-begin
  (μ* (form ...)
    #,(begin
        (for-each lint-form (syntax-e #'(form ...)))
        #'(#%module-begin form ...))))

;;; ----------------------------------------------------------------------------

(begin-for-syntax

  (define lint-form
    (syntax-parser
      #:literals (#%expression quote provide require)

      [(~or ((~or (~literal λ) (~literal lambda)) (x:id) y:id)
            ((~or (~literal φ) (~literal phi)) x:id y:id))
       #:when (free-identifier=? #'x #'y)
       (warn this-syntax "Identity function can be shortened to `id'")]

      [((~or (~literal λ) (~literal lambda)) (_) _ ...)
       #:do [(define head (syntax-e (car (syntax-e this-syntax))))]
       (warn this-syntax "Univariate ~a can be shortened to φ" head)]

      [((~or (~literal φ*) (~literal phi*)) (_) _ ...)
       #:do [(define head (syntax-e (car (syntax-e this-syntax))))]
       (warn this-syntax "Univariate ~a can be shortened to φ" head)]

      [((~or (~literal φ*) (~literal phi*)) (:id ...) _ ...)
       #:do [(define head (syntax-e (car (syntax-e this-syntax))))]
       (warn this-syntax "Simple ~a can be shortened to λ" head)]

      [x:id #:when (free-id-table-ref prelude-functions #'x (λ _ #f))
            (warn #'x "Prelude function can be shortened to `~a'"
                  (free-id-table-ref prelude-functions #'x))]

      [(#%expression expr) (lint-form #'expr)]
      [(~or (quote _) ((~or provide require) _ ...)) (void)]
      [(x ...) (syntax-map lint-form this-syntax)]
      [_ (void)]))

  (define prelude-functions
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

  (define (warn ctx msg . vs)
    (with-handlers ([exn? print-warning])
      (raise-syntax-error 'linter ($ format msg vs) ctx)))

  (define (print-warning ex)
    (display (exn->string (exn:fail:syntax (exn-message ex)
                                           (continuation-marks #f)
                                           (exn:fail:syntax-exprs ex)))
             (current-error-port))))
