#lang racket/base

(require racket/match
         (for-syntax racket/base
                     racket/syntax
                     syntax/parse
                     syntax/transformer))

(provide (all-defined-out)
         (for-syntax (all-defined-out)))

(begin-for-syntax

  (struct newtype-transformer (type-id pred-id pred ctor-id ctor dtor-id dtor))

  (define (newtype-id? stx)
    (and (identifier? stx)
         (newtype-transformer? (newtype-info stx))))

  (define (newtype-info type-id)
    (syntax-local-value (format-id type-id "newtype:~a" type-id) (λ _ #f)))

  (define newtype-predicate-id
    (compose newtype-transformer-pred-id newtype-info)))

(define-syntax (newtype stx)
  (syntax-parse stx

    [(head type-id:id pred:expr ctor:expr)
     #:with type-id? (format-id #'type-id "~a?" #'type-id)
     #'(head type-id
             #:predicate [type-id? pred]
             #:constructor [type-id ctor])]

    [(head type-id:id pred:expr ctor:expr dtor:expr)
     #:with type-id? (format-id #'type-id "~a?" #'type-id)
     #:with untype-id (format-id #'type-id "un~a" #'type-id)
     #'(head type-id
             #:predicate [type-id? pred]
             #:constructor [type-id ctor]
             #:de-constructor [untype-id dtor])]

    [(head type-id:id
           #:predicate [pred-id:id pred:expr]
           #:constructor [ctor-id:id ctor:expr])
     #:with newtype-id (format-id #'type-id "newtype:~a" #'type-id)
     #'(...
        (begin
          (define-syntax newtype-id
            (newtype-transformer
             #'type-id #'pred-id #'pred #'ctor-id #'ctor #f #f))
          (define-values (pred-id ctor-id) (values pred ctor))))]

    [(_ type-id:id
        #:predicate [pred-id:id pred:expr]
        #:constructor [ctor-id:id ctor:expr]
        #:de-constructor [dtor-id:id dtor:expr])
     #:with newtype-id (format-id #'type-id "newtype:~a" #'type-id)
     #'(...
        (begin
          (define-syntax newtype-id
            (newtype-transformer
             #'type-id #'pred-id #'pred #'ctor-id #'ctor #'dtor-id #'dtor))
          (define-values (pred-id ctor-id dtor-id) (values pred ctor dtor))))]))

(module+ test
  (require racket/function
           rackunit)

  (newtype List list? list (curry apply values))

  (check-pred null? (List))
  (check-pred list? (List 1 2 3))
  (check-pred List? '(4 5 6))
  (check-pred List? (List 7 8 9))
  (check-true (match '(1) [(? List?) #t] [_ #f]))
  (check-false (match 1 [(? List?) #t] [_ #f]))
  (check = (unList '(1)) 1)
  (check = (call-with-values (λ () (unList (List 1 2 3))) +) 6))
