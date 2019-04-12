#lang racket/base

(require algebraic/sum
         racket/format
         (for-syntax racket/base
                     syntax/parse))

(provide (all-defined-out)
         (for-syntax (all-defined-out)))

(struct instance (product args)
  #:transparent
  #:methods gen:custom-write
  [(define (write-proc I port mode)
     (let ([Π (syntax->datum (product-id (instance-product I)))])
       (case mode
         [(#t #f) (write (list* Π (instance-args I)) port)]
         [else (display (list* Π (map ~v (instance-args I))) port)])))])

(struct product (id sum ord hash1 hash2)
  #:transparent
  #:name data:product
  #:constructor-name make-product
  #:property prop:procedure (λ (Π . args) (instance Π args))
  #:methods gen:equal+hash
  [(define (equal-proc Π1 Π2 _)
     (and (equal? (product-sum Π1) (product-sum Π2))
          (= (product-hash1 Π1) (product-hash1 Π2))
          (= (product-hash2 Π1) (product-hash2 Π2))
          (free-identifier=? (product-id Π1) (product-id Π2))))
   (define (hash-proc Π _) (product-hash1 Π))
   (define (hash2-proc Π _) (product-hash2 Π))]
  #:methods gen:custom-write
  [(define (write-proc Π port mode)
     (case mode
       [(#t #f) (fprintf port "#<~a : ~a>"
                         (syntax->datum (product-id Π))
                         (syntax->datum (sum-id (product-sum Π))))]
       [else (display (syntax->datum (product-id Π)) port)]))])

(begin-for-syntax
  (struct product-transformer (product-id sum-id ord hash1 hash2)
    #:transparent
    #:property prop:procedure
    (λ (P stx)
      (syntax-parse stx
        [(_ arg:expr ...)
         #:do [(define product-id (product-transformer-product-id P))]
         #:with Π product-id
         #:with Σ (product-transformer-sum-id P)
         #:with ord (datum->syntax product-id (product-transformer-ord P))
         #:with hash1 (datum->syntax product-id (product-transformer-hash1 P))
         #:with hash2 (datum->syntax product-id (product-transformer-hash2 P))
         #'(instance (make-product #'Π Σ ord hash1 hash2) (list arg ...))]
        [_ #:do [(define product-id (product-transformer-product-id P))]
           #:with Π product-id
           #:with Σ (product-transformer-sum-id P)
           #:with ord (datum->syntax product-id (product-transformer-ord P))
           #:with hash1 (datum->syntax product-id (product-transformer-hash1 P))
           #:with hash2 (datum->syntax product-id (product-transformer-hash2 P))
           #'(make-product #'Π Σ ord hash1 hash2)]))))
