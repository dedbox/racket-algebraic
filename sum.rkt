#lang racket/base

(require syntax/parse/define
         (for-syntax racket/base
                     syntax/parse))

(provide (all-defined-out)
         (for-syntax (all-defined-out)))

(struct sum (id product-ids hash1 hash2)
  #:transparent
  #:name data:sum
  #:constructor-name make-sum
  #:property prop:procedure (λ _ (error "cannot instantiate sum type"))
  #:methods gen:equal+hash
  [(define (equal-proc Σ1 Σ2 _)
     (and (= (sum-hash1 Σ1) (sum-hash1 Σ2))
          (= (sum-hash2 Σ1) (sum-hash2 Σ2))
          (free-identifier=? (sum-id Σ1) (sum-id Σ2))))
   (define (hash-proc Σ _) (sum-hash1 Σ))
   (define (hash2-proc Σ _) (sum-hash2 Σ))]
  #:methods gen:custom-write
  [(define (write-proc Σ port mode)
     (case mode
       [(#t #f) (fprintf port "#<sum:~a:~a ~a>"
                         (sum-hash1 Σ)
                         (sum-hash2 Σ)
                         (syntax->datum (sum-id Σ))
                         ;; (string-join (map (λ (Π) (format "~v" (syntax->datum Π)))
                         ;;                   (sum-product-ids Σ)))
                         )]
       [else (display `(data ,(syntax->datum (sum-id Σ))
                             (,@(map syntax->datum (sum-product-ids Σ))))
                      port)]))])

(begin-for-syntax
  (define sum-introducer (make-syntax-introducer))

  (struct sum-transformer (sum-id product-ids hash1 hash2)
    #:transparent
    #:property prop:procedure
    (λ (S stx)
      (syntax-parse stx
        [(_ _ ...) (raise-syntax-error #f "cannot instantiate sum" stx)]
        [_ #:do [(define sum-id (sum-transformer-sum-id S))]
           #:with Σ (sum-transformer-sum-id S)
           #:with (Π ...) (sum-transformer-product-ids S)
           #:with hash1 (datum->syntax sum-id (sum-transformer-hash1 S))
           #:with hash2 (datum->syntax sum-id (sum-transformer-hash2 S))
           #'(make-sum #'Σ (list #'Π ...) hash1 hash2)]))))

(define-simple-macro (sum expr)
  #:with expr* (sum-introducer #'expr 'add)
  expr*)

(define ((member-id-pred ids) id)
  (and (member id ids free-identifier=?) #t))
