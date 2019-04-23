#lang racket/base

(require algebraic/data/sum
         racket/contract/base
         racket/format
         (for-syntax racket/base))

(provide
 (contract-out
  [struct data:product ([id identifier?]
                        [sum sum?]
                        [ordinal exact-nonnegative-integer?]
                        [hash1 fixnum?]
                        [hash2 fixnum?])
    #:omit-constructor]
  [make-product (-> identifier? sum? exact-nonnegative-integer? fixnum? fixnum?
                    product?)]
  [struct instance ([product product?]
                    [arguments list?])]))

(begin-for-syntax
  (require racket/contract/base)

  (provide
   (contract-out
    [struct product-transformer
      ([id identifier?]
       [sum-id identifier?]
       [ordinal exact-nonnegative-integer?]
       [hash1 fixnum?]
       [hash2 fixnum?])]
    [product-transform (-> product-transformer? syntax?)]))

  (struct product-transformer (id sum-id ordinal hash1 hash2)
    #:transparent
    #:property prop:procedure
    (λ (P stx)
      (syntax-case stx ()
        [(_ arg ...) #`(instance #,(product-transform P) (list arg ...))]
        [_ (product-transform P)])))

  (define (product-transform P)
    (let ([id (product-transformer-id P)])
      (with-syntax ([(Π Σ) (list id (product-transformer-sum-id P))]
                    [ordinal (datum->syntax id (product-transformer-ordinal P))]
                    [hash1 (datum->syntax id (product-transformer-hash1 P))]
                    [hash2 (datum->syntax id (product-transformer-hash2 P))])
        #'(make-product #'Π Σ ordinal hash1 hash2)))))

(struct product (id sum ordinal hash1 hash2)
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

   (define (hash-proc Π _)
     (product-hash1 Π))

   (define (hash2-proc Π _)
     (product-hash2 Π))]

  #:methods gen:custom-write

  [(define (write-proc Π port mode)
     (case mode
       [(#t #f) (fprintf port "#<product:~a:~a:~a ~a∈~a>"
                         (product-ordinal Π)
                         (product-hash1 Π)
                         (product-hash2 Π)
                         (syntax->datum (product-id Π))
                         (syntax->datum (sum-id (product-sum Π))))]
       [else (display (syntax->datum (product-id Π)) port)]))])

(struct instance (product arguments)
  #:transparent
  #:methods gen:custom-write
  [(define (write-proc I port mode)
     (let ([Π (syntax->datum (product-id (instance-product I)))])
       (case mode
         [(#t #f) (write (list* Π (instance-arguments I)) port)]
         [else (display (list* Π (map ~v (instance-arguments I))) port)])))])

;;; ----------------------------------------------------------------------------

;; (module+ test
;;   (require algebraic/testing
;;            rackunit
;;            syntax/macro-testing)

;;   (test-case "product-transformer"
;;     (let-syntax ([Σ (sum-transformer #'Σ null 0 0)])
;;       (let-syntax ([Π (product-transformer #'Π #'Σ 0 0 0)])
;;         (check-pred (phase1-eval product-transformer?)
;;                     (phase1-eval (syntax-local-value #'Π)))
;;         (check eq?
;;                (phase1-eval (syntax-local-value #'Π))
;;                (phase1-eval (syntax-local-value #'Π))))))

;;   (test-case "product"
;;     (let-syntax ([Σ (sum-transformer #'Σ (list #'Π) 1 2)])
;;       (let-syntax ([Π (product-transformer #'Π #'Σ 0 1 2)])
;;         (check-pred product? Π)
;;         (check equal? Π Π)
;;         (check equal? (format "~v" Π) "Π")
;;         (check equal? (format "~a" Π) "#<product:0:1:2 Π∈Σ>"))))

;;   (test-case "instance"
;;     (let-syntax ([Σ (sum-transformer #'Σ (list #'Π1 #'Π2) 1 2)])
;;       (let-syntax ([Π1 (product-transformer #'Π1 #'Σ 0 1 2)]
;;                    [Π2 (product-transformer #'Π2 #'Σ 1 2 3)])
;;         (check-pred instance? (Π1))
;;         ;; (check-pred instance? (Π 1 2 3))
;;         (check equal? (Π1 1 2 3) (Π1 1 2 3))
;;         (check-false (equal? (Π1 1 2 3) (Π1 4 5 6)))
;;         (check-false (equal? (Π1 1 2 3) (Π2 1 2 3)))
;;         (check equal? (format "~v" (Π1)) "(Π1)")
;;         (check equal? (format "~a" (Π1)) "(Π1)")
;;         (check equal? (format "~v" (Π1 1 2 3)) "(Π1 1 2 3)")
;;         (check equal? (format "~a" (Π1 1 2 3)) "(Π1 1 2 3)")))))
