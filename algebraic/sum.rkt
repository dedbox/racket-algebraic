#lang racket/base

(require algebraic/pretty
         racket/contract/base
         racket/pretty
         (for-syntax racket/base)
         (for-meta 2 racket/base))

(provide
 sum
 (contract-out
  [struct data:sum ([id identifier?]
                    [product-ids (listof identifier?)]
                    [hash1 fixnum?]
                    [hash2 fixnum?])]))

(begin-for-syntax
  (require racket/contract/base)

  (provide
   (contract-out
    [sum-introducer (-> syntax? 'add syntax?)]
    [struct sum-transformer ([id identifier?]
                             [product-ids (listof identifier?)]
                             [hash1 fixnum?]
                             [hash2 fixnum?])]))

  (define sum-introducer (make-syntax-introducer))

  (struct sum-transformer (id product-ids hash1 hash2)
    #:transparent
    #:property prop:procedure
    (λ (S stx)
      (define id (sum-transformer-id S))
      (with-syntax ([(Σ Π ...) (cons id (sum-transformer-product-ids S))]
                    [hash1 (datum->syntax id (sum-transformer-hash1 S))]
                    [hash2 (datum->syntax id (sum-transformer-hash2 S))])
        #'(make-sum #'Σ (list #'Π ...) hash1 hash2)))))

(define-syntax (sum stx)
  (syntax-case stx ()
    [(_ id) (identifier? #'id) (sum-introducer #'id 'add)]))

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

   (define (hash-proc Σ _)
     (sum-hash1 Σ))

   (define (hash2-proc Σ _)
     (sum-hash2 Σ))]

  #:methods gen:custom-write

  [(define (write-proc Σ port mode)
     (case mode
       [(#t #f) (fprintf port "#<sum:~a:~a ~a>"
                         (sum-hash1 Σ)
                         (sum-hash2 Σ)
                         (syntax->datum (sum-id Σ)))]
       [else (parameterize ([pretty-print-current-style-table
                             algebraic-pretty-print-style-table])
               (print `(data ,(syntax->datum (sum-id Σ))
                             (,@(map syntax->datum (sum-product-ids Σ))))
                      port 1))]))])

;;; ----------------------------------------------------------------------------

;; (module+ test
;;   (require algebraic/testing
;;            rackunit
;;            syntax/macro-testing)

;;   (test-case "sum-introducer"
;;     (sum (define x OK))
;;     (check-OK (sum x)))

;;   (test-case "sum-transformer"
;;     (let-syntax ([Σ (sum-transformer #f #f #f #f)])
;;       (check-pred (phase1-eval sum-transformer?)
;;                   (phase1-eval (syntax-local-value #'Σ)))
;;       (check eq?
;;              (phase1-eval (syntax-local-value #'Σ))
;;              (phase1-eval (syntax-local-value #'Σ)))))

;;   (test-case "sum"
;;     (let-syntax ([Σ (sum-transformer #'Σ (list #'Π1 #'Π2) 1 2)])
;;       (check-pred sum? Σ)
;;       (check equal? Σ Σ)
;;       (check equal? (format "~v" Σ) "(data Σ (Π1 Π2))")
;;       (check equal? (format "~a" Σ) "#<sum:1:2 Σ>"))))
