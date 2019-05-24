#lang racket/base

(require algebraic/product
         algebraic/sum
         racket/contract/base
         (for-syntax algebraic/product
                     algebraic/sum
                     racket/base
                     racket/list
                     racket/syntax)
         (for-meta 2 racket/base))

(provide
 (all-from-out algebraic/product
               algebraic/sum)
 data
 ;; let-data with-data
 (contract-out
  [data-less-than? (-> product? product? boolean?)]
  [data->list (-> any/c list?)]))

(define-syntax (data stx)
  (define (make-hash1 stx)
    (datum->syntax stx (equal-hash-code stx)))
  (define (make-hash2 stx)
    (datum->syntax stx (equal-secondary-hash-code stx)))
  (syntax-case stx ()
    [(_) #'(void)]
    [(_ Σ (Π ...))
     (andmap identifier? (syntax-e #'(Σ Π ...)))
     (let ([Πs (syntax-e #'(Π ...))])
       (with-syntax ([Σ* (sum-introducer #'Σ 'add)])
         (with-syntax ([Σ-hash1 (make-hash1 #'Σ*)]
                       [Σ-hash2 (make-hash2 #'Σ*)]
                       [(Σ*/Π ...) (make-list (length Πs) #'Σ*)]
                       [(ordinal ...) (build-list (length Πs) values)]
                       [(Π-hash1 ...) (map make-hash1 Πs)]
                       [(Π-hash2 ...) (map make-hash2 Πs)]
                       [Σ*? (sum-introducer (format-id #'Σ* "~a?" #'Σ*) 'add)]
                       [(Π? ...) (map (λ (stx) (format-id stx "~a?" stx)) Πs)])
           (with-syntax ([Σ-trans #'(sum-transformer
                                     #'Σ* (list #'Π ...) Σ-hash1 Σ-hash2)]
                         [(Π-trans ...) #'((product-transformer
                                            #'Π #'Σ*/Π ordinal Π-hash1 Π-hash2) ...)])
             #'(begin
                 (begin-for-syntax
                   (define-syntax Σ* Σ-trans)
                   (begin (define-syntax Π Π-trans) ...))
                 (define-syntax Σ* Σ-trans)
                 (begin (define-syntax Π Π-trans) ...)
                 (define Σ*? (sum-pred Σ*))
                 (begin (define Π? (product-pred Π)) ...))))))]
    [(_ Σ (Π ...) . tail) #'(begin (data Σ (Π ...)) (data . tail))]))

(define ((sum-pred Σ) x)
  (cond [(sum? x) (equal? x Σ)]
        [(product? x) (equal? (product-sum x) Σ)]
        [(product-instance? x)
         (equal? (product-sum (product-instance-product x)) Σ)]
        [else #f]))

(define ((product-pred Π) x)
  (cond [(product? x) (equal? x Π)]
        [(product-instance? x) (equal? (product-instance-product x) Π)]
        [else #f]))

;; (define-syntax (let-data stx)
;;   (define (make-hash1 stx)
;;     (datum->syntax stx (equal-hash-code stx)))
;;   (define (make-hash2 stx)
;;     (datum->syntax stx (equal-secondary-hash-code stx)))

;;   (syntax-case stx ()
;;     [(_ ([Σ (Π ...)]) body ...)
;;      (with-syntax ([Σ* (sum-introducer #'Σ 'add)])
;;        (with-syntax ([Σ-hash1 (make-hash1 #'Σ*)]
;;                      [Σ-hash2 (make-hash2 #'Σ*)])
;;          (println
;;           (syntax-local-eval
;;            #''#`(let-syntax ([Σ* (sum-transformer #'Σ* (list #'Π ...) Σ-hash1 0)])
;;                  #,(let-syntax ([Σ* (sum-transformer #'Σ* (list #'Π ...) Σ-hash1 1)])
;;                      body ...))))

;;          ;; (syntax-local-eval
;;          ;;  #`#'(let-syntax ([Σ* (sum-transformer #'Σ* (list #'Π ...) Σ-hash1 0)])
;;          ;;        #,(syntax-local-eval
;;          ;;           #'(let-syntax ([Σ* (sum-transformer #'Σ* (list #'Π ...) Σ-hash1 1)])
;;          ;;               #'(begin body ...)))))

;;          ))



;;      ;; #'(let ([Πs (syntax-e #'(Π ...))])
;;      ;;     (with-syntax ([Σ* (sum-introducer #'Σ 'add)])
;;      ;;       (with-syntax ([Σ-hash1 (make-hash1 #'Σ*)]
;;      ;;                     [Σ-hash2 (make-hash2 #'Σ*)])
;;      ;;         #`(let-syntax ([Σ* (sum-transformer #'Σ* (list #'Π ...) Σ-hash1 Σ-hash2)])
;;      ;;             #,(let-syntax ([Σ* (sum-transformer #'Σ* (list #'Π ...) Σ-hash1 Σ-hash2)])
;;      ;;                 (begin body ...))))))



;;      ;; (let ([Πs (syntax-e #'(Π ...))])
;;      ;;   (with-syntax ([Σ* (sum-introducer #'Σ 'add)])
;;      ;;     (with-syntax ([Σ-hash1 (make-hash1 #'Σ*)]
;;      ;;                   [Σ-hash2 (make-hash2 #'Σ*)]
;;      ;;                   [(Σ*/Π ...) (make-list (length Πs) #'Σ*)]
;;      ;;                   [(ordinal ...) (build-list (length Πs) values)]
;;      ;;                   [(Π-hash1 ...) (map make-hash1 Πs)]
;;      ;;                   [(Π-hash2 ...) (map make-hash2 Πs)]
;;      ;;                   [Σ*? (sum-introducer (format-id #'Σ* "~a?" #'Σ*) 'add)]
;;      ;;                   [(Π? ...) (map (λ (stx) (format-id stx "~a?" stx)) Πs)])
;;      ;;       #'(let-syntax
;;      ;;             ([Σ* (sum-transformer #'Σ* (list #'Π ...) Σ-hash1 Σ-hash2)]
;;      ;;              [Π (product-transformer #'Π #'Σ*/Π ordinal Π-hash1 Π-hash2)]
;;      ;;              ...
;;      ;;              [go (λ (stx)
;;      ;;                    (let-syntax
;;      ;;                        ([Σ* (sum-transformer #'Σ* (list #'Π ...) Σ-hash1 Σ-hash2)]
;;      ;;                         [Π (product-transformer #'Π #'Σ*/Π ordinal Π-hash1 Π-hash2)]
;;      ;;                         ...)
;;      ;;                      #'(let ([Σ*? (sum-pred Σ*)]
;;      ;;                              [Π? (product-pred Π)]
;;      ;;                              ...)
;;      ;;                          body ...)))])
;;      ;;           (go)))))

;;      ])

;;   ;; (syntax-case stx ()
;;   ;;   [(_ ([Σ (Π ...)]) body ...)
;;   ;;    (let ([Πs (syntax-e #'(Π ...))])
;;   ;;      (with-syntax ([Σ* (sum-introducer #'Σ 'add)])
;;   ;;        (with-syntax ([Σ-hash1 (make-hash1 #'Σ*)]
;;   ;;                      [Σ-hash2 (make-hash2 #'Σ*)]
;;   ;;                      [(Σ*/Π ...) (make-list (length Πs) #'Σ*)]
;;   ;;                      [(ordinal ...) (build-list (length Πs) values)]
;;   ;;                      [(Π-hash1 ...) (map make-hash1 Πs)]
;;   ;;                      [(Π-hash2 ...) (map make-hash2 Πs)]
;;   ;;                      [Σ*? (sum-introducer (format-id #'Σ* "~a?" #'Σ*) 'add)]
;;   ;;                      [(Π? ...) (map (λ (stx) (format-id stx "~a?" stx)) Πs)])
;;   ;;          #''(let-syntax
;;   ;;                ([Σ* (sum-transformer #'Σ* (list #'Π ...) Σ-hash1 Σ-hash2)]
;;   ;;                 [Π (product-transformer #'Π #'Σ*/Π ordinal Π-hash1 Π-hash2)]
;;   ;;                 ...
;;   ;;                 [go (λ (stx)
;;   ;;                       (let-syntax
;;   ;;                           ([Σ* (sum-transformer #'Σ* (list #'Π ...) Σ-hash1 Σ-hash2)]
;;   ;;                            [Π (product-transformer #'Π #'Σ*/Π ordinal Π-hash1 Π-hash2)]
;;   ;;                            ...)
;;   ;;                         #'(let ([Σ*? (sum-pred Σ*)]
;;   ;;                                 [Π? (product-pred Π)]
;;   ;;                                 ...)
;;   ;;                             body ...)))])
;;   ;;              (go)))))])

;;   ;; (syntax-case stx ()
;;   ;;   [(_ ([Σ (Π ...)] ...) body ...)
;;   ;;    #'(let () (begin (data Σ (Π ...)) ...) body ...)])

;;   )

;; (define-syntax (with-data stx)
;;   (syntax-case stx ()
;;     [(_ (Π ...) body ...)
;;      (with-syntax ([Σ (datum->syntax stx (gensym 'Σ))])
;;        #'(let-data ([Σ (Π ...)]) body ...))]))

(define (data-less-than? Π1 Π2)
  (and (not (equal? Π1 Π2))
       (equal? (product-sum Π1)
               (product-sum Π2))
       (< (product-ordinal Π1)
          (product-ordinal Π2))))

(define (data->list a)
  (cond [(sum? a) (map eval-syntax (sum-product-ids a))]
        [(product-instance? a) (cons (product-instance-product a)
                                     (product-instance-arguments a))]
        [else (list a)]))

;;; ----------------------------------------------------------------------------

(module+ test
  (require racket/format
           racket/list
           rackunit)

  (define OK (string->unreadable-symbol "OK"))

  (define-simple-check (check-OK val)
    (eq? val OK))

  ;; Sums

  (data Void ())

  (test-case "Void is a sum"
    (check-true (sum? (sum Void))))

  (test-case "The Void sum has no products"
    (check-pred null? (sum-product-ids (sum Void))))

  ;; Products

  (data Unit (Unit))

  (test-case "Unit is a sum"
    (check-true (sum? (sum Unit))))

  (test-case "Unit is also a product"
    (check-true (product? Unit)))

  (test-case "The Unit product belongs to the Unit sum"
    (check-pred (sum Unit?) Unit))

  (data Bool (False True))

  (test-case "True is an element of Bool"
    (check-pred (sum Bool?) True))

  (test-case "False is an element of Bool"
    (check-pred (sum Bool?) False))

  (test-case "False is the first element of Bool"
    (check equal? (product-sum False) (sum Bool))
    (check = (product-ordinal False) 0))

  (test-case "True is the second element of Bool"
    (check equal? (product-sum True) (sum Bool))
    (check = (product-ordinal True) 1))

  (data Maybe (Just Nothing))

  (test-case "Just is a product"
    (check-pred product? Just))

  (test-case "Nothing is a product"
    (check-pred product? Nothing))

  (test-case "Just is a member of Maybe"
    (check equal? (product-sum Just) (sum Maybe))
    (check-pred (sum Maybe?) Just))

  (test-case "Nothing is a member of Maybe"
    (check equal? (product-sum Nothing) (sum Maybe))
    (check-pred (sum Maybe?) Nothing))

  (test-case "Unit is not a member of Maybe"
    (check-false (equal? (product-sum Unit) (sum Maybe)))
    (check-false ((sum Maybe?) Unit)))

  ;; Product Instances

  (test-case "Just is a product"
    (check-pred product? Just))

  (test-case "Just is not a product instance"
    (check-false (product-instance? Just)))

  (test-case "(Just) is a product instance"
    (check-pred product-instance? (Just)))

  (test-case "(Just 123) is an product instance"
    (check-pred product-instance? (Just 123)))

  ;; Idempotence

  (test-case "The Maybe sum evaluates to itself"
    (check equal? (sum Maybe) (sum Maybe))
    (check equal? (~v (sum Maybe)) "(data Maybe (Just Nothing))"))

  (test-case "The Just product evaluates to itself"
    (check equal? Just Just)
    (check equal? (~v Just) "Just"))

  (test-case "The (Just) product instance evaluates to itself"
    (check equal? (Just) (Just))
    (check equal? (~v (Just)) "(Just)"))

  (test-case "(Just 1 2 3) evaluates to itself"
    (check equal? (Just 1 2 3) (Just 1 2 3))
    (check equal? (~v (Just 1 2 3)) "(Just 1 2 3)"))

  ;; Equality

  (test-case "Like-named constructors are equivalent (in a given scope)"
    (check-true (equal? Just Just))
    (check-true (equal? Nothing Nothing))
    (check-false (equal? Just Nothing))
    (check-false (equal? Nothing Just))
    (let ([J Just])
      (check-true (equal? J Just))
      (check-false (equal? J Nothing))
      (check-false (equal? Nothing J))
      ;; (with-data (Just Nothing)
      ;;   (check-false (equal? J Just)))
      (check-true (equal? J Just))
      (check-false (equal? J Nothing))
      (check-false (equal? Nothing J))))

  (test-case "Structurally equivalent product instances are equal"
    (check equal? (Just) (Just))
    (check equal? (Just 123) (Just 123))
    (check equal? (Just 1 2 3) (Just 1 2 3)))

  ;; Ordinality

  (test-case "False comes before True"
    (check-true (data-less-than? False True)))

  (test-case "True comes after False"
    (check-false (data-less-than? True False)))

  ;; data->list

  (data A-Z (A B C D E F G H I J K L M N O P Q R S T U V W X Y Z))

  (define-namespace-anchor ns-anchor)
  (define ns (namespace-anchor->namespace ns-anchor))

  (test-case "data->list"
    (parameterize ([current-namespace ns])
      (check equal?
             (sort (shuffle (data->list (sum A-Z))) data-less-than?)
             (list A B C D E F G H I J K L M N O P Q R S T U V W X Y Z))
      (check equal? (data->list (A 1 2 3)) (list A 1 2 3))
      (check equal? (data->list '(1 2 3)) '((1 2 3)))
      (check equal? (data->list 123) '(123))))

  ;; (test-case "data"
  ;;   (data ABC (A B C)
  ;;         ZYX (Z Y X))

  ;;   (check-pred (sum ABC?) (sum ABC))
  ;;   (check-false ((sum ABC?) (sum ZYX)))

  ;;   (check-pred (sum ABC?) A)
  ;;   (check-false ((sum ABC?) Z))

  ;;   (check-pred A? A)
  ;;   (check-false (A? B))

  ;;   (check equal? A A)
  ;;   (check-false (equal? A B))

  ;;   (check equal? (A) (A))
  ;;   (check-false (equal? (A) (B)))

  ;;   (check equal? (A 1 2 3) (A 1 2 3))
  ;;   (check-false (equal? (A 1 2 3) (A 4 5 6)))
  ;;   (check-false (equal? (A 1 2 3) (B 1 2 3)))

  ;; )

  )
