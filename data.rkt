#lang racket/base

(require algebraic/product
         algebraic/sum
         syntax/parse/define
         (for-syntax algebraic/product
                     algebraic/sum
                     racket/base
                     racket/list
                     racket/syntax
                     syntax/parse)
         (for-meta 2 algebraic/product
                   algebraic/sum
                   racket/base))

(provide (all-defined-out)
         (all-from-out algebraic/product
                       algebraic/sum)
         (for-syntax (all-defined-out)
                     (all-from-out algebraic/product
                                   algebraic/sum
                                   racket/base))
         (for-meta 2 (all-from-out racket/base)))

(define (data-less-than? Π1 Π2)
  (and (not (equal? Π1 Π2))
       (equal? (product-sum Π1) (product-sum Π2))
       (< (product-ord Π1) (product-ord Π2))))

(define (data->list a)
  (cond [(sum? a) (map eval-syntax (sum-product-ids a))]
        [(instance? a) (cons (instance-product a) (instance-args a))]
        [else (list a)]))

(begin-for-syntax
  (define-syntax-class sum-name #:description "sum name" (pattern :id))
  (define-syntax-class product-name #:description "product name" (pattern :id))

  (define-syntax-class sum-def
    #:description "product names"
    #:attributes ([Π 1])
    (pattern (Π:product-name ...)))

  (define-syntax-class nonempty-sum-def
    #:description "product names"
    #:attributes ([Π 1])
    (pattern (Π:product-name ...+)))

  (define-syntax-class data-def
    #:description "data definition"
    #:attributes (Σ [Π 1])
    (pattern [Σ:sum-name def:sum-def] #:attr (Π 1) (attribute def.Π)))

  (define (sum-id? id)
    (and (identifier-binding id)
         (sum-transformer? (syntax-local-value id (λ _ #f)))))

  (define (product-id? id)
    (and (identifier-binding id)
         (product-transformer? (syntax-local-value id (λ _ #f)))))

  (define (product-id=? id1 id2)
    (and (product-id? id1)
         (product-id? id2)
         (equal? (syntax-local-value id1 (λ _ -1))
                 (syntax-local-value id2 (λ _ -2))))))

(define-simple-macro (data (~seq Σ:sum-name def:sum-def) ...+)
  #:do [(define Πs (flatten (attribute def.Π)))
        (define Σ*s (map (λ (Σ) (sum-introducer Σ 'add)) (attribute Σ)))]
  #:with (Σ* ...) Σ*s
  #:with (Σ*-hash1 ...) (map (λ (Σ*) (datum->syntax Σ* (equal-hash-code Σ*))) Σ*s)
  #:with (Σ*-hash2 ...) (map (λ (Σ*) (datum->syntax Σ* (equal-secondary-hash-code Σ*))) Σ*s)
  #:with (Σ*? ...) (map (λ (Σ) (sum-introducer (format-id Σ "~a?" Σ) 'add)) (attribute Σ))
  #:with (Σ*/Π ...) (flatten (map (λ (Σ* Πs) (make-list (length Πs) Σ*))
                                  (attribute Σ*)
                                  (attribute def.Π)))
  #:with (Π ...) Πs
  #:with (Π? ...) (map (λ (Π) (format-id Π "~a?" Π)) Πs)
  #:with (ord ...) (build-list (length Πs) (λ (n) (datum->syntax this-syntax n)))
  #:with (Π-hash1 ...) (map (λ (Π) (datum->syntax Π (equal-hash-code Π))) Πs)
  #:with (Π-hash2 ...) (map (λ (Π) (datum->syntax Π (equal-secondary-hash-code Π))) Πs)
  (begin
    (begin-for-syntax
      (begin (define-syntax Σ* (sum-transformer #'Σ* (list #'Π ...) Σ*-hash1 Σ*-hash2)) ...)
      (begin (define-syntax Π (product-transformer #'Π #'Σ*/Π ord Π-hash1 Π-hash2)) ...))
    (begin (define-syntax Σ* (sum-transformer #'Σ* (list #'Π ...) Σ*-hash1 Σ*-hash2)) ...)
    (begin (define-syntax Π (product-transformer #'Π #'Σ*/Π ord Π-hash1 Π-hash2)) ...)
    (begin (define Σ*? (sum-pred Σ*)) ...)
    (begin (define Π? (product-pred Π)) ...)))

(define ((sum-pred Σ) x)
  (cond [(sum? x) (equal? x Σ)]
        [(product? x) (equal? (product-sum x) Σ)]
        [(instance? x) (equal? (product-sum (instance-product x)) Σ)]
        [else #f]))

(define ((product-pred Π) x)
  (cond [(product? x) (equal? x Π)]
        [(instance? x) (equal? (instance-product x) Π)]
        [else #f]))

(define-simple-macro (let-data (~describe "let-data bindings" (def:data-def ...+))
                       (~describe "body" body:expr) ...+)
  #:do [(define Πs (flatten (attribute def.Π)))
        (define Σ*s (map (λ (Σ) (sum-introducer Σ 'add)) (attribute def.Σ)))]
  #:with (Σ* ...) (map syntax-local-introduce Σ*s)
  #:with (Σ*-hash1 ...) (map (λ (Σ*) (datum->syntax Σ* (equal-hash-code Σ*))) Σ*s)
  #:with (Σ*-hash2 ...) (map (λ (Σ*) (datum->syntax Σ* (equal-secondary-hash-code Σ*))) Σ*s)
  #:with (Σ*? ...) (map (λ (Σ) (sum-introducer (format-id Σ "~a?" Σ) 'add)) (attribute def.Σ))
  #:with (Σ*/Π ...) (flatten (map (λ (Σ* Πs) (make-list (length Πs) Σ*))
                                  (attribute Σ*)
                                  (attribute def.Π)))
  #:with (Π ...) Πs
  #:with (Π? ...) (map (λ (Π) (format-id Π "~a?" Π)) Πs)
  #:with (ord ...) (build-list (length Πs) (λ (n) (datum->syntax this-syntax n)))
  #:with (Π-hash1 ...) (map (λ (Π) (datum->syntax Π (equal-hash-code Π))) Πs)
  #:with (Π-hash2 ...) (map (λ (Π) (datum->syntax Π (equal-secondary-hash-code Π))) Πs)
  (let-syntax ([Σ* (sum-transformer #'Σ* (list #'Π ...) Σ*-hash1 Σ*-hash2)] ...)
    (let-syntax ([Π (product-transformer #'Π #'Σ*/Π ord Π-hash1 Π-hash2)] ...)
      (let ([Σ*? (sum-pred Σ*)] ...)
        (let ([Π? (product-pred Π)] ...)
          body ...)))))

(define-simple-macro (with-data def:nonempty-sum-def (~describe "body" body:expr) ...+)
  #:with Σ (datum->syntax this-syntax (gensym 'Sum))
  (let-data ([Σ (def.Π ...)]) body ...))

;;; ============================================================================

(module+ test
  (require racket/format
           racket/list
           rackunit
           syntax/macro-testing)

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
    (check = (product-ord False) 0))

  (test-case "True is the second element of Bool"
    (check equal? (product-sum True) (sum Bool))
    (check = (product-ord True) 1))

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

  ;; Instances

  (test-case "Just is a product"
    (check-pred product? Just))

  (test-case "Just is not an instance"
    (check-false (instance? Just)))

  (test-case "(Just) is an instance"
    (check-pred instance? (Just)))

  (test-case "(Just 123) is an instance"
    (check-pred instance? (Just 123)))

  ;; Idempotence

  (test-case "The Maybe sum evaluates to itself"
    (check equal? (sum Maybe) (sum Maybe))
    (check equal? (~v (sum Maybe)) "(data Maybe (Just Nothing))"))

  (test-case "The Just product evaluates to itself"
    (check equal? Just Just)
    (check equal? (~v Just) "Just"))

  (test-case "The (Just) instance evaluates to itself"
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
      ;; (check-true (equal? J Just))
      ;; (check-false (equal? J Nothing))
      ;; (check-false (equal? Nothing J))
      ;; (with-data (Just Nothing)
      ;;   (check-false (equal? J Just)))
      (check-true (equal? J Just))
      (check-false (equal? J Nothing))
      (check-false (equal? Nothing J))))

  (test-case "Structurally equivalent instances are equivalent"
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
      (check equal? (data->list 123) '(123)))))
