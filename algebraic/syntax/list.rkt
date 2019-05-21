#lang racket/base

(require algebraic/prelude
         algebraic/syntax
         racket/contract/base)

(provide
 syntax-list?
 (contract-out

  ;; Syntax Pair Constructors and Selectors

  [syntax-pair? (-> syntax? boolean?)]
  [syntax-null? (-> syntax? boolean?)]
  [syntax-cons (-> syntax? syntax? syntax-pair?)]
  [syntax-car (-> syntax-pair? syntax?)]
  [syntax-cdr (-> syntax-pair? syntax?)]
  [syntax-null syntax-null?]
  [syntax-list (-> syntax? ... syntax-list?)]
  [syntax-list* (-> syntax? ... syntax? syntax?)]
  [build-syntax-list (-> exact-nonnegative-integer?
                         (-> exact-nonnegative-integer? syntax?)
                         syntax-list?)]

  ;;; Syntax List Operations

  [syntax-length (-> syntax-list? exact-nonnegative-integer?)]
  [syntax-list-ref (-> syntax-pair? exact-nonnegative-integer? syntax?)]
  [syntax-list-tail (-> syntax? exact-nonnegative-integer? syntax?)]
  [syntax-append (first-or/c (-> syntax-list? ... syntax?)
                             (-> syntax-list? ... syntax? syntax?))]
  [syntax-reverse (-> syntax-list? syntax-list?)]

  ;; Syntax List Iteration

  [syntax-map (-> procedure? syntax-list? ... syntax-list?)]
  [syntax-andmap (-> procedure? syntax-list? syntax-list? ... any)]
  [syntax-ormap (-> procedure? syntax-list? syntax-list? ... any)]
  [syntax-foldr (-> procedure? syntax? syntax-list? ... syntax?)]

  ;; Syntax List Filtering

  [syntax-filter (-> procedure? syntax-list? syntax-list?)]

  ;; Syntax Pair Accessor Shorthands

  [syntax-cadr (-> (syntax/c (cons/c any/c pair?)) syntax?)]
  [syntax-cddr (-> (syntax/c (cons/c any/c pair?)) syntax?)]))

;;; Syntax Pair Constructors and Selectors

(define syntax-pair? (.. pair? syntax-e))
(define syntax-null? (.. null? syntax-e))

(define (syntax-cons a b)
  (syntax-list* a b))

(define syntax-car (.. car syntax-e))
(define syntax-cdr (.. (>> $ id) syntax-e return cdr syntax-e))
(define syntax-null mempty)
;;; syntax-list? comes from the syntax module
(define syntax-list return)

(define (syntax-list* . xs)
  (datum->syntax (car xs) ($ list* xs) (car xs)))

(define (build-syntax-list n f)
  ($ return (build-list n f)))

;;; Syntax List Operations

(define syntax-length (.. length syntax-e))

(define (syntax-list-ref xs pos)
  (list-ref (syntax-e xs) pos))

(define (syntax-list-tail xs pos)
  (define ys (if ((|| syntax-list? syntax-pair?) xs)
                  (list-tail (syntax-e xs) pos)
                  (list-tail xs pos)))
  (cond [(list? ys) ($ syntax-list ys)]
        [(pair? ys) (datum->syntax (car ys) ys (car ys))]
        [else ys]))

(define syntax-append mappend)
(define syntax-reverse (.. (>> $ id) syntax-e return reverse syntax-e))

;;; Syntax List Iteration

(define syntax-map fmap)

(define (syntax-andmap f . xss)
  ($ (>> andmap f) (map syntax-e xss)))

(define (syntax-ormap f . xss)
  ($ (>> ormap f) (map syntax-e xss)))

(define (syntax-foldr f init . xss)
  ($ foldr f init (map syntax-e xss)))

;;; Syntax List Filtering

(define (syntax-filter pred xs)
  (bind (λ xs* ($ syntax-list (filter pred xs*))) xs))

;;; Syntax Pair Accessor Shorthands

(define syntax-cadr (.. syntax-car syntax-cdr))
(define syntax-cddr (twice syntax-cdr))

;;; ----------------------------------------------------------------------------

(module+ test
  (require rackunit)

  (define-simple-check (check-syntax stx want)
    (equal? (syntax->datum stx) want))

  ;;; Syntax Pair Constructors and Selectors

  (test-case "syntax-pair?"
    (check-false (syntax-pair? #'()))
    (check-false (syntax-pair? #'1))
    (check-true (syntax-pair? #'(1)))
    (check-true (syntax-pair? #'(()))))

  (test-case "syntax-null?"
    (check-true (syntax-null? #'()))
    (check-false (syntax-null? #'(())))
    (check-false (syntax-null? #'(1))))

  (test-case "syntax-cons"
    (check-syntax (syntax-cons #'1 #'()) '(1))
    (check-syntax (syntax-cons #'1 #'2) '(1 . 2))
    (check-syntax (syntax-cons #'1 #'(2)) '(1 2))
    (check-syntax (syntax-cons #'1 #'(2 3)) '(1 2 3)))

  (test-case "syntax-car"
    (check-syntax (syntax-car #'(1 2)) 1)
    (check-syntax (syntax-car #'(1 . 2)) 1)
    (check-pred syntax-null? (syntax-car #'(()))))

  (test-case "syntax-cdr"
    (check-syntax (syntax-cdr #'(1 . 2)) 2)
    (check-syntax (syntax-cdr #'(1 2)) '(2)))

  (test-case "syntax-null"
    (check-pred syntax-null? syntax-null)
    (check-syntax syntax-null null)
    (check-syntax (syntax-cons #'1 syntax-null) '(1)))

  (test-case "syntax-list?"
    (check-true (syntax-list? syntax-null))
    (check-true (syntax-list? #'()))
    (check-false (syntax-list? #'1))
    (check-false (syntax-list? #'(1 . 2))))

  (test-case "syntax-list"
    (check-syntax (syntax-list) null)
    (check-syntax (syntax-list #'1) '(1))
    (check-syntax (syntax-list #'1 #'2) '(1 2))
    (check-syntax (syntax-list #'1 #'2 #'3) '(1 2 3)))

  (test-case "syntax-list*"
    (check-syntax (syntax-list* #'()) null)
    (check-syntax (syntax-list* #'1 #'()) '(1))
    (check-syntax (syntax-list* #'1 #'(2)) '(1 2))
    (check-syntax (syntax-list* #'1 #'2 #'()) '(1 2))
    (check-syntax (syntax-list* #'1 #'2 #'(3)) '(1 2 3))
    (check-syntax (syntax-list* #'1 #'2 #'(3 4)) '(1 2 3 4)))

  (test-case "build-syntax-list"
    (check-syntax (build-syntax-list 0 error) null)
    (check-syntax (build-syntax-list 5 (>> datum->syntax #f)) '(0 1 2 3 4)))

  ;; Syntax List Operations

  (test-case "syntax-length"
    (for ([i 10])
      (check = (syntax-length (build-syntax-list i (>> datum->syntax #f))) i)))

  (test-case "syntax-list-ref"
    (for ([i 10])
      (check-syntax (syntax-list-ref #'(9 8 7 6 5 4 3 2 1 0) i)
                    (- 9 i)))
    (check-syntax (syntax-list-ref #'(1 2 . 3) 0) 1)
    (check-syntax (syntax-list-ref #'(1 2 . 3) 1) 2))

  (test-case "syntax-list-tail"
    (for ([i 10])
      (check-syntax (syntax-list-tail #'(9 8 7 6 5 4 3 2 1 0) i)
                    (reverse (build-list (- 10 i) values))))
    (check-syntax (syntax-list-tail (syntax-cons #'1 #'2) 1) 2)
    (check-syntax (syntax-list-tail #'not-a-pair 0) 'not-a-pair))

  (test-case "syntax-append"
    (check-syntax (syntax-append) null)
    (check-syntax (syntax-append #'()) null)
    (check-syntax (syntax-append #'() syntax-null) null)
    (check-syntax (syntax-append syntax-null #'()) null)
    (check-syntax (syntax-append syntax-null syntax-null) null)
    (check-syntax (syntax-append #'(1)) '(1))
    (check-syntax (syntax-append #'(1) #'()) '(1))
    (check-syntax (syntax-append #'(1) #'(2)) '(1 2))
    (check-syntax (syntax-append #'(1 2) #'()) '(1 2))
    (check-syntax (syntax-append #'(1 2) #'(3)) '(1 2 3))
    (check-syntax (syntax-append #'(1 2) #'(3) #'()) '(1 2 3))
    (check-syntax (syntax-append #'(1 2) #'(3) #'(4)) '(1 2 3 4))
    (check-syntax (syntax-append #'(1 2) #'(3 4)) '(1 2 3 4))
    (check-syntax (syntax-append #'(1 2) #'(3 4) #'()) '(1 2 3 4))
    (check-syntax (syntax-append #'(1 2) #'(3 4) #'(5)) '(1 2 3 4 5))
    (check-syntax (syntax-append #'(1 2) #'(3 4) #'(5 6)) '(1 2 3 4 5 6)))

  (test-case "syntax-reverse"
    (for ([i 10])
      (check-syntax (syntax-reverse (build-syntax-list i (>> datum->syntax #f)))
                    (reverse (build-list i values)))))

  ;; Syntax List Iteration

  (test-case "syntax-map"
    (check-syntax (syntax-map (>> syntax-cons #'!) #'(1 2 3)) '((! . 1) (! . 2) (! . 3))))

  (test-case "syntax-andmap"
    (check-true (syntax-andmap syntax-null? #'()))
    (check-true (syntax-andmap syntax-null? #'(())))
    (check-true (syntax-andmap syntax-null? #'(() ())))
    (check-true (syntax-andmap syntax-null? #'(() () ())))
    (check-false (syntax-andmap syntax-null? #'(() () 1 ())))
    (check-false (syntax-andmap syntax-null? #'(1 2 () 3))))

  (test-case "syntax-ormap"
    (check-true (syntax-ormap syntax-null? #'(() () ())))
    (check-true (syntax-ormap syntax-null? #'(() ())))
    (check-true (syntax-ormap syntax-null? #'(())))
    (check-false (syntax-ormap syntax-null? #'()))
    (check-false (syntax-ormap syntax-null? #'(1)))
    (check-false (syntax-ormap syntax-null? #'(1 2)))
    (check-false (syntax-ormap syntax-null? #'(1 2 3)))
    (check-true (syntax-ormap syntax-null? #'(1 2 3 ())))
    (check-true (syntax-ormap syntax-null? #'(1 2 () 3)))
    (check-true (syntax-ormap syntax-null? #'(1 () 2 3)))
    (check-true (syntax-ormap syntax-null? #'(() 1 2 3))))

  (test-case "syntax-foldr"
    (check-syntax (syntax-foldr (>> syntax-list* #'!) syntax-null #'(1 2 3)) '(! 1 ! 2 ! 3)))

  ;; Syntax List Filtering

  (test-case "syntax-filter"
    (check-syntax (syntax-filter (.. number? syntax-e) #'(x 1 y 2 3 z w 4 v)) '(1 2 3 4)))

  ;; Syntax Pair Accessor Shorthands

  (test-case "syntax-cadr"
    (check-syntax (syntax-cadr #'(1 2)) 2)
    (check-syntax (syntax-cadr #'(1 2 . 3)) 2)
    (check-syntax (syntax-cadr #'(1 2 3)) 2))

  (test-case "syntax-cddr"
    (check-syntax (syntax-cddr #'(1 2)) null)
    (check-syntax (syntax-cddr #'(1 2 . 3)) 3)
    (check-syntax (syntax-cddr #'(1 2 3)) '(3))))
