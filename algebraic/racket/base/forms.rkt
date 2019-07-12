#lang racket/base

(require algebraic/function
         algebraic/prelude
         (for-syntax algebraic/macro
                     (except-in algebraic/prelude id)
                     algebraic/private
                     racket/base
                     syntax/parse
                     syntax/strip-context))

(provide (rename-out [algebraic-λ              λ]
                     [algebraic-lambda         lambda]
                     [algebraic-case-λ         case-λ]
                     [algebraic-case-lambda    case-lambda]
                     [algebraic-let            let]
                     [algebraic-let*           let*]
                     [algebraic-letrec         letrec]
                     [algebraic-let-values     let-values]
                     [algebraic-let*-values    let*-values]
                     [algebraic-letrec-values  letrec-values]
                     [algebraic-case           case]
                     [algebraic-case-values    case-values]
                     [algebraic-define         define]
                     [algebraic-define-values  define-values]))

(define-syntax algebraic-λ (μ* (args body ...+) (φ* args body ...)))
(define-syntax algebraic-lambda (μ* (args body ...+) (phi* args body ...)))

(define-syntax algebraic-case-λ
  (μ* ([formals body ...+] ...) (function* [formals body ...] ...)))

(define-syntax algebraic-case-lambda
  (μ* ([formals body ...+] ...) (function* [formals body ...] ...)))

(define-syntax algebraic-let
  (macro*
    [(proc-id:id ([patt expr] ...) body ...+)
     (letrec ([proc-id (φ* (patt ...) body ...)]) (proc-id expr ...))]
    [(([patt expr] ...) body ...+)
     #:with ((y ...) ...) (map pattern-variables (syntax-e #'(patt ...)))
     (let-values ([(y ...) ((φ patt (id y ...)) expr)] ...)
       body ...)]))

(define-syntax algebraic-let*
  (μ* (([patt expr] ...) body ...+)
    #:with ((y ...) ...) (map pattern-variables (syntax-e #'(patt ...)))
    (let*-values ([(y ...) ((φ patt (id y ...)) expr)] ...)
      body ...)))

(define-syntax algebraic-letrec
  (μ* (([patt expr] ...) body ...+)
    #:with ((y ...) ...) (map pattern-variables (syntax-e #'(patt ...)))
    (letrec-values ([(y ...) ((φ patt (id y ...)) expr)] ...)
      body ...)))

(define-syntax algebraic-let-values
  (μ* (([(patt ...) expr] ...) body ...+)
    #:with ((y ...) ...) (map pattern-variables (syntax-e #'((patt ...) ...)))
    (let-values ([(y ...) (values-> (φ* (patt ...) (id y ...)) expr)] ...)
      body ...)))

(define-syntax algebraic-let*-values
  (μ* (([(patt ...) expr] ...) body ...+)
    #:with ((y ...) ...) (map pattern-variables (syntax-e #'((patt ...) ...)))
    (let*-values ([(y ...) (values-> (φ* (patt ...) (id y ...)) expr)] ...)
      body ...)))

(define-syntax algebraic-letrec-values
  (μ* (([(patt ...) expr] ...) body ...+)
    #:with ((y ...) ...) (map pattern-variables (syntax-e #'((patt ...) ...)))
    (letrec-values ([(y ...) (values-> (φ* (patt ...) (id y ...)) expr)] ...)
      body ...)))

(define-syntax algebraic-case
  (macro*
    #:literals (else)
    [(expr [patt body ...+] ... [else then-body ...+])
     ((function [patt body ...] ... [_ then-body ...]) expr)]
    [(expr [patt body ...+] ...)
     ((function [patt body ...] ... [_ (void)]) expr)]))

(define-syntax algebraic-case-values
  (macro*
    #:literals (else)
    [(expr [(patt ...) body ...+] ... [else then-body ...+])
     (values-> (function* [(patt ...) body ...] ... [_ then-body ...]) expr)]
    [(expr [(patt ...) body ...+] ...)
     (values-> (function* [(patt ...) body ...] ...) expr)]))

(define-syntax algebraic-define
  (macro*
    [((head:id patt ...) body ...+)
     #:if (variable? #'head)
     (define head (φ* (patt ...) body ...) )]
    [((head:id patt ... . patt0) body ...+)
     #:if (variable? #'head)
     (define head (φ* (patt ... . patt0) body ...))]
    [(patt body ...+)
     #:with (y ...) (pattern-variables #'patt)
     (define-values (y ...) ((φ patt (id y ...)) (let () body ...)))]))

(define-syntax algebraic-define-values
  (μ* ((patt ...) expr)
    #:with (y ...) ($ ++ (map pattern-variables (syntax-e #'(patt ...))))
    (define-values (y ...) (values-> (φ* (patt ...) (id y ...)) expr))))

;;; ----------------------------------------------------------------------------

(module+ test
  (require algebraic/data
           rackunit
           (for-meta 2 racket/base))

  (data XYZ (X Y Z))

  (test-case "algebraic-λ"
    (check equal? ((algebraic-λ ((X . xs) (Y . ys)) (list xs ys))
                   (X 1 2 3) (Y 4 5 6))
           '((1 2 3) (4 5 6))))

  (test-case "algebraic-lambda"
    (check equal?
           ((algebraic-case-lambda [((X . xs) (Y . ys)) (list xs ys)])
            (X 1 2) (Y 3 4))
           '((1 2) (3 4))))

  (test-case "algebraic-let"
    (check equal? (algebraic-let ([(X a b c) (X 1 2 3)]
                                  [(Y d e) (Y 4 5)])
                    (list a b c d e))
           '(1 2 3 4 5))
    (check = (algebraic-let loop ([(X x) (X 0)])
               (if (< x 10) (loop (X (add1 x))) x))
           10))

  (test-case "algebraic-let* "
    (check equal? (algebraic-let* ([(X a b) (X 1 2)]
                                   [(Y c d) (Y a 3)])
                    (list a b c d))
           '(1 2 1 3)))

  (test-case "algebraic-letrec"
    (algebraic-letrec
        ([(Y is-even?) (Y (φ (X x) ((|| zero? (.. is-odd? X sub1)) x)))]
         [(Y is-odd?) (Y (φ (X x) ((&& (.. not zero?) (.. is-even? X sub1)) x)))])
      (check-true (is-odd? (X 11)))))

  (test-case "algebraic-let-values"
    (check equal?
           (algebraic-let-values ([((X a b) (Y c d)) (id (X 1 2) (Y 3 4))])
             (list a b c d))
           '(1 2 3 4)))

  (test-case "algebraic-let*-values"
    (check equal? (algebraic-let*-values ([((X a b)) (X 1 2)]
                                          [((Y c d)) (Y a 3)])
                    (list a b c d))
           '(1 2 1 3)))

  (test-case "algebraic-letrec-values"
    (check equal? (algebraic-letrec-values ([((X a b)) (X 1 (λ () d))]
                                            [((Y c d)) (Y (λ () a) 2)])
                    (list a (b) (c) d))
           '(1 2 1 2))
    (algebraic-letrec-values
        ([((Y is-even?)) (Y (φ (X x) ((|| zero? (.. is-odd? X sub1)) x)))]
         [((Y is-odd?)) (Y (φ (X x) ((&& (.. not zero?) (.. is-even? X sub1)) x)))])
      (check-true (is-odd? (X 11)))))

  (test-case "algebraic-case"
    (check equal? (algebraic-case (Y 1 2)
                    [(X a b) (list a b)]
                    [(Y a b) (vector a b)])
           #(1 2)))

  (test-case "algebraic-case-values"
    (check equal? (algebraic-case-values (id (X 1 2) (Y 3 4))
                    [((Y a b) (Z c d)) (vector a b c d)]
                    [((X a b) (Y c d)) (list a b c d)]
                    [else 1])
           '(1 2 3 4))
    (check = (algebraic-case-values (id (X 1 2) (Z 3 4))
               [((Y a b) (Z c d)) (vector a b c d)]
               [((X a b) (Y c d)) (list a b c d)]
               [else 0])
           0))

  (test-case "algebraic-define"
    (algebraic-define ((X . xs) (Y . ys)) (list (X 1 2) (Y 3 4)))
    (check equal? (list xs ys) '((1 2) (3 4)))
    (algebraic-define (f (X a b) (Y c d)) (list a b c d))
    (check equal? (f (X 1 2) (Y 3 4)) '(1 2 3 4)))

  (test-case "algebraic-define-values"
    (algebraic-define-values ((X a b) (Y c d)) (id (X 1 2) (Y 3 4)))
    (check equal? (list a b c d) '(1 2 3 4))))
