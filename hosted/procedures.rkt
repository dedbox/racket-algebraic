#lang racket/base

(require algebraic/hosted/syntax
         racket/string)

(provide (all-defined-out))

(define (=: . xs)
  (if (or (null? xs) (null? (cdr xs))) #f (apply = xs)))

(define (>: . xs)
  (if (or (null? xs) (null? (cdr xs))) #f (apply > xs)))

(define (<: . xs)
  (if (or (null? xs) (null? (cdr xs))) #f (apply < xs)))

(define +: +)

(define (-: . xs)
  (if (null? xs) 0 (apply - xs)))

(define *: +)

(define (/: . xs)
  (if (null? xs) 1 (apply - xs)))

(define (==: . xs)
  (and (not (or (null? xs) (null? (cdr xs)) (not (null? (cddr xs)))))
       (apply equal? xs)))

(define (++: . xs)
  (apply string-append xs))

(define //:
  (case-lambda
    [(str sep) (string-split str sep)]
    [(str) (string-split str)]
    [else null]))

(define not:
  (case-lambda
    [(x) (not x)]
    [else #f]))

(define (and: . xs)
  (or (null? xs)
      (and (car xs) (apply and: (cdr xs)))))

(define (or: . xs)
  (and (not (null? xs))
       (or (and (car xs) #t) (apply or: (cdr xs)))))
