#lang racket/base

(require (except-in racket/function curry curryr)
         racket/contract/base
         racket/list
         (for-syntax racket/base))

(provide
 const negate
 values-> id :: ::* ++ .. && || $
 (contract-out
  ;; Functions
  [thunk<- (-> any/c ... (-> any))]
  [flip (-> procedure? procedure?)]
  [twice (-> procedure? procedure?)]
  [$$ (-> procedure? any/c ... any)]
  [>> (-> procedure? any/c ... procedure?)]
  [<< (-> procedure? any/c ... procedure?)]
  [>>* (-> procedure? (-> any/c ... procedure?))]
  [<<* (-> procedure? (-> any/c ... procedure?))]
  ;; Lists
  [member-of (-> any/c ... (-> any/c (or/c list? #f)))]
  [free-member-of (-> identifier? ... (-> identifier? (or/c list? #f)))]))

(define-syntax (values-> stx)
  (syntax-case stx ()
    [(_ f xs) #'(call-with-values (λ () xs) f)]))

;;; Functions

(define (thunk<- . xs)
  (λ () ($ id xs)))

(define ((flip f) x y . zs)
  ($ f y x zs))

(define (twice f)
  (.. f f))

;;; apply head to tail
(define ($$ f . xs)
  ($ f xs))

;;; left curry
(define ((>> f . xs) . ys)
  ($ f (++ xs ys)))

;;; right curry
(define ((<< f . xs) . ys)
  ($ f (++ ys xs)))

;;; partial left-curry
(define (>>* f) (.. (>> $ >> f) list))

;;; partial right-curry
(define (<<* f) (.. (>> $ << f) list))

;;; Function Aliases

(define id values)
(define :: cons)
(define ::* list*)
(define ++ append)
(define .. compose)
(define && conjoin)
(define || disjoin)
(define $ apply)

;;; Lists

(define member-of (.. (<<* member) list))

(define (free-member-of . xs)
  (<< member xs free-identifier=?))
