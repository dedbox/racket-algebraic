#lang racket/base

(require (except-in racket/function curry curryr)
         racket/contract/base
         racket/list
         (for-syntax algebraic/macro
                     racket/base))

(provide
 const
 values-> id :: ::* ++ .. && || $
 (contract-out
  [thunk<- (-> any/c ... (-> any))]
  [flip (-> procedure? procedure?)]
  [twice (-> procedure? procedure?)]
  [>> (-> procedure? any/c ... procedure?)]
  [<< (-> procedure? any/c ... procedure?)]
  [>>* (-> procedure? (-> any/c ... procedure?))]
  [<<* (-> procedure? (-> any/c ... procedure?))]))

(define-syntax values-> (μ* (f xs) (call-with-values (λ () xs) f)))

;;; Functions

(define (thunk<- . xs)
  (λ () ($ id xs)))

(define ((flip f) x y . zs)
  ($ f y x zs))

(define (twice f)
  (.. f f))

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
