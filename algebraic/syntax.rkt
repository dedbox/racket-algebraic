#lang racket/base

(require algebraic/prelude
         racket/contract/base
         (for-syntax algebraic/prelude
                     racket/base))

(provide
 do
 (contract-out
  [->syntax (-> (or/c syntax? #f) any/c syntax?)]
  [syntax-list? predicate/c]
  [bind (-> (-> any/c ... syntax-list?) syntax-list? syntax-list?)]
  [return (-> any/c ... syntax-list?)]
  [join (-> syntax-list? any)]
  [pure (-> any/c ... syntax-list?)]
  [ap (-> syntax-list? syntax-list? ... syntax-list?)]
  [fmap (-> procedure? syntax-list? ... syntax-list?)]
  [mempty syntax-list?]
  [mappend (-> syntax-list? ... syntax-list?)]))

(define (->syntax ctx stx)
  (datum->syntax ctx stx ctx))

(define syntax-list? (.. list? syntax-e))

(module+ test
  (require rackunit)

  (define-simple-check (check-syntax stx want)
    (equal? (syntax->datum stx) want)))

;;; ----------------------------------------------------------------------------
;;; Helpers

(define (leftmost-context x)
  (cond [(and (syntax? x)
              (not (null? (hash-ref (syntax-debug-info x) 'context null)))) x]
        [(pair? x) (or (leftmost-context (car x))
                       (leftmost-context (cdr x)))]
        [else #f]))

(define (simplify xs)
  (if (singleton? xs) (car xs) xs))

(define singleton? (&& pair? (.. null? cdr)))

;;; ----------------------------------------------------------------------------
;;; Monad

(define (bind f xs)
  ($ f (syntax-e xs)))

(define (return . vs)
  (->syntax (leftmost-context vs) vs))

;;; syntax-list -> values
(define (join xs)
  ($ id (map syntax-e (syntax-e xs))))

(module+ test
  (check-syntax (bind return #'(1)) '(1))
  (check-syntax (bind return #'(1 2)) '(1 2))
  (check eq? (join (return +)) +)
  (check equal? (values-> list (join (return 1 2 3))) '(1 2 3)))

;;; ----------------------------------------------------------------------------
;;; Applicative Functor

(define (pure . vs)
  (->syntax #f vs))

(define (ap f . xss)
  (->syntax (leftmost-context (:: f xss))
            (bind (λ xss ($ map (join f) (map syntax-e xss))) ($ pure xss))))

(module+ test
  (check-syntax (ap (pure ::) #'(1) #'(2)) '((1 . 2)))
  (check-syntax (ap (pure ::) #'(1 2) #'(3 4)) '((1 . 3) (2 . 4)))
  (check-syntax (ap (pure (.. add1 syntax-e)) #'(1)) '(2))
  (check-syntax (ap (pure (.. add1 syntax-e)) #'(1 2)) '(2 3))
  (check-syntax (ap (pure (.. add1 syntax-e)) #'(1 2 3)) '(2 3 4)))

;;; ----------------------------------------------------------------------------
;;; Functor

(define (fmap f . xss)
  (->syntax (leftmost-context xss)
            (bind (λ xss ($ map f (map syntax-e xss))) ($ return xss))))

(module+ test
  (check-syntax (fmap :: #'(1) #'(2)) '((1 . 2)))
  (check-syntax (fmap :: #'(1 2) #'(3 4)) '((1 . 3) (2 . 4)))
  (check-syntax (fmap (.. add1 syntax-e) #'(1)) '(2))
  (check-syntax (fmap (.. add1 syntax-e) #'(1 2)) '(2 3))
  (check-syntax (fmap (.. add1 syntax-e) #'(1 2 3)) '(2 3 4)))

;;; ----------------------------------------------------------------------------
;;; Monoid

(define mempty (pure))

(define (mappend . xss)
  ($ return ($ ++ (map syntax-e xss))))

(module+ test
  (check-syntax (mappend mempty mempty) null)
  (check-syntax (mappend mempty #'(1 2)) '(1 2))
  (check-syntax (mappend #'(1 2) mempty) '(1 2))
  (check-syntax (mappend #'(1 2) #'(3 4)) '(1 2 3 4))
  (check-syntax (mappend (mappend #'(1 2) #'(3 4)) #'(5 6)) '(1 2 3 4 5 6))
  (check-syntax (mappend #'(1 2) (mappend #'(3 4) #'(5 6))) '(1 2 3 4 5 6)))

;;; ----------------------------------------------------------------------------
;;; ``Do'' Notation

(begin-for-syntax
  (define arrow? (&& identifier? (.. (<< eq? '<-) syntax->datum)))

  (define (make-block stx)
    (syntax-case stx ()

      [(as arrow xs . block)
       (and (identifier? #'as) (arrow? #'arrow))
       #`(bind (λ as #,(make-block #'block)) xs)]

      [((a ...) arrow xs . block)
       (and (andmap identifier? (syntax-e #'(a ...))) (arrow? #'arrow))
       #`(bind (λ (a ...) #,(make-block #'block)) xs)]

      [((a ... . b) arrow xs . block)
       (and (andmap identifier? (syntax-e #'(a ... b))) (arrow? #'arrow))
       #`(bind (λ (a ... . b) #,(make-block #'block)) xs)]

      [(expr) #'expr]

      [(expr . block) #`(begin expr #,(make-block #'block))])))

(define-syntax (do stx)
  (syntax-case stx () [(_ . block) #`#,(make-block #'block)]))

(module+ test
  (check-syntax (do xs <- #'(1 2)
                    (y z) <- #'(3 4)
                    (w v . us) <- #'(5 6 7 8 9)
                    ($ return (++ xs (list y z) (list* w v us))))
                '(1 2 3 4 5 6 7 8 9)))
