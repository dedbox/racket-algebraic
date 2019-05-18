#lang racket/base

;;; Context-preserving Syntax Transformations

;;; A /context-preserving syntax transformer/ is a syntax transformer that
;;; derives the lexical context and source locations of its result from its
;;; argument.
;;;
;;; The class of syntax lists together with all context-preserving syntax
;;; transformers between them (as morphisms), where the composition of
;;; morphisms is the usual function composition, forms a category.

(require algebraic/prelude
         racket/contract/base
         (for-syntax algebraic/prelude
                     racket/base))

(provide
 do
 (contract-out
  [syntax->values (-> syntax? any)]
  [syntax-list? predicate/c]
  [bind (-> (-> any/c ... syntax-list?) syntax-list? syntax-list?)]
  [return (-> any/c ... syntax-list?)]
  [join (-> syntax-list? any)]
  [pure (-> any/c ... syntax-list?)]
  [ap (-> syntax-list? syntax-list? ... syntax-list?)]
  [fmap (-> procedure? syntax-list? ... syntax-list?)]
  [mempty syntax-list?]
  [mappend (-> syntax-list? ... syntax-list?)]))

(define (syntax->values xs)
  ($ id (syntax-e xs)))

(define syntax-list? (.. list? syntax-e))

(define (leftmost-context x)
  (cond [(syntax? x) x]
        [(pair? x) (or (leftmost-context (car x))
                       (leftmost-context (cdr x)))]
        [else #f]))

(define (simplify xs)
  (if (singleton? xs) (car xs) xs))

(define singleton? (&& pair? (.. null? cdr)))

(module+ test
  (require rackunit)

  (define-simple-check (check-syntax stx want)
    (equal? (syntax->datum stx) want)))

;;; ----------------------------------------------------------------------------
;;; Monad

(define (bind f xs)
  ($ f (syntax-e xs)))

(define (return . vs)
  (let ([ctx (leftmost-context vs)])
    (datum->syntax ctx vs ctx)))

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

(define pure return)

(define (ap f . xss)
  ($ fmap (join f) xss))

(module+ test
  (check-syntax (ap (pure ::) #'(1) #'(2)) '((1 . 2)))
  (check-syntax (ap (pure ::) #'(1 2) #'(3 4)) '((1 . 3) (2 . 4)))
  (check-syntax (ap (pure (.. add1 syntax-e)) #'(1)) '(2))
  (check-syntax (ap (pure (.. add1 syntax-e)) #'(1 2)) '(2 3))
  (check-syntax (ap (pure (.. add1 syntax-e)) #'(1 2 3)) '(2 3 4)))

;;; ----------------------------------------------------------------------------
;;; Functor

;;; maps f along the elements of the xss, taking an argument from each
(define (fmap f . xss)
  (define ctx (leftmost-context xss))
  (datum->syntax ctx (bind (λ xss ($ map f (map syntax-e xss))) ($ return xss))
                 ctx))

(module+ test
  (check-syntax (fmap :: #'(1) #'(2)) '((1 . 2)))
  (check-syntax (fmap :: #'(1 2) #'(3 4)) '((1 . 3) (2 . 4)))
  (check-syntax (fmap (.. add1 syntax-e) #'(1)) '(2))
  (check-syntax (fmap (.. add1 syntax-e) #'(1 2)) '(2 3))
  (check-syntax (fmap (.. add1 syntax-e) #'(1 2 3)) '(2 3 4)))

;;; ----------------------------------------------------------------------------
;;; Monoid

(define mempty (return))

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
                    (w . vs) <- #'(5 6 7)
                    ($ pure (++ xs (list y z) (list* w vs))))
                '(1 2 3 4 5 6 7)))
