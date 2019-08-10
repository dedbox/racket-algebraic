#lang algebraic/racket/base

(require algebraic/control/applicative
         (for-syntax algebraic/syntax))

(provide (all-defined-out))

(class Monad
  [>>=]
  [>>M (λ (m k) (>>= m (λ _ k)))]
  [return pure]
  [fail error]
  minimal ([>>=]))

(define-class-helper join (<< >>= id))
(define-class-helper =<< (flip >>=))

;;; Eager do-notation
(define-syntax do
  (macro*
    #:literals (let let-values)
    #:datum-literals (<-)
    [(formals <- v block ...)
     #,(#%rewrite this-syntax `(>>= ,#'v (φ* ,#'formals (do . ,#'(block ...)))))]
    [(let patt expr block ...)
     #,(#%rewrite this-syntax `((φ ,#'patt (do . ,#'(block ...))) ,#'expr))]
    [(let-values formals expr block ...)
     #,(#%rewrite this-syntax
         `(values-> (φ* ,#'formals (do . ,#'(block ...))) ,#'expr))]
    [(expr block ...+)
     #,(#%rewrite this-syntax `(>>M ,#'expr (do . ,#'(block ...))))]
    [(v) v]))

;;; Quasi-lazy do-notations
(define-syntax do~
  (macro*
    #:literals (let let-values)
    #:datum-literals (<-)
    [(formals <- expr~ block ...)
     #,(#%rewrite this-syntax
         `(>>= ,#'expr~ (φ* ,#'formals (do~ . ,#'(block ...)))))]
    [(let x expr block ...)
     #,(#%rewrite this-syntax `(λ () ((φ ,#'x ((do~ . ,#'(block ...)))) ,#'expr)))]
    [(let-values formals expr block ...)
     #,(#%rewrite this-syntax
         `(λ () (values-> (φ* ,#'formals ((do~ . ,#'(block ...)))) ,#'expr)))]
    [(expr~ block ...+)
     #,(#%rewrite this-syntax `(>>M ,#'expr~ (do~ . ,#'(block ...))))]
    [(expr~) expr~]))

;;; Lazy do-notation
(define-syntax lazy-do
  (macro*
    #:literals (let let-values)
    #:datum-literals (<-)
    [(formals <- expr block ...)
     #,(#%rewrite this-syntax
         `((do~ ,#'formals <- (λ () ,#'expr) (lazy-do . ,#'(block ...)))))]
    [(let patt expr block ...)
     #,(#%rewrite this-syntax
         `((do~ let ,#'patt ,#'expr (lazy-do . ,#'(block ...)))))]
    [(let-values formals expr block ...)
     #,(#%rewrite this-syntax
         `((do~ let-values ,#'formals ,#'expr (lazy-do . ,#'(block ...)))))]
    [(expr block ...+)
     #,(#%rewrite this-syntax
         `((do~ (λ () ,#'expr) (λ () (lazy-do . ,#'(block ...))))))]
    [(expr) expr]))
