#lang algebraic/racket/base

(require algebraic/class/applicative
         (for-syntax algebraic/syntax))

(provide (all-defined-out))

;;; ----------------------------------------------------------------------------
;; The Monad class defines the basic operations over a /monad/, a concept from
;; a branch of mathematics known as /category theory/. From the perspective of
;; a Haskell programmer, however, it is best to think of a monad as an
;; /abstract datatype/ of actions. Haskell's do expressions provide a
;; convenient syntax for writing monadic expressions.
;;
;; Instances of 'Monad' should satisfy the following laws:
;;
;;  return a >>= k  =  k a
;;  m >>= return  =  m
;;  m >>= (\x -> k x >>= h)  =  (m >>= k) >>= h
;;
;; Furthermore, the Monad and Applicative operations should relate as follows:
;;
;;  pure = return
;;  (<*>) = ap
;;
;; The above laws imply:
;;
;;  fmap f xs  =  xs >>= return . f
;;  (>>) = (*>)
;;
;; and that pure and (<*>) satisfy the applicative functor laws.
;;
;; The instances of 'Monad' for lists, 'Data.Maybe.Maybe' and 'System.IO.IO'
;; defined in the "Prelude" satisfy these laws.
(class Monad

  ;; Sequentially compose two actions, passing any value produced by the first
  ;; as an argument to the second.
  ;;
  ;; (>>=) :: forall a b. m a -> (a -> m b) -> m b
  [>>=]

  ;; Sequentially compose two actions, discarding any value produced
  ;; by the first, like sequencing operators (such as the semicolon)
  ;; in imperative languages.
  ;;
  ;; (>>) :: forall a b. m a -> m b -> m b
  [>>M (λ (m k) (>>= m (λ _ k)))]

  ;; Inject a value into the monadic type.
  ;;
  ;; return :: a -> m a
  [return pure]

  ;; Fail with a message. This operation is not part of the mathematical
  ;; definition of a monad, but is invoked on pattern-match failure in a do
  ;; expression.
  ;;
  ;; As part of the MonadFail proposal (MFP), this function is moved
  ;; to its own class 'MonadFail' (see "Control.Monad.Fail" for more
  ;; details). The definition here will be removed in a future
  ;; release.
  ;;
  ;; fail :: String -> m a
  [fail error]

  minimal ([>>=]))

;;; ----------------------------------------------------------------------------
;;; Helpers

;; The join function is the conventional monad join operator. It is used to
;; remove one level of monadic structure, projecting its bound argument into
;; the outer level.
;;
;; join :: (Monad m) => m (m a) -> m a
(define-syntax join (class-helper (λ (xs) (>>= xs id))))

;; Same as >>=, but with the arguments interchanged.
;;
;; (=<<) :: Monad m => (a -> m b) -> m a -> m b
(define-syntax =<< (class-helper (λ (f x) (>>= x f))))

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
