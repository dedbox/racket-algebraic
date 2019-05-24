#lang algebraic/racket/base

(require algebraic/class/applicative
         (for-syntax syntax/parse
                     syntax/strip-context))

(provide (all-defined-out))

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

(define-syntax do
  (macro*
    #:literals (let)
    #:datum-literals (<-)

    [(formals <- x block ...)
     #,(replace-context this-syntax #'(>>= x (φ* formals (do block ...))))]

    [(let a x block ...)
     #,(replace-context this-syntax #'(let ([a x]) (do block ...)))]

    [(x) #,(replace-context this-syntax #'x)]

    [(x block ...) #,(replace-context this-syntax #'(>>M x (do block ...)))]))
