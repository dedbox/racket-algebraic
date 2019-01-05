#lang racket/base

(require algebraic/hosted/algebraic
         racket/pretty
         syntax/parse/define)

(provide #%app #%datum
         (rename-out [module-begin #%module-begin]
                     [top-interaction #%top-interaction]))

(define-simple-macro (module-begin form ...)
  (#%plain-module-begin (pretty-write (algebraic form)) ...))

(define-simple-macro (top-interaction . form)
  (#%top-interaction . (algebraic form)))

(module+ test
  (require rackunit)

  (test-case "Numbers"
    (check equal? '(Fibs ($ 1 1 2 3 5 8 13))
           (algebraic
            (letrec ([fib fun [(n if < n 2) 1] [n + (fib - n 1) fib - n 2]])
              Fibs $ (fib 0) (fib 1) (fib 2) (fib 3) (fib 4) (fib 5) (fib 6)))))

  (test-case "Booleans"
    (check-false (algebraic
                  (letrec ([xor μ (a b) (fun [#f b] [x and (not b) x]) a])
                    or (not #t) and (xor #t #t) #t)))))
