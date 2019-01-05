#lang algebraic/hosted

;;; Numbers

(letrec ([fib fun [(n if < n 2) 1] [n + (fib - n 1) fib - n 2]])
  Fibs $ (fib 0) (fib 1) (fib 2) (fib 3) (fib 4) (fib 5) (fib 6))

;;; Booleans

(letrec ([xor μ (a b) (fun [#f b] [x and (not b) x]) a])
  or (not #t) and (xor #t #t) #t)
