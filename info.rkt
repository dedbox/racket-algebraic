#lang info

(define collection "algebraic")
(define deps '("base"))
(define build-deps '("racket-doc"
                     "rackunit-lib"
                     "sandbox-lib"
                     "scribble-lib"))
(define scribblings '(("scribblings/algebraic.scrbl")))
