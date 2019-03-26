#lang info

(define collection "algebraic")
(define deps '("base"))
(define build-deps '("racket-doc"
                     "rackunit-lib"
                     "sandbox-lib"
                     "scribble-lib"
                     "texmath"))
(define scribblings '(("scribblings/algebraic.scrbl" (multi-page))))
