#lang racket/base

(require algebraic/data
         algebraic/function
         (for-syntax algebraic/macro
                     racket/base)
         (for-meta 2 racket/base))

(provide (all-from-out algebraic/data
                       algebraic/function)
         (for-syntax (all-from-out algebraic/macro
                                   racket/base))
         (for-meta 2 (all-from-out racket/base)))

(module reader syntax/module-reader
  algebraic/racket/base/lang)

