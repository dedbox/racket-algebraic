#lang racket/base

(require algebraic/data
         algebraic/function
         algebraic/prelude
         (for-syntax algebraic/macro
                     algebraic/prelude
                     racket/base)
         (for-meta 2 racket/base))

(provide (all-from-out algebraic/data
                       algebraic/function
                       algebraic/prelude)
         (for-syntax (all-from-out algebraic/macro
                                   algebraic/prelude
                                   racket/base))
         (for-meta 2 (all-from-out racket/base)))

(module reader syntax/module-reader
  algebraic/racket/base/lang)

