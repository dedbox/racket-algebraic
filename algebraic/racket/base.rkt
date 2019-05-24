#lang racket/base

(require algebraic/class
         algebraic/data
         algebraic/function
         algebraic/prelude
         (for-syntax algebraic/class
                     algebraic/macro
                     algebraic/prelude
                     racket/base)
         (for-meta 2 racket/base))

(provide (all-from-out algebraic/class
                       algebraic/data
                       algebraic/function
                       algebraic/prelude
                       racket/base)
         (for-syntax (all-from-out algebraic/class
                                   algebraic/macro
                                   algebraic/prelude
                                   racket/base))
         (for-meta 2 (all-from-out racket/base)))

;;; ----------------------------------------------------------------------------

(module reader syntax/module-reader
  algebraic/racket/base)
