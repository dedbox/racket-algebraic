#lang racket/base

(require algebraic/data
         (for-syntax racket/base)
         (for-meta 2 racket/base))

(provide (all-defined-out))

;;; Basic Data Types

(data Maybe (Nothing Just))
(data Either (Left Right))
(data Ordering (LT EQ GT))
