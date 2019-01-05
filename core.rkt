#lang racket/base

(require algebraic/core/algebraic
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
    (test-case "2 + 1 = 3"
      (check equal? '(Succ (Succ (Succ Zero)))
             (algebraic
              ((φ fix
                 ((φ add
                    (add ((Succ (Succ Zero)) (Succ Zero))))
                  (fix (φ add ($ (μ (a Zero) a)
                                 (μ (a (Succ b)) (Succ (add (a b)))))))))
               (φ f ((φ x (f (φ y ((x x) y))))
                     (φ x (f (φ y ((x x) y))))))))))

    (test-case "2 * 3 = 6"
      (check equal? '(Succ (Succ (Succ (Succ (Succ (Succ Zero))))))
             (algebraic
              ((φ fix
                 ((φ add
                    ((φ mul
                       (mul ((Succ (Succ Zero)) (Succ (Succ (Succ Zero))))))
                     (fix (φ mul ($ (φ (a Zero) Zero)
                                    (φ (a (Succ b)) (add (a (mul (a b))))))))))
                  (fix (φ add ($ (μ (a Zero) a)
                                 (μ (a (Succ b)) (Succ (add (a b)))))))))
               (φ f ((φ x (f (φ y ((x x) y))))
                     (φ x (f (φ y ((x x) y)))))))))))

  (test-case "Booleans"
    (check equal? 'False
           (algebraic
            ((φ fix
               ((φ not
                  ((φ and
                     ((φ or
                        ((φ xor
                           (or ((not True) (and ((xor (True True)) True)))))
                         (fix (φ xor (μ (a b) (($ (φ False b) (φ x (and ((not b) x)))) a))))))
                      (fix (φ or (μ (a b) (($ (φ False b) (φ x x)) a))))))
                   (fix (φ and (μ (a b) (($ (φ False False) (φ _ b)) a))))))
                (fix (φ not ($ (φ False True) (φ _ False))))))
             (φ f ((φ x (f (φ y ((x x) y))))
                   (φ x (f (φ y ((x x) y))))))))))

  (test-case "Lists"
    (test-case "list 1 2 3 ◊"
      (check equal?
             '(Cons ($ (Succ Zero)
                       (Cons ($ (Succ (Succ Zero))
                                (Cons ($ (Succ (Succ (Succ Zero))) Nil))))))
             (algebraic
              ((φ fix
                 ((φ list
                    (list ((Succ Zero)
                           ((Succ (Succ Zero)) ((Succ (Succ (Succ Zero))) ◊)))))
                  (fix (φ list ($ (μ (x ◊) (Cons ($ x Nil)))
                                  (μ (x xs) (Cons ($ x (list xs)))))))))
               (φ f ((φ x (f (φ y ((x x) y))))
                     (φ x (f (φ y ((x x) y))))))))))

    (test-case "reverse list 1 2 3 ◊"
      (check equal? '(Cons ($ (Succ (Succ (Succ Zero)))
                              (Cons ($ (Succ (Succ Zero))
                                       (Cons ($ (Succ Zero) Nil))))))
             (algebraic
              ((φ fix
                 ((φ rev
                    ((φ reverse
                       (reverse
                        (Cons ($ (Succ Zero)
                                 (Cons ($ (Succ (Succ Zero))
                                          (Cons ($ (Succ (Succ (Succ Zero)))
                                                   Nil))))))))
                     (fix (φ reverse (φ xs (rev (xs Nil)))))))
                  (fix
                   (φ rev
                     ($ (φ (Nil a) a)
                        (φ ((Cons ($ y ys)) a) (rev (ys (Cons ($ y a))))))))))
               (φ f ((φ x (f (φ y ((x x) y))))
                     (φ x (f (φ y ((x x) y))))))))))

    (test-case "append (list 1 2 ◊) list 3 4 ◊"
      (check equal?
             '(Cons ($ (Succ Zero)
                       (Cons ($ (Succ (Succ Zero))
                                (Cons ($ (Succ (Succ (Succ Zero)))
                                         (Cons ($ (Succ (Succ (Succ (Succ Zero))))
                                                  Nil))))))))
             (algebraic
              ((φ fix
                 ((φ append
                    (append
                     ((Cons ($ (Succ Zero) (Cons ($ (Succ (Succ Zero)) Nil))))
                      (Cons ($ (Succ (Succ (Succ Zero)))
                               (Cons ($ (Succ (Succ (Succ (Succ Zero))))
                                        Nil)))))))
                  (fix
                   (φ append
                     ($ (φ (Nil ys) ys)
                        (φ ((Cons ($ x xs)) ys) (Cons ($ x (append (xs ys))))))))))
               (φ f ((φ x (f (φ y ((x x) y))))
                     (φ x (f (φ y ((x x) y))))))))))

    (test-case "map Succ list 3 2 1 ◊"
      (check equal? '(Cons ($ (Succ (Succ (Succ (Succ Zero))))
                              (Cons ($ (Succ (Succ (Succ Zero)))
                                       (Cons ($ (Succ (Succ Zero)) Nil))))))
             (algebraic
              ((φ fix
                 ((φ map
                    (map (Succ
                          (Cons ($ (Succ (Succ (Succ Zero)))
                                   (Cons ($ (Succ (Succ Zero))
                                            (Cons ($ (Succ Zero) Nil)))))))))
                  (fix
                   (φ map
                     ($ (φ (_ Nil) Nil)
                        (φ (f (Cons ($ x xs))) (Cons ($ (f x) (map (f xs))))))))))
               (φ f ((φ x (f (φ y ((x x) y))))
                     (φ x (f (φ y ((x x) y))))))))))))
