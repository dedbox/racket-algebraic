#lang algebraic/core

;;; Numbers

((φ fix
   ((φ add
      (add ((Succ (Succ Zero)) (Succ Zero))))
    (fix (φ add ($ (μ (a Zero) a)
                   (μ (a (Succ b)) (Succ (add (a b)))))))))
 (φ f ((φ x (f (φ y ((x x) y))))
       (φ x (f (φ y ((x x) y)))))))

((φ fix
   ((φ add
      ((φ mul
         (mul ((Succ (Succ Zero)) (Succ (Succ (Succ Zero))))))
       (fix (φ mul ($ (φ (a Zero) Zero)
                      (φ (a (Succ b)) (add (a (mul (a b))))))))))
    (fix (φ add ($ (μ (a Zero) a)
                   (μ (a (Succ b)) (Succ (add (a b)))))))))
 (φ f ((φ x (f (φ y ((x x) y))))
       (φ x (f (φ y ((x x) y)))))))

;;; Booleans

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
       (φ x (f (φ y ((x x) y)))))))

;;; Lists

((φ fix
   ((φ list
      (list ((Succ Zero) ((Succ (Succ Zero)) ((Succ (Succ (Succ Zero))) ◊)))))
    (fix (φ list ($ (μ (x ◊) (Cons ($ x Nil)))
                    (μ (x xs) (Cons ($ x (list xs)))))))))
 (φ f ((φ x (f (φ y ((x x) y))))
       (φ x (f (φ y ((x x) y)))))))

((φ fix
   ((φ rev
      ((φ reverse
         (reverse (Cons ($ (Succ Zero)
                           (Cons ($ (Succ (Succ Zero))
                                    (Cons ($ (Succ (Succ (Succ Zero))) Nil))))))))
       (fix (φ reverse (φ xs (rev (xs Nil)))))))
    (fix (φ rev ($ (φ (Nil a) a)
                   (φ ((Cons ($ y ys)) a) (rev (ys (Cons ($ y a))))))))))
 (φ f ((φ x (f (φ y ((x x) y))))
       (φ x (f (φ y ((x x) y)))))))

((φ fix
   ((φ append
      (append ((Cons ($ (Succ Zero) (Cons ($ (Succ (Succ Zero)) Nil))))
               (Cons ($ (Succ (Succ (Succ Zero)))
                        (Cons ($ (Succ (Succ (Succ (Succ Zero)))) Nil)))))))
    (fix (φ append ($ (φ (Nil ys) ys)
                      (φ ((Cons ($ x xs)) ys) (Cons ($ x (append (xs ys))))))))))
 (φ f ((φ x (f (φ y ((x x) y))))
       (φ x (f (φ y ((x x) y)))))))

((φ fix
   ((φ map
      (map (Succ (Cons ($ (Succ (Succ (Succ Zero)))
                          (Cons ($ (Succ (Succ Zero))
                                   (Cons ($ (Succ Zero) Nil)))))))))
    (fix (φ map ($ (φ (_ Nil) Nil)
                   (φ (f (Cons ($ x xs))) (Cons ($ (f x) (map (f xs))))))))))
 (φ f ((φ x (f (φ y ((x x) y))))
       (φ x (f (φ y ((x x) y)))))))
