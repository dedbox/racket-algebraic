#lang algebraic/ext

;;; Numbers

(letrec ([add mac
              [(a Zero  ) a           ]
              [(a Succ b) Succ add a b]])
  add (Succ Zero) Succ Succ Zero)

(letrec ([add mac
              [(a Zero  ) a           ]
              [(a Succ b) Succ add a b]]
         [mul mac
              [(a Zero  ) Zero         ]
              [(a Succ b) add a mul a b]])
  mul (Succ Succ Zero) Succ Succ Succ Zero)

;;; Booleans

(letrec ([not fun [(False) True] [(_) False]]
         [and μ (a b) (fun [(False) False] [(_) b            ]) a]
         [or  μ (a b) (fun [(False) b    ] [(x) x            ]) a]
         [xor μ (a b) (fun [(False) b    ] [(x) and (not b) x]) a])
  or (not True) and (xor True True) True)

;;; Lists

(letrec ([list mac
               [(x ◊ ) Cons $ x Nil    ]
               [(x xs) Cons $ x (list xs)]])
  list (Succ Zero) (Succ Succ Zero) (Succ Succ Succ Zero) ◊)

(letrec ([reverse φ xs
                  letrec ([rev fun
                               [(Nil           a) a                ]
                               [((Cons $ y ys) a) rev ys Cons $ y a]])
                  rev xs Nil])
  reverse
  Cons $ (Succ Zero)
  (Cons $ (Succ Succ Zero)
        (Cons $ (Succ Succ Succ Zero) Nil)))

(letrec ([map fun
              [(_ Nil        ) Nil                    ]
              [(f Cons $ x xs) Cons $ (f x) (map f xs)]])
  map Succ
  Cons $ (Succ Succ Succ Zero) (Cons ($ (Succ Succ Zero) (Cons ($ (Succ Zero) Nil)))))
