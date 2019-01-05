#lang racket/base

(require algebraic/ext/algebraic
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
    (test-case "1 + 2 = 3"
      (check equal? '(Succ Succ Succ Zero)
             (algebraic
              (letrec ([add mac
                            [(a Zero  ) a           ]
                            [(a Succ b) Succ add a b]])
                add (Succ Zero) Succ Succ Zero))))

    (test-case "2 * 3 = 6"
      (check equal? '(Succ Succ Succ Succ Succ Succ Zero)
             (algebraic
              (letrec ([add mac
                            [(a Zero  ) a           ]
                            [(a Succ b) Succ add a b]]
                       [mul mac
                            [(a Zero  ) Zero         ]
                            [(a Succ b) add a mul a b]])
                mul (Succ Succ Zero) Succ Succ Succ Zero)))))

  (test-case "Booleans"
    (check equal? 'False
           (algebraic
            (letrec ([not fun [(False) True] [(_) False]]
                     [and μ (a b) (fun [(False) False] [(_) b            ]) a]
                     [or  μ (a b) (fun [(False) b    ] [(x) x            ]) a]
                     [xor μ (a b) (fun [(False) b    ] [(x) and (not b) x]) a])
              or (not True) and (xor True True) True))))

  (test-case "Lists"

    (test-case "list 1 2 3 ◊"
      (check equal? '(Cons ($ (Succ Zero)
                              (Cons ($ (Succ Succ Zero)
                                       (Cons ($ (Succ Succ Succ Zero) Nil))))))
             (algebraic
              (letrec ([list mac
                             [(x ◊ ) Cons $ x Nil    ]
                             [(x xs) Cons $ x (list xs)]])
                list (Succ Zero) (Succ Succ Zero) (Succ Succ Succ Zero) ◊))))

    (test-case "reverse list 1 2 3 ◊"
      (check equal? '(Cons
                      ($
                       (Succ Succ Succ Zero)
                       (Cons ($ (Succ Succ Zero) (Cons ($ (Succ Zero) Nil))))))
             (algebraic
              (letrec ([reverse
                        φ xs
                        letrec ([rev fun
                                     [(Nil           a) a                ]
                                     [((Cons $ y ys) a) rev ys Cons $ y a]])
                        rev xs Nil])
                reverse
                Cons $ (Succ Zero)
                (Cons $ (Succ Succ Zero)
                      (Cons $ (Succ Succ Succ Zero) Nil))))))

    (test-case "map Succ list 3 2 1 ◊"
      (check equal? '(Cons ($ (Succ Succ Succ Succ Zero)
                              (Cons ($ (Succ Succ Succ Zero)
                                       (Cons ($ (Succ Succ Zero) Nil))))))
             (algebraic
              (letrec ([map fun
                            [(_ Nil        ) Nil                    ]
                            [(f Cons $ x xs) Cons $ (f x) (map f xs)]])
                map Succ
                (Cons $ (Succ Succ Succ Zero)
                      (Cons ($ (Succ Succ Zero)
                               (Cons ($ (Succ Zero) Nil)))))))))))
