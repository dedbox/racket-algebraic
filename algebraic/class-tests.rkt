#lang algebraic/racket/base

(module+ test
  (require rackunit
           syntax/macro-testing)

  (class Eq
    [== (.. not /=)]
    [/= (.. not ==)]
    minimal ([==] [/=]))

  (test-case "class"
    (check-pred class? Eq)
    (check-true (phase1-eval (class-id? #'Eq)))
    (check-exn exn:fail:syntax? (λ () (convert-compile-time-error ==)))
    (check-exn exn:fail:syntax? (λ () (convert-compile-time-error /=))))

  (test-case "instance"
    (check-true (phase1-eval (instance-id? #'EqEq)))
    (check-true (phase1-eval (instance-id? #'StringEq)))
    (check-exn exn:fail:syntax? (λ () (convert-compile-time-error ==)))
    (check-exn exn:fail:syntax? (λ () (convert-compile-time-error /=))))

  (define-syntax EqEq (instance Eq [== eq?]))
  (define-syntax StringEq (instance Eq [== string=?]))

  (test-case "with-instance"
    (with-instance [|| EqEq]
      (check == + +)
      (check == - -)
      (check /= + -)
      (check /= - +)
      (check-false (== + -))
      (check-false (== - +))
      (check-false (/= + +))
      (check-false (/= - -)))
    (with-instance StringEq
      (check == "A" "A")
      (check == "Z" "Z")
      (check-false (== "A" "Z"))
      (check-false (== "Z" "A"))
      (check /= "A" "Z")
      (check /= "Z" "A")
      (check-false (/= "A" "A"))
      (check-false (/= "Z" "Z"))))

  (test-case "with-instance prefix"
    (with-instance [E: EqEq]
      (check E:== + +)
      (check E:== - -)
      (check E:/= + -)
      (check E:/= - +)
      (check-false (E:== + -))
      (check-false (E:== - +))
      (check-false (E:/= + +))
      (check-false (E:/= - -)))
    (with-instance [S: StringEq]
      (check S:== "A" "A")
      (check S:== "Z" "Z")
      (check-false (S:== "A" "Z"))
      (check-false (S:== "Z" "A"))
      (check S:/= "A" "Z")
      (check S:/= "Z" "A")
      (check-false (S:/= "A" "A"))
      (check-false (S:/= "Z" "Z"))))

  (define-syntax EqEq2 (instance Eq extends (EqEq)))
  (define-syntax EqEq3 (instance Eq extends (EqEq2)))

  (test-case "instance extends"
    (with-instance EqEq3
      (check == + +)
      (check == - -)
      (check /= + -)
      (check /= - +)
      (check-false (== + -))
      (check-false (== - +))
      (check-false (/= + +))
      (check-false (/= - -))))

  (test-case "with-instances"
    (with-instances (EqEq [S: StringEq])
      (check == + +)
      (check == - -)
      (check /= + -)
      (check /= - +)
      (check-false (== + -))
      (check-false (== - +))
      (check-false (/= + +))
      (check-false (/= - -))
      (check S:== "A" "A")
      (check S:== "Z" "Z")
      (check-false (S:== "A" "Z"))
      (check-false (S:== "Z" "A"))
      (check S:/= "A" "Z")
      (check S:/= "Z" "A")
      (check-false (S:/= "A" "A"))
      (check-false (S:/= "Z" "Z"))))

  (class Frob [**] [*** (λ xs ($ ** ($ ** xs)))])

  (define-syntax ListFrob (instance Frob [** ++]))

  (instantiate ListFrob)

  (test-case "instantiate"
    (check equal? (** '(1 2 3) '(4 5 6)) '(1 2 3 4 5 6))
    (check equal? (*** '((1 2) (3 4))
                       '((5 6) (7 8))) '(1 2 3 4 5 6 7 8)))

  (instantiate E: EqEq)
  (instantiate S: StringEq)

  (test-case "instantiate prefix"
    (check E:== + +)
    (check E:== - -)
    (check E:/= + -)
    (check E:/= - +)
    (check-false (E:== + -))
    (check-false (E:== - +))
    (check-false (E:/= + +))
    (check-false (E:/= - -))
    (check S:== "A" "A")
    (check S:== "Z" "Z")
    (check-false (S:== "A" "Z"))
    (check-false (S:== "Z" "A"))
    (check S:/= "A" "Z")
    (check S:/= "Z" "A")
    (check-false (S:/= "A" "A"))
    (check-false (S:/= "Z" "Z"))))
