#lang algebraic/racket/base

(require algebraic/racket/base/forms)

(module+ test
  (require rackunit)

  (data XYZ (X Y Z))

  (test-case "λ"
    (check equal? ((λ ((X . xs) (Y . ys)) (list xs ys))
                   (X 1 2 3) (Y 4 5 6))
           '((1 2 3) (4 5 6))))

  (test-case "lambda"
    (check equal?
           ((case-lambda [((X . xs) (Y . ys)) (list xs ys)])
            (X 1 2) (Y 3 4))
           '((1 2) (3 4))))

  (test-case "let"
    (check equal? (let ([(X a b c) (X 1 2 3)]
                        [(Y d e) (Y 4 5)])
                    (list a b c d e))
           '(1 2 3 4 5))
    (check = (let loop ([(X x) (X 0)]) (if (< x 10) (loop (X (add1 x))) x))
           10))

  (test-case "let* "
    (check equal? (let* ([(X a b) (X 1 2)]
                         [(Y c d) (Y a 3)])
                    (list a b c d))
           '(1 2 1 3)))

  (test-case "letrec"
    (letrec
        ([(Y is-even?) (Y (φ (X x) ((|| zero? (.. is-odd? X sub1)) x)))]
         [(Y is-odd?) (Y (φ (X x) ((&& (.. not zero?) (.. is-even? X sub1)) x)))])
      (check-true (is-odd? (X 11)))))

  (test-case "let-values"
    (check equal?
           (let-values ([((X a b) (Y c d)) (id (X 1 2) (Y 3 4))])
             (list a b c d))
           '(1 2 3 4)))

  (test-case "let*-values"
    (check equal? (let*-values ([((X a b)) (X 1 2)]
                                [((Y c d)) (Y a 3)])
                    (list a b c d))
           '(1 2 1 3)))

  (test-case "letrec-values"
    (check equal? (letrec-values ([((X a b)) (X 1 (λ () d))]
                                  [((Y c d)) (Y (λ () a) 2)])
                    (list a (b) (c) d))
           '(1 2 1 2))
    (letrec-values
        ([((Y is-even?)) (Y (φ (X x) ((|| zero? (.. is-odd? X sub1)) x)))]
         [((Y is-odd?)) (Y (φ (X x) ((&& (.. not zero?) (.. is-even? X sub1)) x)))])
      (check-true (is-odd? (X 11)))))

  (test-case "case"
    (check equal? (case (Y 1 2)
                    [(X a b) (list a b)]
                    [(Y a b) (vector a b)])
           #(1 2)))

  (test-case "case-values"
    (check equal? (case-values (id (X 1 2) (Y 3 4))
                    [((Y a b) (Z c d)) (vector a b c d)]
                    [((X a b) (Y c d)) (list a b c d)]
                    [else 1])
           '(1 2 3 4))
    (check = (case-values (id (X 1 2) (Z 3 4))
               [((Y a b) (Z c d)) (vector a b c d)]
               [((X a b) (Y c d)) (list a b c d)]
               [else 0])
           0))

  (test-case "define"
    (define ((X . xs) (Y . ys)) (list (X 1 2) (Y 3 4)))
    (check equal? (list xs ys) '((1 2) (3 4)))
    (define (f (X a b) (Y c d)) (list a b c d))
    (check equal? (f (X 1 2) (Y 3 4)) '(1 2 3 4)))

  (test-case "define-values"
    (define-values ((X a b) (Y c d)) (id (X 1 2) (Y 3 4)))
    (check equal? (list a b c d) '(1 2 3 4))))
