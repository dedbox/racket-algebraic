#lang racket/base

(require (for-syntax algebraic/macro
                     racket/base)
         (for-meta 2 racket/base))

(module+ test
  (require algebraic/data
           rackunit
           syntax/macro-testing)

  (define-for-syntax OK (string->uninterned-symbol "OK"))
  (define OK (let-syntax ([go (λ (stx) #`'#,(datum->syntax stx OK))]) go))

  (define-syntax-rule (check-OK def arg)
    (with-check-info (['macro 'def] ['argument 'arg] ['expected OK])
      (let-syntax ([m def])
        (let ([val (m arg)])
          (with-check-info (['actual val])
            (check eq? val OK))))))

  (define-syntax-rule (check-OK* def arg ...)
    (with-check-info (['macro 'def] ['arguments '(arg ...)] ['expected OK])
      (let-syntax ([m def])
        (let ([val (m arg ...)])
          (with-check-info (['actual val])
            (check eq? val OK))))))

  (define-syntax-rule (check-not-OK def arg rx-or-pred)
    (with-check-info (['macro 'def] ['argument 'arg] ['expected rx-or-pred])
      (let-syntax ([m def])
        (check-exn rx-or-pred (λ () (convert-syntax-error (m arg)))))))

  (test-case "μ0 / mu0"
    (check eq? (let-syntax ([ok (μ0 OK)]) ok) OK)
    (check eq? (let-syntax ([ok (mu0 OK)]) ok) OK)
    (check = (let-syntax ([ab (μ0 1 2)]) ab) 2)
    (check = (let-syntax ([ab (mu0 1 2)]) ab) 2))

  (test-case "boolean"
    (check-OK (μ #t OK) #t)
    (check-OK (μ #f OK) #f)
    (check-not-OK (μ #t OK) #f #rx"expected the literal #t")
    (check-not-OK (μ #f OK) #t #rx"expected the literal #f"))

  (test-case "character"
    (check-OK (μ #\a OK) #\a)
    (check-OK (μ #\b OK) #\b)
    (check-OK (μ #\c OK) #\c)
    (check-not-OK (μ #\a OK) #\b #rx"expected the literal #\\\\a")
    (check-not-OK (μ #\b OK) #\c #rx"expected the literal #\\\\b")
    (check-not-OK (μ #\c OK) #\a #rx"expected the literal #\\\\c"))

  (test-case "integer"
    (check-OK (μ 1 OK) 1)
    (check-OK (μ 2 OK) 2)
    (check-OK (μ 3 OK) 3)
    (check-not-OK (μ 1 OK) 2 #rx"expected the literal 1")
    (check-not-OK (μ 2 OK) 3 #rx"expected the literal 2")
    (check-not-OK (μ 3 OK) 1 #rx"expected the literal 3"))

  (test-case "rational number"
    (check-OK (μ 1/2 OK) 2/4)
    (check-OK (μ 3/4 OK) 6/8)
    (check-OK (μ 5/6 OK) 10/12)
    (check-not-OK (μ 1/2 OK) 3/4 #rx"expected the literal 1/2")
    (check-not-OK (μ 3/4 OK) 5/6 #rx"expected the literal 3/4")
    (check-not-OK (μ 5/6 OK) 1/2 #rx"expected the literal 5/6"))

  (test-case "real number"
    (check-OK (μ 1.2 OK) 1.20)
    (check-OK (μ 3.4 OK) 3.40)
    (check-OK (μ 5.6 OK) 5.60)
    (check-not-OK (μ 1.2 OK) 3.4 #rx"expected the literal 1\\.2")
    (check-not-OK (μ 3.4 OK) 5.6 #rx"expected the literal 3\\.4")
    (check-not-OK (μ 5.6 OK) 1.2 #rx"expected the literal 5\\.6"))

  (test-case "complex number"
    (check-OK (μ 1.2+3.4i OK) 1.20+3.40i)
    (check-OK (μ 5.6+7.8i OK) 5.60+7.80i)
    (check-OK (μ 9.0+1.2i OK) 9.00+1.20i)
    (check-not-OK (μ 1.2+3.4i OK) 1.2 #rx"expected the literal 1\\.2\\+3\\.4i")
    (check-not-OK (μ 5.6+7.8i OK) 5.6 #rx"expected the literal 5\\.6\\+7\\.8i")
    (check-not-OK (μ 9.0+1.2i OK) 9.0 #rx"expected the literal 9\\.0\\+1\\.2i"))

  (test-case "string"
    (check-OK (μ "a" OK) "a")
    (check-OK (μ "b" OK) "b")
    (check-OK (μ "c" OK) "c")
    (check-not-OK (μ "a" OK) "b" #rx"expected the literal \"a\"")
    (check-not-OK (μ "b" OK) "c" #rx"expected the literal \"b\"")
    (check-not-OK (μ "c" OK) "a" #rx"expected the literal \"c\"")
    (check-not-OK (μ "a" OK) #"a" #rx"expected the literal \"a\"")
    (check-not-OK (μ "b" OK) #"b" #rx"expected the literal \"b\"")
    (check-not-OK (μ "c" OK) #"c" #rx"expected the literal \"c\""))

  (test-case "bytes"
    (check-OK (μ #"a" OK) #"a")
    (check-OK (μ #"b" OK) #"b")
    (check-OK (μ #"c" OK) #"c")
    (check-not-OK (μ #"a" OK) #"b" #rx"expected the literal #\"a\"")
    (check-not-OK (μ #"b" OK) #"c" #rx"expected the literal #\"b\"")
    (check-not-OK (μ #"c" OK) #"a" #rx"expected the literal #\"c\"")
    (check-not-OK (μ #"a" OK) "a" #rx"expected the literal #\"a\"")
    (check-not-OK (μ #"b" OK) "b" #rx"expected the literal #\"b\"")
    (check-not-OK (μ #"c" OK) "c" #rx"expected the literal #\"c\""))

  (test-case "symbol"
    (check-OK (μ 'A OK) 'A)
    (check-OK (μ 'B OK) 'B)
    (check-OK (μ 'C OK) 'C)
    (check-not-OK (μ 'A OK) 'B #rx"expected the literal symbol `A'")
    (check-not-OK (μ 'B OK) 'C #rx"expected the literal symbol `B'")
    (check-not-OK (μ 'C OK) 'A #rx"expected the literal symbol `C'"))

  (test-case "quote"
    (check-OK (μ '(a) OK) '(a))
    (check-OK (μ '(b) OK) '(b))
    (check-OK (μ '(c) OK) '(c))
    (check-not-OK (μ '(a) OK) '(b) #rx"expected the literal \\(a\\)")
    (check-not-OK (μ '(b) OK) '(c) #rx"expected the literal \\(b\\)")
    (check-not-OK (μ '(c) OK) '(a) #rx"expected the literal \\(c\\)")
    (check-OK (μ '() OK) '())
    (check-OK (μ '() OK) (quote ()))
    (check-OK (μ '(x . _) OK) '(x . _))
    (check-OK (μ '(x . _) OK) (quote (x . _)))
    (check-not-OK (μ '(x . _) OK) '(x . y) #rx"expected the literal \\(x \\. \\_\\)")
    (check-OK (μ '(x y z) OK) '(x y z))
    (check-OK (μ '(x y z) OK) (quote (x y z))))

  (test-case "wildcard"
    (check-OK (μ _ OK) 1)
    (check-OK (μ _ OK) 2)
    (check-OK (μ _ OK) 3)
    (check-OK (μ _a OK) 1)
    (check-OK (μ _b OK) 2)
    (check-OK (μ _c OK) 3)
    (check-OK (μ (_a _a) OK) (1 1))
    (check-OK (μ (_a _a) OK) (1 2)))

  (test-case "variable"
    (check-OK (μ x x) OK)
    (check-OK (μ x (and x OK)) #t))

  (test-case "alias"
    (check-OK (μ (_ #:as x) x) OK)
    (check-OK (μ (x #:as #f) (and (not x) OK)) #f)
    (check-OK (μ ((_ #:as (_ y)) #:as (z _)) (and (not z) y)) (#f OK))
    (check-not-OK (μ ((_ #:as (_ y)) #:as (#f _)) (and (not z) y))
                  (#t OK) #rx"expected the literal #f"))

  (test-case "condition"
    (check-OK (μ _ #:if #t OK) #f)
    (check-OK (μ x #:if (var x) x) OK)
    (check-not-OK (μ _ #:if #f OK) #t #rx"condition failed: #f")
    (check-not-OK (μ x #:if (var x) x) #f #rx"condition failed: \\(var x\\)")
    (check-not-OK (μ x #:if (var x) x) (not #t) #rx"condition failed: \\(var x\\)")
    (check-OK (μ (_ #:if #t) OK) #f)
    (check-OK (μ (x #:if (var x)) OK) #t)
    (check-not-OK (μ (_ #:if #f) OK) #t #rx"condition failed: #f")
    (check-not-OK (μ (x #:if (var x)) OK) #f #rx"condition failed: \\(var x\\)"))

  (test-case "premise"
    (check-OK (μ _ #:with (x 1) #'(OK 1) x) #f)
    (check-not-OK (μ _ #:with (x 1) #'(OK 2) x) #f #rx"expected the literal 1"))

  (data XYZ (X Y Z))

  (test-case "data bindings"
    (check-OK (μ X OK) X)
    (check-OK (μ Y OK) Y)
    (check-OK (μ Z OK) Z)
    (check-not-OK (μ X OK) Y #rx"expected the product X")
    (check-not-OK (μ Y OK) Z #rx"expected the product Y")
    (check-not-OK (μ Z OK) X #rx"expected the product Z"))

  ;; (test-case "let-data bindings"
  ;;   (let-syntax ([x (make-rename-transformer #'X)]
  ;;                [y (make-rename-transformer #'Y)]
  ;;                [z (make-rename-transformer #'Z)])
  ;;     (check equal? X x)
  ;;     (check equal? Y y)
  ;;     (check equal? Z z)
  ;;     (check-OK (μ X OK) x)
  ;;     (check-OK (μ Y OK) y)
  ;;     (check-OK (μ Z OK) z)
  ;;     (let-data ([ZYX (Z Y X)])
  ;;       (check-OK (μ Z OK) Z)
  ;;       (check-OK (μ Y OK) Y)
  ;;       (check-OK (μ X OK) X)
  ;;       (check-not-OK (μ Z OK) X #rx"expected the product Z")
  ;;       (check-not-OK (μ Y OK) Z #rx"expected the product Y")
  ;;       (check-not-OK (μ X OK) Y #rx"expected the product X")
  ;;       (check-false (equal? Z z))
  ;;       (check-false (equal? Y y))
  ;;       (check-false (equal? X x))
  ;;       (check-not-OK (μ Z OK) z #rx"expected the product Z")
  ;;       (check-not-OK (μ Y OK) y #rx"expected the product Y")
  ;;       (check-not-OK (μ X OK) x #rx"expected the product X"))
  ;;     (check equal? X x)
  ;;     (check equal? Y y)
  ;;     (check equal? Z z)
  ;;     (check-OK (μ X OK) x)
  ;;     (check-OK (μ Y OK) y)
  ;;     (check-OK (μ Z OK) z)))

  (test-case "product-instance"
    (check-OK (μ (X) OK) (X))
    (check-OK (μ (Y) OK) (Y))
    (check-OK (μ (Z) OK) (Z))
    (check-not-OK (μ (X) OK) X #rx"expected an instance of product X")
    (check-not-OK (μ (Y) OK) Y #rx"expected an instance of product Y")
    (check-not-OK (μ (Z) OK) Z #rx"expected an instance of product Z")
    (check-not-OK (μ (X) OK) (Y) #rx"expected the product X")
    (check-not-OK (μ (Y) OK) (Z) #rx"expected the product Y")
    (check-not-OK (μ (Z) OK) (X) #rx"expected the product Z")
    (check-OK (μ (X a) a) (X OK))
    (check-OK (μ (Y 1 2 3) OK) (Y 1 2 3))
    (check-not-OK (μ (Y 1 2 3) OK) (Y 1 2 4) #rx"expected the literal 3")
    (check-not-OK (μ (Y 1 2 3) OK) (Y 1 2)
                  #rx"expected more terms starting with the literal 3"))

  (test-case "pair"
    (check-OK (μ () OK) ())
    (check-OK (μ (1 . 2) OK) (1 . 2))
    (check-OK (μ (x . _) x) (OK))
    (check-OK (μ (x . _) x) (OK . #f))
    (check-OK (μ (1 2 3) OK) (1 2 3))
    (check-OK (μ (1 2 x) x) (1 2 OK))
    (check-OK (μ (1 2 . x) x) (1 2 . OK)))

  (test-case "vector"
    (check-OK (μ #() OK) #())
    (check-OK (μ #(1 2 x) x) #(1 2 OK))
    (check-not-OK (μ #(1 2 3) x) (1 2 3) #rx"expected vector")
    (check-not-OK (μ #(1) OK) #(2) #rx"expected the literal 1"))

  (test-case "box"
    (check-OK (μ #&1 OK) #&1)
    (check-OK (μ #&2 OK) #&2)
    (check-OK (μ #&3 OK) #&3)
    (check-not-OK (μ #&1 OK) 1 #rx"expected box")
    (check-not-OK (μ #&2 OK) 2 #rx"expected box")
    (check-not-OK (μ #&3 OK) 3 #rx"expected box")
    (check-not-OK (μ #&1 OK) #&2 #rx"expected the literal 1")
    (check-not-OK (μ #&2 OK) #&3 #rx"expected the literal 2")
    (check-not-OK (μ #&3 OK) #&1 #rx"expected the literal 3"))

  (struct S (x y z))

  (test-case "struct"
    (check-OK (μ (S 1 2 3) OK) (S 1 2 3))
    (check-OK (μ (S 1 2 a) a) (S 1 2 OK))
    (check-not-OK (μ (S 1 2 3) OK) 123 #rx"expected an instance of struct S")
    (check-not-OK (μ (S 1 2 3) OK) (S 1 2 4) #rx"expected the literal 3")
    (check-not-OK (μ (S 1 2 3) OK) (X 1 2 4) #rx"expected the identifier `S'"))

  (test-case "quasiquote"
    (check-OK (μ `(x y ,z) z) (x y OK))
    (check-not-OK (μ `(x y ,z) z) (1 2 OK) #rx"expected the literal symbol `x'"))

  (require (for-syntax syntax/parse))

  (test-case ":syntax-class"
    (check-OK (μ x:number OK) -1)
    (check-not-OK (μ x:number OK) #t #rx"expected number"))

  (test-case "..."
    (check-OK (μ (_ ...) OK) ())
    (check-OK (μ (1 ...) OK) (1))
    (check-not-OK (μ (1 ...) OK) 1 #rx"bad syntax")
    (check-not-OK (μ (1 ...) OK) (2) #rx"expected the literal 1")
    (check-OK (μ (1 2 ...) OK) (1))
    (check-OK (μ (1 2 ...) OK) (1 2))
    (check-OK (μ (1 2 ...) OK) (1 2 2))
    (check-not-OK (μ (1 2 ...) OK) (2) #rx"expected the literal 1"))

  (test-case "...+"
    (check-not-OK (μ (_ ...+) OK) () #rx"expected more terms starting with any term")
    (check-OK (μ (1 ...+) OK) (1))
    (check-OK (μ (1 2 ...+) OK) (1 2))
    (check-OK (μ (1 2 ...+) OK) (1 2 2))
    (check-not-OK (μ (1 2 ...+) OK) (1)
                  #rx"expected more terms starting with the literal 2"))

  (test-case "formals"
    (check-OK* (μ* (x y) (and (= x y) OK)) 1 1)
    (check-OK* (mu* (x y) (and (= x y) OK)) 1 1)
    (check-OK* (macro* [(x y) (and (= x y) OK)]) 1 1)
    (check-OK* (μ* (x . xs) (and (andmap (λ (y) (= x y)) 'xs) OK)) 1 1 1)
    (check-OK* (mu* (x . xs) (and (andmap (λ (y) (= x y)) 'xs) OK)) 1 1 1)
    (check-OK* (macro* [(x . xs) (and (andmap (λ (y) (= x y)) 'xs) OK)]) 1 1 1)
    (check-OK* (μ* xs (and (andmap (λ (y) (= y 1)) 'xs) OK)) 1 1 1 1 1)
    (check-OK* (mu* xs (and (andmap (λ (y) (= y 1)) 'xs) OK)) 1 1 1 1 1)
    (check-OK* (macro* [xs (and (andmap (λ (y) (= y 1)) 'xs) OK)]) 1 1 1 1 1)
    (let-syntax ([m (μ* _ OK)]) (check equal? m OK)))

  (test-case "directives"
    (check-OK (μ _ #:do [(define x #'OK)] #:with y x y) 0)
    (check-OK (μ x #:as y y) OK)
    (check-OK (μ x #:if (var x) OK) #t)
    (check-not-OK (μ x #:if (var x) OK) #f #rx"condition failed: \\(var x\\)"))

  (test-case "options"
    (check-OK (μ #:datum-literals (!!) !! OK) !!)
    (check-OK (mu #:datum-literals (!!) !! OK) !!)
    (check-OK* (μ* #:datum-literals (<<) (_ << x) x) 1 << OK)
    (check-OK* (mu* #:datum-literals (<<) (_ << x) x) 1 << OK)
    (check-OK* (macro #:datum-literals (<- >>) [(<- _) #f] [(>> x) x]) (>> OK))
    (check-OK* (macro* #:datum-literals (<- >>) [(<- _) #f] [(>> x) x]) >> OK)
    (check-not-OK (μ #:datum-literals (!!) !! OK) '!! #rx"expected the literal symbol `!!'")))
