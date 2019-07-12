#lang racket/base

(require algebraic/data
         algebraic/pretty
         algebraic/private
         racket/contract/base
         racket/match
         racket/pretty
         (for-syntax algebraic/private
                     racket/base
                     racket/struct-info
                     racket/syntax)
         (for-meta 2 racket/base))

(provide
 φ phi function φ* phi* function*
 (contract-out
  [function? predicate/c]))

(struct fun (def impl)
  #:reflection-name 'function
  #:property prop:procedure (λ (F . args) (apply (fun-impl F) args))
  #:methods gen:custom-write
  [(define (write-proc F port mode)
     (case mode
       [(#t #f) (fprintf port "#<function>")]
       ;; [else (display (syntax->datum (fun-def F)) port)]
       [else (parameterize ([pretty-print-current-style-table
                             algebraic-pretty-print-style-table])
               ((current-print) (syntax->datum (fun-def F))))]))])

(define function? fun?)

(struct exn:algebraic:match exn:fail (value srclocs)
  #:transparent
  #:property prop:exn:srclocs (λ (ex) (exn:algebraic:match-srclocs ex)))

(define-syntax (algebraic-match-error stx)
  (syntax-case stx ()
    [(_ val type ctx)
     #'(raise (exn:algebraic:match
               (format "~a: no matching clause for ~e" type val)
               (current-continuation-marks)
               val
               (list (srcloc (syntax-source   #'ctx)
                             (syntax-line     #'ctx)
                             (syntax-column   #'ctx)
                             (syntax-position #'ctx)
                             (syntax-span     #'ctx)))))]))

(begin-for-syntax
  (define (make-function src type clauses)
    (let ([clauses* (syntax-e clauses)])
      (with-syntax ([ctx (datum->syntax (car clauses*) 'ctx (car clauses*))]
                    [(clause ...) (map (make-clause type argument) clauses*)])
        (syntax/loc src
          (match-lambda
            clause ...
            [val (algebraic-match-error val '#,type ctx)])))))

  (define (make-function* src type clauses)
    (let ([clauses* (syntax-e clauses)])
      (with-syntax ([ctx (datum->syntax (car clauses*) 'ctx (car clauses*))]
                    [(clause ...) (map (make-clause type formals) clauses*)])
        (syntax/loc src
          (λ args
            (match args
              clause ...
              [val (algebraic-match-error val '#,type ctx)]))))))

  (define ((make-clause type maker) clause)
    (let* ([clause* (syntax-e clause)]
           [args (car clause*)]
           [tail (cdr clause*)])
      (with-syntax ([patt (maker args)]
                    [(alias-patt ...) (map argument (aliases tail))]
                    [(con ...) (conditions tail)])
        #`[(and patt alias-patt ...)
           #:when (and con ...)
           #,@(make-side-conditions
               type (consequents tail) (premises tail) (body tail))])))

  (define (make-side-conditions type patts exprs body)
    (if (null? exprs)
        body
        (with-syntax ([ctx (datum->syntax (car exprs) 'ctx (car exprs))]
                      [expr (car exprs)]
                      [patt (argument (car patts))])
          #`((match expr
               [patt #,@(make-side-conditions type (cdr patts) (cdr exprs) body)]
               [val (algebraic-match-error val '#,type ctx)])))))

  (define (formals args)
    (syntax-case args ()
      [id (wildcard? #'id) #'_]
      [id (variable? #'id) #'id]
      [(_ ...) #`(list #,@(map argument (syntax-e args)))]
      [(_ . _) #`(list-rest #,@(rest-args (syntax-e args)))]
      [else (raise-syntax-error #f "invalid formals syntax" args)]))

  (define (argument arg)
    (let ([arg* (syntax-e arg)])
      (syntax-case arg (quote quasiquote void)
        [_ (literal-data? arg) arg]
        [(quote _) arg]

        [(quasiquote datum)
         (let quasi ([stx #'datum])
           (if (identifier? stx)
               (with-syntax ([id stx]) #`'id)
               (syntax-case stx (unquote)
                 [(unquote patt) (argument #'patt)]
                 [(patt1 . patt2) #`(cons #,(quasi #'patt1) #,(quasi #'patt2))]
                 [() #'(list)]
                 [_ stx])))]

        ;; bindings
        [_ (wildcard? arg) #'_]
        [_ (variable? arg) arg]

        ;;   ;; composite data
        ;;   ;;
        ;;   ;; #(patt ...)
        ;;   ;; #&patt
        ;;   ;; #hash([key . patt] ...)
        [_ (vector? arg*)
           (with-syntax ([(patt ...) (map argument (vector->list arg*))])
             #`(vector patt ...))]
        [_ (box? arg*)
           (with-syntax ([patt (argument (unbox arg*))]) #'(box patt))]
        [_ (hash? arg*)
           (let* ([keys (hash-keys arg*)]
                  [key-stx (map (λ (key) (datum->syntax arg key)) keys)])
             (with-syntax ([(key ...) (map maybe-quote/ids key-stx)]
                           [(patt ...) (map (λ (key) (hash-ref arg* key)) keys)])
               #'(hash-table [key patt] ... (_ _) (... ...))))]

        ;; algebraic data
        ;;
        ;; Π
        ;; (Π patt ...)
        [Π1 (product-identifier? #'Π1) #`(? (λ (Π2) (equal? Π2 Π1)))]
        [(Π . _)
         (product-identifier? #'Π)
         #`(product-instance
            #,(argument #'Π)
            #,(if (list? arg*)
                  #`(list #,@(map argument (cdr arg*)))
                  #`(list-rest #,@(rest-args (cdr arg*)))))]

        ;; patt #:as patt
        [(patt #:as alias) #`(and #,(argument #'patt) #,(argument #'alias))]

        ;; patt #:if expr
        [(patt1 #:if expr)
         (with-syntax ([patt (argument #'patt1)])
           #'(and patt (app (match-lambda [patt expr]) (not #f))))]

        ;; regular expressions
        ;;
        ;; rx
        ;; (rx patt ...)
        ;; (rx patt ... . patt)
        [_ (regexp? arg*) #`(app (maybe-regexp-match #,arg*) (not #f))]
        [(rx . _)
         (regexp? (syntax-e #'rx))
         (with-syntax ([(patt ...) (fold-args (cdr arg*))])
           #`(app (maybe-regexp-match rx)
                  (or (? singleton-list? (list patt ...))
                      (#,(if (list? arg*) #'list #'list-rest) _ patt ...))))]

        ;; Racket structs
        ;;
        ;; (struct-id [field-id patt] ...)
        ;; (struct-id patt ...)
        [(S [field-id arg] ...)
         (and (struct-identifier? #'S)
              (member (syntax->datum (car (syntax-e #'(field-id ...))))
                      (struct-field-names #'S)))
         (with-syntax ([(patt ...) (map argument (syntax-e #'(arg ...)))])
           #'(struct* S ([field-id patt] ...)))]

        [(S _ ...)
         (struct-identifier? #'S)
         (with-syntax ([(patt ...) (map argument (cdr arg*))]) #`(S patt ...))]

        ;; void
        [(void) #'(? void?)]

        ;; unquoted lists
        [(_ ...) #`(list #,@(map argument arg*))]
        [(_ . _) #`(cons #,(argument (car arg*))
                          #,(argument #`(#,@(cdr arg*))))]

        [_ (raise-syntax-error #f "invalid pattern" arg)])))

  (define (fold-args args*)
    (if (list? args*) (map argument args*) (rest-args args*)))

  (define (rest-args args*)
    (let loop ([in args*]
               [out null])
      (cond [(pair? in) (loop (cdr in) (cons (argument (car in)) out))]
            [else (reverse (cons (argument in) out))]))))

(define (singleton-list? a)
  (and (list? a)
       (pair? a)
       (null? (cdr a))))

(define ((maybe-regexp-match rx) v)
  (and (or (string? v) (bytes? v) (path? v) (input-port? v))
       (regexp-match rx v)))

(define-syntax (φ stx)
  (syntax-case stx ()
    [(_ . clause) #`(fun #'#,stx #,(make-function stx 'φ #'(clause)))]))

(define-syntax (phi stx)
  (syntax-case stx ()
    [(_ . clause) #`(fun #'#,stx #,(make-function stx 'phi #'(clause)))]))

(define-syntax (function stx)
  (syntax-case stx ()
    [(_ . clauses) #`(fun #'#,stx #,(make-function stx 'function #'clauses))]))

(define-syntax (φ* stx)
  (syntax-case stx ()
    [(_ . clause) #`(fun #'#,stx #,(make-function* stx 'φ* #'(clause)))]))

(define-syntax (phi* stx)
  (syntax-case stx ()
    [(_ . clause) #`(fun #'#,stx #,(make-function* stx 'phi* #'(clause)))]))

(define-syntax (function* stx)
  (syntax-case stx ()
    [(_ . clauses) #`(fun #'#,stx #,(make-function* stx 'function* #'clauses))]))

;;; -----------------------------------------------------------------------------

(module+ test
  (require algebraic/data
           rackunit)

  (define OK (string->unreadable-symbol "OK"))

  (define-simple-check (check-OK val)
    (eq? val OK))

  (test-case "boolean"
    (check-OK ((φ #t OK) #t))
    (check-OK ((φ #f OK) #f))
    (check-exn exn:algebraic:match? (λ () ((φ #t OK) #f)))
    (check-exn exn:algebraic:match? (λ () ((φ #f OK) #t))))

  (test-case "character"
    (check-OK ((φ #\a OK) #\a))
    (check-OK ((φ #\b OK) #\b))
    (check-OK ((φ #\c OK) #\c))
    (check-exn exn:algebraic:match? (λ () ((φ #\a OK) #\b)))
    (check-exn exn:algebraic:match? (λ () ((φ #\b OK) #\c)))
    (check-exn exn:algebraic:match? (λ () ((φ #\c OK) #\a))))

  (test-case "integer"
    (check-OK ((φ 1 OK) 1))
    (check-OK ((φ 2 OK) 2))
    (check-OK ((φ 3 OK) 3))
    (check-exn exn:algebraic:match? (λ () ((φ 1 OK) 2)))
    (check-exn exn:algebraic:match? (λ () ((φ 2 OK) 3)))
    (check-exn exn:algebraic:match? (λ () ((φ 3 OK) 1))))

  (test-case "rational number"
    (check-OK ((φ 1/2 OK) 2/4))
    (check-OK ((φ 3/4 OK) 6/8))
    (check-OK ((φ 5/6 OK) 10/12))
    (check-exn exn:algebraic:match? (λ () ((φ 1/2 OK) 3/4)))
    (check-exn exn:algebraic:match? (λ () ((φ 3/4 OK) 5/6)))
    (check-exn exn:algebraic:match? (λ () ((φ 5/6 OK) 1/2))))

  (test-case "real number"
    (check-OK ((φ 1.2 OK) 1.2))
    (check-OK ((φ 3.4 OK) 3.4))
    (check-OK ((φ 5.6 OK) 5.6))
    (check-exn exn:algebraic:match? (λ () ((φ 1.2 OK) 3.4)))
    (check-exn exn:algebraic:match? (λ () ((φ 3.4 OK) 5.6)))
    (check-exn exn:algebraic:match? (λ () ((φ 5.6 OK) 1.2))))

  (test-case "complex number"
    (check-OK ((φ 1.2+3.4i OK) 1.2+3.4i))
    (check-OK ((φ 5.6+7.8i OK) 5.6+7.8i))
    (check-OK ((φ 9.0+1.2i OK) 9.0+1.2i))
    (check-exn exn:algebraic:match? (λ () ((φ 1.2+3.4i OK) 3.4+5.6i)))
    (check-exn exn:algebraic:match? (λ () ((φ 5.6+7.8i OK) 7.8+9.0i)))
    (check-exn exn:algebraic:match? (λ () ((φ 9.0+1.2i OK) 1.2+3.4i))))

  (test-case "string"
    (check-OK ((φ "a" OK) "a"))
    (check-OK ((φ "b" OK) "b"))
    (check-OK ((φ "c" OK) "c"))
    (check-exn exn:algebraic:match? (λ () ((φ "a" OK) "b")))
    (check-exn exn:algebraic:match? (λ () ((φ "b" OK) "c")))
    (check-exn exn:algebraic:match? (λ () ((φ "c" OK) "a")))
    (check-exn exn:algebraic:match? (λ () ((φ "a" OK) #"a")))
    (check-exn exn:algebraic:match? (λ () ((φ "b" OK) #"b")))
    (check-exn exn:algebraic:match? (λ () ((φ "c" OK) #"c"))))

  (test-case "bytes"
    (check-OK ((φ #"a" OK) #"a"))
    (check-OK ((φ #"b" OK) #"b"))
    (check-OK ((φ #"c" OK) #"c"))
    (check-exn exn:algebraic:match? (λ () ((φ #"a" OK) #"b")))
    (check-exn exn:algebraic:match? (λ () ((φ #"b" OK) #"c")))
    (check-exn exn:algebraic:match? (λ () ((φ #"c" OK) #"a")))
    (check-exn exn:algebraic:match? (λ () ((φ #"a" OK) "a")))
    (check-exn exn:algebraic:match? (λ () ((φ #"b" OK) "b")))
    (check-exn exn:algebraic:match? (λ () ((φ #"c" OK) "c"))))

  (test-case "symbol"
    (check-OK ((φ 'A OK) 'A))
    (check-OK ((φ 'B OK) 'B))
    (check-OK ((φ 'C OK) 'C))
    (check-exn exn:algebraic:match? (λ () ((φ 'A OK) 'B)))
    (check-exn exn:algebraic:match? (λ () ((φ 'B OK) 'C)))
    (check-exn exn:algebraic:match? (λ () ((φ 'C OK) 'A))))

  (test-case "quote"
    (check-OK ((φ '(a) OK) '(a)))
    (check-OK ((φ '(b) OK) '(b)))
    (check-OK ((φ '(c) OK) '(c)))
    (check-exn exn:algebraic:match? (λ () ((φ '(a) OK) '(b))))
    (check-exn exn:algebraic:match? (λ () ((φ '(b) OK) '(c))))
    (check-exn exn:algebraic:match? (λ () ((φ '(c) OK) '(a)))))

  (test-case "wildcard"
    (check-OK ((φ _ OK) 1))
    (check-OK ((φ _ OK) 2))
    (check-OK ((φ _ OK) 3))
    (check-OK ((φ _a OK) 1))
    (check-OK ((φ _b OK) 2))
    (check-OK ((φ _c OK) 3))
    (check-OK ((φ (_a _a) OK) '(1 2))))

  (test-case "variable"
    (check-OK ((φ x x) OK))
    (check-OK ((φ x (and x OK)) #t))
    (check-OK ((φ A (and A OK)) #t))
    (check-false ((φ x (and x OK)) #f))
    (check-false ((φ A (and A OK)) #f)))

  (test-case "alias"
    (check-OK ((φ (_ #:as x) x) OK))
    (check-OK ((φ ((x #:as (#f y)) #:as (z _)) (and x (not z) y)) (list #f OK)))
    (check-exn
     exn:algebraic:match?
     (λ () ((φ ((x #:as (#f y)) #:as (z _)) (and x (not z) y)) (list #t OK)))))

  (test-case "condition"
    (check-OK ((φ _ #:if #t OK) #f))
    (check-OK ((φ x #:if x x) OK))
    (check-exn exn:algebraic:match? (λ () ((φ _ #:if #f OK) #t)))
    (check-exn exn:algebraic:match? (λ () ((φ x #:if x x) #f))))

  (test-case "premise"
    (check-OK ((φ _ #:with (x 1) (list OK 1) x) #f))
    (check-exn exn:algebraic:match? (λ () ((φ _ #:with (x 1) (list OK 2) x) #f)) #f))

  (test-case "void"
    (check-OK ((φ (void) OK) (void)))
    (check-exn exn:algebraic:match? (λ () ((φ (void) OK) #t))))

  (data XYZ (X Y Z))

  (test-case "data bindings"
    (check-OK ((φ X OK) X))
    (check-OK ((φ Y OK) Y))
    (check-OK ((φ Z OK) Z))
    (check-exn exn:algebraic:match? (λ () ((φ X OK) Y)))
    (check-exn exn:algebraic:match? (λ () ((φ Y OK) Z)))
    (check-exn exn:algebraic:match? (λ () ((φ Z OK) X))))

  ;; (test-case "let-data bindings"
  ;;   (let-values ([(x y z) (values X Y Z)])
  ;;     (check equal? X x)
  ;;     (check equal? Y y)
  ;;     (check equal? Z z)
  ;;     (check-OK ((φ X OK) x))
  ;;     (check-OK ((φ Y OK) y))
  ;;     (check-OK ((φ Z OK) z))
  ;;     (let-data ([ZYX (Z Y X)])
  ;;       (check-OK ((φ Z OK) Z))
  ;;       (check-OK ((φ Y OK) Y))
  ;;       (check-OK ((φ X OK) X))
  ;;       (check-exn exn:algebraic:match? (λ () ((φ Z OK) X)))
  ;;       (check-exn exn:algebraic:match? (λ () ((φ Y OK) Z)))
  ;;       (check-exn exn:algebraic:match? (λ () ((φ X OK) Y)))
  ;;       (check-false (equal? Z z))
  ;;       (check-false (equal? Y y))
  ;;       (check-false (equal? X x))
  ;;       (check-exn exn:algebraic:match? (λ () ((φ Z OK) z)))
  ;;       (check-exn exn:algebraic:match? (λ () ((φ Y OK) y)))
  ;;       (check-exn exn:algebraic:match? (λ () ((φ X OK) x))))
  ;;     (check equal? X x)
  ;;     (check equal? Y y)
  ;;     (check equal? Z z)
  ;;     (check-OK ((φ X OK) x))
  ;;     (check-OK ((φ Y OK) y))
  ;;     (check-OK ((φ Z OK) z))))

  ;; (test-case "with-data bindings"
  ;;   (let-values ([(x y z) (values X Y Z)])
  ;;     (with-data (Y Z X)
  ;;       (check-OK ((φ Z OK) Z))
  ;;       (check-OK ((φ Y OK) Y))
  ;;       (check-OK ((φ X OK) X))
  ;;       (check-exn exn:algebraic:match? (λ () ((φ Z OK) X)))
  ;;       (check-exn exn:algebraic:match? (λ () ((φ Y OK) Z)))
  ;;       (check-exn exn:algebraic:match? (λ () ((φ X OK) Y)))
  ;;       (check-false (equal? X x))
  ;;       (check-false (equal? Y y))
  ;;       (check-false (equal? Z z))
  ;;       (check-exn exn:algebraic:match? (λ () ((φ Z OK) z)))
  ;;       (check-exn exn:algebraic:match? (λ () ((φ Y OK) y)))
  ;;       (check-exn exn:algebraic:match? (λ () ((φ X OK) x))))
  ;;     (check equal? X x)
  ;;     (check equal? Y y)
  ;;     (check equal? Z z)))

  (test-case "product-instance"
    (check-OK ((φ (X) OK) (X)))
    (check-OK ((φ (Y) OK) (Y)))
    (check-OK ((φ (Z) OK) (Z)))
    (check-exn exn:algebraic:match? (λ () ((φ (X) OK) X)))
    (check-exn exn:algebraic:match? (λ () ((φ (Y) OK) Y)))
    (check-exn exn:algebraic:match? (λ () ((φ (Z) OK) Z)))
    (check-exn exn:algebraic:match? (λ () ((φ (X) OK) (Y))))
    (check-exn exn:algebraic:match? (λ () ((φ (Y) OK) (Z))))
    (check-exn exn:algebraic:match? (λ () ((φ (Z) OK) (X))))
    (check-OK ((φ (X a) a) (X OK)))
    (check-OK ((φ (Y 1 2 3) OK) (Y 1 2 3)))
    (check-exn exn:algebraic:match? (λ () ((φ (Y 1 2 3) OK) (Y 1 2)))))

  (test-case "regexp"
    (check-OK ((φ #rx"a+b+" OK) "ab"))
    (check-OK ((φ #rx"a+b+" OK) "xxabbby"))
    (check-exn exn:algebraic:match? (λ () ((φ #rx"a+b+" OK) "")))
    (check-exn exn:algebraic:match? (λ () ((φ #rx"^a+b+$" OK) "xxabbby")))
    (check-OK ((φ #rx"(a+)(b+)" OK) "xxabbby"))
    (check-OK ((φ (#rx"a+b+" "abbb") OK) "xxabbby"))
    (check-OK ((φ (#rx"(a+)(b+)" "a" "bbb") OK) "xxabbby"))
    (check-exn exn:algebraic:match? (λ () ((φ (#rx"(a+)(b+)" _) OK) "xxabbby")))
    (check-exn exn:algebraic:match? (λ () ((φ (#rx"(a+)(b+)" _ _ _) OK) "xxabbby")))
    (check-OK ((φ (#rx"(a+)(b+)" . as+bs) (and (equal? as+bs '("a" "bbb")) OK)) "xxabbby"))
    (check-OK ((φ (#rx"(a+)(b+)(y+)" "a" . bs+ys) (and (equal? bs+ys '("bbb" "y")) OK))
               "xxabbby")))

  (test-case "quote"
    (check-OK ((φ '() OK) '()))
    (check-OK ((φ '() OK) null))
    (check-OK ((φ '(x . _) OK) (cons 'x '_)))
    (check-OK ((φ '(x . _) OK) '(x . _)))
    (check-exn exn:algebraic:match? (λ () ((φ '(x . _) OK) (cons 'x 'y))))
    (check-exn exn:algebraic:match? (λ () ((φ '(x . _) OK) (cons 'z '_))))
    (check-OK ((φ '(x y z) OK) '(x y z)))
    (check-OK ((φ '(x y z) OK) (list 'x 'y 'z))))

  (test-case "pair"
    (check-OK ((φ () OK) '()))
    (check-OK ((φ () OK) null))
    (check-OK ((φ (1 . 2) OK) (cons 1 2)))
    (check-OK ((φ (x . _) x) (list OK)))
    (check-OK ((φ (1 2 3) OK) '(1 2 3)))
    (check-OK ((φ (1 2 x) x) (list 1 2 OK)))
    (check-OK ((φ (1 2 . x) x) (list* 1 2 OK))))

  (test-case "vector"
    (check-OK ((φ #() OK) #()))
    (check-OK ((φ #(1 2 x) x) (vector 1 2 OK)))
    (check-exn exn:algebraic:match? (λ () ((φ #(1) OK) #(2)))))

  (test-case "box"
    (check-OK ((φ #&1 OK) #&1))
    (check-OK ((φ #&2 OK) #&2))
    (check-OK ((φ #&3 OK) #&3))
    (check-exn exn:algebraic:match? (λ () ((φ #&1 OK) #&2)))
    (check-exn exn:algebraic:match? (λ () ((φ #&2 OK) #&3)))
    (check-exn exn:algebraic:match? (λ () ((φ #&3 OK) #&1))))

  (test-case "hash"
    (check-OK ((φ #hash() OK) #hash()))
    (check-OK ((φ #hash([A . 1]) OK) #hash([A . 1])))
    (check-OK ((φ #hash([B . 1]) OK) #hash([B . 1])))
    (check-OK ((φ #hash([C . 1]) OK) #hash([C . 1])))
    (check-exn exn:algebraic:match? (λ () ((φ #hash([A . 1]) OK) #hash([B . 2]))))
    (check-exn exn:algebraic:match? (λ () ((φ #hash([B . 2]) OK) #hash([C . 3]))))
    (check-exn exn:algebraic:match? (λ () ((φ #hash([C . 3]) OK) #hash([A . 1]))))
    (check-OK ((φ #hash([A . 1]) OK) #hash([A . 1] [B . 2]))))

  (struct S (x y z))

  (test-case "struct"
    (check-OK ((φ (S 1 2 3) OK) (S 1 2 3)))
    (check-OK ((φ (S 1 2 a) a) (S 1 2 OK)))
    (check-OK ((φ (S [x 1] [y 2] [z 3]) OK) (S 1 2 3)))
    (check-OK ((φ (S [y 2] [x 1] [z 3]) OK) (S 1 2 3)))
    (check-OK ((φ (S [y 2] [z 3] [x 1]) OK) (S 1 2 3)))
    (check-OK ((φ (S [x 1]) OK) (S 1 2 3)))
    (check-exn exn:algebraic:match? (λ () ((φ (S 1 2 3) OK) (S 1 2 4))))
    (check-exn exn:algebraic:match? (λ () ((φ (S 1 2 3) OK) (X 1 2 3)))))

  (test-case "quasiquote"
    (check-OK ((φ `(1 2 ,x) x) (list 1 2 OK)))
    (check-exn exn:algebraic:match? (λ () ((φ `(x y ,z) z) (list 1 2 OK))))
    (check-exn exn:algebraic:match? (λ () ((φ `(1 2 ,x) #:if x OK) '(1 2 #f)))))

  (test-case "premise"
    (check equal?
           ((φ (#rx"^([^ ]+) ([^ ]+) HTTP/([^\r\n]+)" method uri version)
              #:with #rx"^(?:GET|PUT|POST)$" method
              #:with (#rx"^(.+)\\?(.+)$" path params) uri
              #:with #rx"^[0-9]\\.[0-9]$" version
              (list method path params version))
            "GET /r/s?q=123&p=4 HTTP/1.0\r\n\r\n")
           '("GET" "/r/s" "q=123&p=4" "1.0"))

    (check equal?
           ((phi (#rx"^([^ ]+) ([^ ]+) HTTP/([^\r\n]+)" method uri version)
              #:with #rx"^(?:GET|PUT|POST)$" method
              #:with (#rx"^(.+)\\?(.+)$" path params) uri
              #:with #rx"^[0-9]\\.[0-9]$" version
              (list method path params version))
            "GET /r/s?q=123&p=4 HTTP/1.0\r\n\r\n")
           '("GET" "/r/s" "q=123&p=4" "1.0"))

    (check equal?
           ((function [(#rx"^([^ ]+) ([^ ]+) HTTP/([^\r\n]+)" method uri version)
                       #:with #rx"^(?:GET|PUT|POST)$" method
                       #:with (#rx"^(.+)\\?(.+)$" path params) uri
                       #:with #rx"^[0-9]\\.[0-9]$" version
                       (list method path params version)])
            "GET /r/s?q=123&p=4 HTTP/1.0\r\n\r\n")
           '("GET" "/r/s" "q=123&p=4" "1.0"))
    (check equal?
           ((φ* ((#rx"^([^ ]+) ([^ ]+) HTTP/([^\r\n]+)" method uri version))
              #:with #rx"^(?:GET|PUT|POST)$" method
              #:with (#rx"^(.+)\\?(.+)$" path params) uri
              #:with #rx"^[0-9]\\.[0-9]$" version
              (list method path params version))
            "GET /r/s?q=123&p=4 HTTP/1.0\r\n\r\n")
           '("GET" "/r/s" "q=123&p=4" "1.0"))

    (check equal?
           ((phi* ((#rx"^([^ ]+) ([^ ]+) HTTP/([^\r\n]+)" method uri version))
              #:with #rx"^(?:GET|PUT|POST)$" method
              #:with (#rx"^(.+)\\?(.+)$" path params) uri
              #:with #rx"^[0-9]\\.[0-9]$" version
              (list method path params version))
            "GET /r/s?q=123&p=4 HTTP/1.0\r\n\r\n")
           '("GET" "/r/s" "q=123&p=4" "1.0"))

    (check equal?
           ((function* [((#rx"^([^ ]+) ([^ ]+) HTTP/([^\r\n]+)" method uri version))
                        #:with #rx"^(?:GET|PUT|POST)$" method
                        #:with (#rx"^(.+)\\?(.+)$" path params) uri
                        #:with #rx"^[0-9]\\.[0-9]$" version
                        (list method path params version)])
            "GET /r/s?q=123&p=4 HTTP/1.0\r\n\r\n")
           '("GET" "/r/s" "q=123&p=4" "1.0"))))
