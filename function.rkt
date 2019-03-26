#lang racket/base

(require algebraic/data
         algebraic/product
         racket/contract/base
         racket/match
         syntax/parse/define
         (for-syntax algebraic/syntax
                     racket/syntax
                     syntax/parse))

(provide (all-defined-out)
         (for-syntax (all-defined-out)))

(define ((maybe-regexp-match rx) v)
  (and ((or/c string? bytes? path? input-port?) v)
       (regexp-match rx v)))

(define (singleton-list? vs)
  (and (pair? vs) (null? (cdr vs))))

(begin-for-syntax

  (define-syntax-class fun-quote
    #:description "quoted data"
    #:attributes (compiled)
    #:literals (quote)
    (pattern (quote datum) #:attr compiled this-syntax))

  (define-syntax-class fun-wildcard
    #:description "wildcard"
    #:attributes (compiled)
    (pattern :wildcard #:attr compiled #'_))

  (define-syntax-class fun-variable
    #:description "variable"
    #:attributes (compiled)
    (pattern (~and compiled:variable (~not :wildcard))))

  (define-syntax-class fun-product
    #:description "product pattern"
    #:attributes (compiled)
    (pattern (~and Π:id (~not (~or :wildcard :variable)))
             #:when (product-id? #'Π)
             #:with Π? (format-id #'Π "~a?" #'Π)
             #:attr compiled #'(? Π?)))

  (define-syntax-class fun-reference
    #:description "variable reference"
    #:attributes (compiled)
    (pattern (~and x:id (~not (~or :wildcard :variable :fun-product :struct-id)))
             #:when (identifier-binding #'x 0)
             #:attr compiled #'(app (λ (y) (equal? y x)) #t)))

  (define-syntax-class fun-symbol
    #:description "symbol pattern"
    #:attributes (compiled)
    (pattern (~and s:id (~not (~or :wildcard :variable :fun-product :fun-reference)))
             #:attr compiled #''s))

  (define-syntax-class fun-instance
    #:description "instance pattern"
    #:attributes (compiled)
    (pattern (Π:fun-product ps:fun-patt ...)
             #:attr compiled #'(instance Π.compiled (list ps.compiled ...)))
    (pattern (Π:fun-product ps:fun-patt ... . p:fun-patt)
             #:attr compiled #'(instance Π.compiled (list-rest ps.compiled ... p.compiled))))

  (define-syntax-class fun-void
    #:description "void pattern"
    #:attributes (compiled)
    #:literals (void)
    (pattern (void) #:attr compiled #'(? void?)))

  (define-syntax-class fun-conditional
    #:description "conditional pattern"
    #:attributes (compiled)
    (pattern (p:fun-patt #:if t:expr)
             #:attr compiled
             #'(and p.compiled (app (match-lambda [p.compiled t]) (not #f)))))

  (define-syntax-class fun-alias
    #:description "pattern alias"
    #:attributes (compiled)
    (pattern (p1:fun-patt #:as p2:fun-patt)
             #:attr compiled #'(and p1.compiled p2.compiled)))

  (define-syntax-class fun-regexp
    #:description "regexp pattern"
    #:attributes (compiled)
    (pattern (r:regex p:fun-patt ...+)
             #:attr compiled
             #'(app (maybe-regexp-match (syntax->datum #'r))
                    (or (? singleton-list? (list p.compiled ...))
                        (list _ p.compiled ...))))
    (pattern (r:regex p:fun-patt ... . rest-p:fun-patt)
             #:attr compiled
             #'(app (maybe-regexp-match (syntax->datum #'r))
                    (or (? singleton-list?
                           (list-rest p.compiled ... rest-p.compiled))
                        (list-rest _ p.compiled ... rest-p.compiled))))
    (pattern r:regex
             #:attr compiled
             #'(app (maybe-regexp-match (syntax->datum #'r)) (not #f))))

  (define-syntax-class fun-pair
    #:description "pair pattern"
    #:attributes (compiled)
    (pattern (p:fun-patt ...) #:attr compiled #'(list p.compiled ...))
    (pattern (head:fun-patt ...+ . tail:fun-patt)
             #:attr compiled #'(list-rest head.compiled ... tail.compiled)))

  (define-syntax-class fun-vector
    #:description "vector pattern"
    #:attributes (compiled)
    (pattern :expr
             #:do [(define V (syntax-e this-syntax))]
             #:when (vector? V)
             #:with (p:fun-patt ...) (map maybe-quote/ids (vector->list V))
             #:attr compiled #'(vector p.compiled ...)))

  (define-syntax-class fun-box
    #:description "box pattern"
    #:attributes (compiled)
    (pattern :expr
             #:do [(define B (syntax-e this-syntax))]
             #:when (box? B)
             #:with p:fun-patt (maybe-quote/ids (unbox B))
             #:attr compiled #'(box p.compiled)))

  (define-syntax-class hash-key
    #:description "hash pattern key"
    #:attributes (compiled)
    #:commit
    (pattern k:id #:attr compiled #''k)
    (pattern compiled:literal-value))

  (define-syntax-class fun-hash
    #:description "hash pattern"
    #:attributes (compiled)
    (pattern :expr
             #:do [(define H (syntax-e this-syntax))]
             #:when (hash? H)
             #:do [(define ks (hash-keys H))]
             #:with (k ...) ks
             #:with (p ...) (map (λ (k) (hash-ref H k)) ks)
             #:with (k*:hash-key ...) (map maybe-quote/ids (attribute k))
             #:with (p*:fun-patt ...) (map maybe-quote/ids (attribute p))
             #:attr compiled
             #'(hash-table (k*.compiled p*.compiled) ... (_ _) (... ...))))

  (define-syntax-class fun-struct
    #:description "struct pattern"
    #:attributes (compiled)
    (pattern (id:struct-id [field:id p:fun-patt] ...)
             #:attr compiled #'(struct* id ([field p.compiled] ...)))
    (pattern (id:struct-id p:fun-patt ...)
             #:attr compiled #'(id p.compiled ...))
    ;; (pattern s:prefab-struct
    ;;          #:do [(println this-syntax)]
    ;;          #:with (p:fun-patt ...) #'(s.item ...)
    ;;          #:attr compiled #'#s(s.key p.compiled ...))
    )

  (define-syntax-class fun-quasiquoted
    #:attributes (compiled)
    #:literals (unquote)
    (pattern x:id #:attr compiled #''x)
    (pattern (unquote p:fun-patt) #:attr compiled #'p.compiled)
    (pattern () #:attr compiled #'(list))
    (pattern (q1:fun-quasiquoted . q2:fun-quasiquoted)
             #:attr compiled #'(cons q1.compiled q2.compiled))
    (pattern compiled))

  (define-syntax-class fun-quasiquote
    #:description "quasiquoted pattern"
    #:attributes (compiled)
    #:literals (quasiquote)
    (pattern (quasiquote datum:fun-quasiquoted)
             #:attr compiled #'datum.compiled))

  (define-syntax-class fun-patt
    #:description "function pattern"
    #:attributes (compiled)
    #:commit
    (pattern compiled:literal-value)
    (pattern (~or p:fun-quote
                  p:fun-wildcard
                  p:fun-variable
                  p:fun-void
                  p:fun-product
                  p:fun-reference
                  p:fun-conditional
                  p:fun-alias
                  p:fun-instance
                  p:fun-regexp
                  p:fun-struct
                  p:fun-vector
                  p:fun-box
                  p:fun-hash
                  p:fun-quasiquote
                  p:fun-symbol
                  p:fun-pair)
             #:attr compiled #'p.compiled))

  (define-syntax-class fun-arg
    #:description "function argument"
    #:attributes (compiled)
    (pattern p:fun-patt #:attr compiled #'p.compiled))

  (define-syntax-class fun-rest-arg
    #:description "function rest-argument"
    #:attributes (compiled)
    (pattern p:fun-patt #:attr compiled #'p.compiled))

  (define-splicing-syntax-class maybe-if
    #:description #f
    #:attributes (e)
    (pattern (~seq #:if (~describe "condition" e:expr))))

  (define-splicing-syntax-class maybe-as
    #:description #f
    #:attributes (p)
    (pattern (~seq #:as p1:fun-patt)
             #:attr p #'p1.compiled)))

(struct fun (matcher)
  #:reflection-name 'function
  #:property prop:procedure (λ (f . args) (apply (fun-matcher f) args)))

(define function? fun?)

;;; ----------------------------------------------------------------------------
;;; adapted from racket/match internals

(struct exn:algebraic:match exn:fail (value srclocs)
  #:transparent
  #:property prop:exn:srclocs (λ (ex) (exn:algebraic:match-srclocs ex)))

(define-simple-macro (match:error arg form-name ctx)
  #:with source   (datum->syntax this-syntax (syntax-source   #'ctx))
  #:with line     (datum->syntax this-syntax (syntax-line     #'ctx))
  #:with column   (datum->syntax this-syntax (syntax-column   #'ctx))
  #:with position (datum->syntax this-syntax (syntax-position #'ctx))
  #:with span     (datum->syntax this-syntax (syntax-span     #'ctx))
  (let ([val arg])
    (raise (exn:algebraic:match
            (format "~a: no matching clause for ~e" form-name val)
            (current-continuation-marks)
            val
            (list (srcloc (syntax-source   #'ctx)
                          (syntax-line     #'ctx)
                          (syntax-column   #'ctx)
                          (syntax-position #'ctx)
                          (syntax-span     #'ctx)))))))

;;; ----------------------------------------------------------------------------

(define-simple-macro (function* [(~describe
                                  "argument patterns"
                                  (~or (~and rest-id:id (~or :variable :wildcard))
                                       (p:fun-arg ...)
                                       (p:fun-arg ...+ . rest-p:fun-rest-arg)))
                                 ifs:maybe-if ...
                                 as:maybe-as ...
                                 (~describe "function clause body" body:expr) ...+]
                                ...+)
  #:with ctx this-syntax
  (fun (λ args
         (match args
           [(~? (and rest-id as.p ...)
                (~? (and (list-rest p.compiled ... rest-p.compiled) as.p ...)
                    (and (list p.compiled ...) as.p ...)))
            (~? (~@ #:when (and ifs.e ...)))
            body ...]
           ...
           [_ (match:error args 'function* ctx)]))))

(define-simple-macro (phi* (~describe
                            "argument patterns"
                            (~or (~and rest-id:id (~or :variable :wildcard))
                                 (p:fun-arg ...)
                                 (p:fun-arg ...+ . rest-p:fun-rest-arg)))
                       ifs:maybe-if ...
                       as:maybe-as ...
                       (~describe "function body" body:expr) ...+)
  #:with ctx this-syntax
  (fun (λ args
         (match args
           [(~? (and rest-id as.p ...)
                (~? (and (list-rest p.compiled ... rest-p.compiled) as.p ...)
                    (and (list p.compiled ...) as.p ...)))
            (~? (~@ #:when (and ifs.e ...)))
            body ...]
           [_ (match:error args 'phi* ctx)]))))

(define-simple-macro (φ* (~describe
                          "argument patterns"
                          (~or rest-id:id
                               (p:fun-arg ...)
                               (p:fun-arg ...+ . rest-p:fun-rest-arg)))
                       ifs:maybe-if ...
                       as:maybe-as ...
                       (~describe "function body" body:expr) ...+)
  #:with ctx this-syntax
  (fun (λ args
         (match args
           [(~? (and rest-id as.p ...)
                (~? (and (list-rest p.compiled ... rest-p.compiled) as.p ...)
                    (and (list p.compiled ...) as.p ...)))
            (~? (~@ #:when (and ifs.e ...)))
            body ...]
           [_ (match:error args 'φ* ctx)]))))

(define-simple-macro (function
                       [(~describe "argument pattern" p:fun-patt)
                        ifs:maybe-if ...
                        as:maybe-as ...
                        (~describe "function clause body" body:expr) ...+]
                       ...+)
  #:with ctx this-syntax
  (fun
   (λ (arg)
     (match arg
       [(and p.compiled as.p ...) (~? (~@ #:when (and ifs.e ...))) body ...]
       ...
       [_ (match:error arg 'function ctx)]))))

(define-simple-macro (phi (~describe "argument pattern" p:fun-patt)
                       ifs:maybe-if ...
                       as:maybe-as ...
                       (~describe "function body" body:expr) ...+)
  #:with ctx this-syntax
  (fun (λ (arg)
         (match arg
           [(and p.compiled as.p ...) (~? (~@ #:when (and ifs.e ...))) body ...]
           [_ (match:error arg 'phi ctx)]))))

(define-simple-macro (φ (~describe "argument pattern" p:fun-patt)
                       ifs:maybe-if ...
                       as:maybe-as ...
                       (~describe "function body" body:expr) ...+)
  #:with ctx this-syntax
  (fun (λ (arg)
         (match arg
           [(and p.compiled as.p ...) (~? (~@ #:when (and ifs.e ...))) body ...]
           [_ (match:error arg 'φ ctx)]))))

;;; ============================================================================

(module+ test
  (require algebraic/data
           rackunit)

  (define OK (string->unreadable-symbol "OK"))

  (define-simple-check (check-OK val)
    (with-check-info (['expected OK] ['actual val])
      (eq? val OK)))

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
    (check-exn exn:algebraic:match? (λ () ((φ 'C OK) 'A)))
    (check-OK ((φ A OK) 'A))
    (check-OK ((φ B OK) 'B))
    (check-OK ((φ C OK) 'C))
    (check-exn exn:algebraic:match? (λ () ((φ A OK) 'B)))
    (check-exn exn:algebraic:match? (λ () ((φ B OK) 'C)))
    (check-exn exn:algebraic:match? (λ () ((φ C OK) 'A))))

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
    (check-false ((φ x (and x OK)) #f)))

  (test-case "variable reference"
    (check-OK ((φ + OK) +))
    (check-OK ((φ - OK) -))
    (check-OK ((φ * OK) *))
    (check-exn exn:algebraic:match? (λ () ((φ + OK) -)))
    (check-exn exn:algebraic:match? (λ () ((φ - OK) *)))
    (check-exn exn:algebraic:match? (λ () ((φ * OK) +)))
    (define +++ 123)
    (check-OK ((φ +++ OK) +++))
    (check-OK ((φ +++ OK) 123))
    (check-exn exn:algebraic:match? (λ () ((φ +++ OK) 456)))
    (set! +++ 456)
    (check-exn exn:algebraic:match? (λ () ((φ +++ OK) 123)))
    (check-OK ((φ +++ OK) 456))
    (check-OK ((φ +++ OK) +++)))

  (test-case "conditional"
    (check-OK ((φ _ #:if #t OK) #f))
    (check-OK ((φ x #:if x x) OK))
    (check-exn exn:algebraic:match? (λ () ((φ _ #:if #f OK) #t)))
    (check-exn exn:algebraic:match? (λ () ((φ x #:if x x) #f))))

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

  (test-case "let-data bindings"
    (let-values ([(x y z) (values X Y Z)])
      (check equal? X x)
      (check equal? Y y)
      (check equal? Z z)
      (check-OK ((φ X OK) x))
      (check-OK ((φ Y OK) y))
      (check-OK ((φ Z OK) z))
      (let-data ([ZYX (Z Y X)])
        (check-OK ((φ Z OK) Z))
        (check-OK ((φ Y OK) Y))
        (check-OK ((φ X OK) X))
        (check-exn exn:algebraic:match? (λ () ((φ Z OK) X)))
        (check-exn exn:algebraic:match? (λ () ((φ Y OK) Z)))
        (check-exn exn:algebraic:match? (λ () ((φ X OK) Y)))
        (check-false (equal? Z z))
        (check-false (equal? Y y))
        (check-false (equal? X x))
        (check-exn exn:algebraic:match? (λ () ((φ Z OK) z)))
        (check-exn exn:algebraic:match? (λ () ((φ Y OK) y)))
        (check-exn exn:algebraic:match? (λ () ((φ X OK) x))))
      (check equal? X x)
      (check equal? Y y)
      (check equal? Z z)
      (check-OK ((φ X OK) x))
      (check-OK ((φ Y OK) y))
      (check-OK ((φ Z OK) z))))

  (test-case "with-data bindings"
    (let-values ([(x y z) (values X Y Z)])
      (with-data (Y Z X)
        (check-OK ((φ Z OK) Z))
        (check-OK ((φ Y OK) Y))
        (check-OK ((φ X OK) X))
        (check-exn exn:algebraic:match? (λ () ((φ Z OK) X)))
        (check-exn exn:algebraic:match? (λ () ((φ Y OK) Z)))
        (check-exn exn:algebraic:match? (λ () ((φ X OK) Y)))
        (check-false (equal? X x))
        (check-false (equal? Y y))
        (check-false (equal? Z z))
        (check-exn exn:algebraic:match? (λ () ((φ Z OK) z)))
        (check-exn exn:algebraic:match? (λ () ((φ Y OK) y)))
        (check-exn exn:algebraic:match? (λ () ((φ X OK) x))))
      (check equal? X x)
      (check equal? Y y)
      (check equal? Z z)))

  (test-case "instance"
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
    (check-exn exn:algebraic:match? (λ () ((φ `(1 2 ,x) #:if x OK) '(1 2 #f))))))
