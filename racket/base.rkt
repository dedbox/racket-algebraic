#lang racket/base

(require (for-syntax algebraic/racket/base/syntax
                     (except-in racket/base
                                hash void vector regexp quasiquote struct box)
                     racket/match
                     racket/syntax
                     syntax/stx)
         racket/base
         racket/contract/base
         racket/format
         racket/function
         racket/match
         racket/string
         racket/syntax
         syntax/parse/define)

(provide
 (for-syntax literal-value fun-patt)
 (all-from-out racket/base)
 data φ phi function φ* phi* function*
 (contract-out
  [constructor? predicate/c]
  [instance? predicate/c]
  [function? predicate/c]))

;;; Constructors

(struct con (δ)
  #:transparent
  #:property prop:procedure (λ (c . args) (ins c args))
  #:methods gen:custom-write
  [(define (write-proc c port mode)
     (case mode
       [(#t #f) (write (con-δ c) port)]
       [else (display (con-δ c) port)]))])

(struct ins (con vs)
  #:transparent
  #:methods gen:custom-write
  [(define (write-proc i port mode)
     (case mode
       [(#t #f) (write `(,(con-δ (ins-con i)) ,@(ins-vs i)) port)]
       [else (display `(,(con-δ (ins-con i)) ,@(map ~v (ins-vs i))) port)]))])

(define constructor? con?)
(define instance? ins?)

;;; Data

(define-syntax-parser data
  [(_ δ:id ...+)
   #:when (andmap (λ (d) (not (char-lower-case? (first-char d))))
                  (syntax-e #'(δ ...)))
   #'(begin (begin (define-for-syntax δ (scon 'δ)) (define δ (con 'δ))) ...)])

;;; Functions

(define ((maybe-regexp-match rx) v)
  (and ((or/c string? bytes? path? input-port?) v)
       (regexp-match rx v)))

(define (singleton-list? vs)
  (and (pair? vs) (null? (cdr vs))))

(begin-for-syntax
  (define-syntax-class fun-wildcard
    #:description "wildcard"
    #:attributes (compiled)
    (pattern :wildcard #:attr compiled #'_))

  (define-syntax-class fun-variable
    #:description "variable"
    #:attributes (compiled)
    (pattern compiled:variable))

  (define-syntax-class fun-reference
    #:description "variable reference"
    #:attributes (compiled)
    (pattern r:reference
             #:attr compiled #'(app (λ (a) (eq? a r)) #t)))

  (define-syntax-class fun-constructor
    #:description "constructor pattern"
    #:attributes (compiled)
    (pattern δ:constructor #:attr compiled #'(con 'δ)))

  (define-syntax-class fun-instance
    #:description "instance pattern"
    #:attributes (compiled)
    (pattern (δ:fun-constructor ps:fun-patt ...)
             #:attr compiled #'(ins δ.compiled (list ps.compiled ...)))
    (pattern (δ:fun-constructor ps:fun-patt ... . p:fun-patt)
             #:attr compiled #'(ins δ.compiled (list* ps.compiled ... p.compiled))))

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

  (define-syntax-class fun-regexp
    #:description "regexp pattern"
    #:attributes (compiled)
    (pattern (r:regexp p:fun-patt ...+)
             #:attr compiled
             #'(app (maybe-regexp-match (syntax->datum #'r))
                    (or (? singleton-list? (list p.compiled ...))
                        (list _ p.compiled ...))))
    (pattern (r:regexp p:fun-patt ... . rest-p:fun-patt)
             #:attr compiled
             #'(app (maybe-regexp-match (syntax->datum #'r))
                    (or (? singleton-list?
                           (list-rest p.compiled ... rest-p.compiled))
                        (list-rest _ p.compiled ... rest-p.compiled))))
    (pattern r:regexp
             #:attr compiled
             #'(app (maybe-regexp-match (syntax->datum #'r)) (not #f))))

  (define-syntax-class fun-pair
    #:description "pair pattern"
    #:attributes (compiled)
    (pattern () #:attr compiled #'(list))
    (pattern :pair
             #:with pcar:fun-patt (attribute car)
             #:with pcdr:fun-patt (attribute cdr)
             #:attr compiled #'(cons pcar.compiled pcdr.compiled)))

  (define-syntax-class fun-vector
    #:description "vector pattern"
    #:attributes (compiled)
    (pattern v:vector
             #:do [(define items (syntax-e #'(v.item ...)))]
             #:with (p:fun-patt ...) (map maybe-quote/ids items)
             #:attr compiled #'(vector p.compiled ...)))

  (define-syntax-class fun-box
    #:description "box pattern"
    #:attributes (compiled)
    (pattern b:box
             #:with p:fun-patt (maybe-quote/ids #'b.item)
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
    (pattern h:hash
             #:do [(define ks (syntax-e #'(h.k ...)))
                   (define vs (syntax-e #'(h.v ...)))]
             #:with (k:hash-key ...) (map maybe-quote/ids ks)
             #:with (v:fun-patt ...) (map maybe-quote/ids vs)
             #:attr compiled
             #'(hash-table (k.compiled v.compiled) ... (_ _) (... ...))))

  (define-syntax-class fun-struct
    #:description "struct pattern"
    #:attributes (compiled)
    (pattern (id:struct-id ([field:id p:fun-patt] ...))
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
    (pattern _ #:attr compiled this-syntax))

  (define-syntax-class fun-quasiquote
    #:description "pattern quasiquote"
    #:attributes (compiled)
    #:literals (quasiquote)
    (pattern (quasiquote datum:fun-quasiquoted)
             #:attr compiled #'datum.compiled))

  (define-syntax-class fun-patt
    #:description "function pattern"
    #:attributes (compiled)
    #:commit
    (pattern compiled:literal-value)
    (pattern (~or p:fun-wildcard
                  p:fun-variable
                  p:fun-void
                  p:fun-constructor
                  p:fun-reference
                  p:fun-conditional
                  p:fun-instance
                  p:fun-regexp
                  p:fun-struct
                  p:fun-vector
                  p:fun-box
                  p:fun-hash
                  p:fun-quasiquote
                  p:fun-pair)
             #:attr compiled #'p.compiled))

  (define-splicing-syntax-class maybe-if
    #:description #f
    #:attributes (e)
    (pattern (~seq #:if e:expr))))

(struct fun (matcher)
  #:reflection-name 'function
  #:property prop:procedure (λ (f . args) (apply (fun-matcher f) args)))

(define function? fun?)

;;; ----------------------------------------------------------------------------
;;; lifted from racket/match internals

(struct exn:algebraic:match exn:fail (value srclocs)
  #:property prop:exn:srclocs (λ (ex) (exn:algebraic:match-srclocs ex))
  #:transparent)

(define-simple-macro (match:error val form-name)
  #:with srclocs #`(list (srcloc #,(syntax-source this-syntax)
                                 #,(syntax-line this-syntax)
                                 #,(syntax-column this-syntax)
                                 #,(syntax-position this-syntax)
                                 #,(syntax-span this-syntax)))
  (raise (exn:algebraic:match
          (format "~a: no matching clause for ~e" form-name val)
          (current-continuation-marks)
          val
          srclocs)))

;;; ----------------------------------------------------------------------------

(define-simple-macro (function* [(~or (p:fun-patt ...)
                                      (p:fun-patt ...+ . rest-p:fun-patt)
                                      rest-p:fun-patt)
                                 ifs:maybe-if ... t:expr ...+] ...+)
  (fun (λ args (match args
                 [(~? (~@ (list-rest p.compiled ... rest-p.compiled))
                      (~? (~@ (list p.compiled ...))
                          (~@ (list-rest rest-p.compiled))))
                  (~? (~@ #:when (and ifs.e ...))) t ...]
                 ...
                 [_ (match:error args 'function*)]))))

(define-simple-macro (phi* p:fun-patt ifs:maybe-if ... t:expr ...+)
  (fun (λ (arg) (match arg
                  [p.compiled (~? (~@ #:when (and ifs.e ...))) t ...]
                  [_ (match:error arg 'phi*)]))))

(define-simple-macro (φ* p:fun-patt ifs:maybe-if ... t:expr ...+)
  (fun (λ (arg) (match arg
                  [p.compiled (~? (~@ #:when (and ifs.e ...))) t ...]
                  [_ (match:error arg 'φ*)]))))

(define-simple-macro (function [p:fun-patt ifs:maybe-if ... t:expr ...+] ...+)
  (fun (λ (arg) (match arg
                  [p.compiled (~? (~@ #:when (and ifs.e ...))) t ...]
                  ...
                  [_ (match:error arg  'function)]))))

(define-simple-macro (phi p:fun-patt ifs:maybe-if ... t:expr ...+)
  (fun (λ (arg) (match arg
                  [p.compiled (~? (~@ #:when (and ifs.e ...)))
                              t ...]
                  [_ (match:error arg  'phi)]))))

(define-simple-macro (φ p:fun-patt ifs:maybe-if ... t:expr ...+)
  (fun (λ (arg) (match arg
                  [p.compiled (~? (~@ #:when (and ifs.e ...))) t ...]
                  [_ (match:error arg 'φ)]))))

;;; ----------------------------------------------------------------------------

(module+ test
  (require (for-syntax algebraic/racket/macro)
           rackunit
           syntax/macro-testing)

  (define iterations 20)
  (define max-size 3)

  (define OK (string->unreadable-symbol "OK"))
  (define FAIL (string->unreadable-symbol "FAIL"))

  (define-simple-check (check-OK f arg)
    (let ([ret (with-handlers ([exn:algebraic:match? (λ _ FAIL)]
                               [exn:fail? (λ _ (fail-check "Exception raised"))])
                 (f arg))])
      (or (eq? ret OK)
          (with-check-info (['argument arg] ['expected OK] ['actual ret])
            (fail-check "Match failed")))))

  (define-simple-check (check-not-OK f arg)
    (let/ec succeed
      (with-handlers ([exn:algebraic:match? (λ _ (succeed #t))]
                      [exn:fail? (λ _ (fail-check "Wrong exception raised"))])
        (f arg))
      (fail-check "No exception raised")))

  (define-simple-check (check-OK** thunk)
    (let ([ret (with-handlers ([exn:algebraic:match? (λ _ FAIL)]
                               [exn:fail? (λ _ (fail-check "Exception raised"))])
                 (thunk))])
      (or (eq? ret OK)
          (with-check-info (['expected OK] ['actual ret])
            (fail-check "Match failed")))))

  (define-simple-check (check-not-OK** thunk)
    (let/ec succeed
      (with-handlers ([exn:fail:syntax? (λ _ (succeed #t))]
                      [exn:fail? (λ _ (fail-check "Wrong exception raised"))])
        (thunk))
      (fail-check "No exception raised")))

  (define-syntax-rule (check-OK* m arg)
    (check-OK** (λ () (m arg))))

  (define-syntax-rule (check-not-OK* m arg)
    (check-not-OK** (λ () (convert-compile-time-error (m arg)))))

  (define (restrict rand pred)
    (let ([v (rand)])
      (if (pred v) v (restrict rand pred))))

  (define (random-boolean)
    (eq? (random 2) 1))

  (define (random-integer)
    (random #x-10000 #x10000))

  (define (random-positive-integer)
    (random 1 #x10000))

  (define (random-rational)
    (/ (random-integer) (random-positive-integer)))

  (define (random-real)
    (sqrt (random #x10000)))

  (define (random-complex)
    (make-rectangular (random-integer) (random-integer)))

  (define (random-byte)
    (random 256))

  (define (random-bytes)
    (apply bytes (random-list random-byte)))

  (define (random-char)
    (integer->char
     (restrict (λ () (random 0 #x110000))
               (λ (n) (or (< n #xD800) (> n #xDFFF))))))

  (define (random-lowercase-char)
    (integer->char (random (char->integer #\a) (char->integer #\z))))

  (define (random-non-lowercase-char)
    (restrict random-char (λ (c) (not (char-lower-case? c)))))

  (define (random-string)
    (apply string (random-list random-char)))

  (define (random-nonempty-string)
    (apply string (cons (random-char) (random-list random-char))))

  (define (random-symbol)
    (string->symbol (random-nonempty-string)))

  (define (random-nonempty-symbol)
    (string->symbol (random-nonempty-string)))

  (define (random-size)
    (random max-size))

  (define (random-list rand)
    (build-list (random-size) (λ _ (rand))))

  (define (random-choice . rands)
    ((list-ref rands (random (length rands)))))

  (define (random-quote [depth 0])
    (if (= depth 0)
        `,(random-quote 1)
        (random-list
         (λ () (random-choice random-symbol
                              (λ () (random-literal-value depth)))))))

  (define (random-unquoted-literal-value)
    (random-choice random-char
                   random-integer
                   random-rational
                   random-real
                   random-complex
                   random-bytes
                   random-string))

  (define (random-literal-value [depth 0])
    (random-choice random-unquoted-literal-value
                   (λ () (random-quote depth))))

  (define (random-wildcard)
    (string->symbol (format "_~a" (random-symbol))))

  (define (random-variable)
    (string->symbol (format "~a~a" (random-lowercase-char) (random-symbol))))

  (define (random-reference)
    (let ([refs
           (filter (λ (s)
                     (let ([c0 (string-ref (symbol->string s) 0)])
                       (and (not (char=? c0 #\?))
                            (not (char=? c0 #\_))
                            (not (char-lower-case? c0))
                            (namespace-variable-value s #t (λ _ #f)))))
                   (namespace-mapped-symbols (module->namespace 'racket/base)))])
      (list-ref refs (random (length refs)))))

  (define (random-constructor)
    (string->symbol
     (restrict random-string
               (λ (s)
                 (and (> (string-length s) 0)
                      (not (char-lower-case? (string-ref s 0))))))))

  (define (random-pair rand)
    (cons (rand) (rand)))

  (define (random-vector rand)
    (list->vector (random-list rand)))

  (define (random-box rand)
    (box (rand)))

  (define (random-hash-key)
    (random-choice random-symbol random-literal-value))

  (define (random-hash k-rand v-rand)
    (let* ([n (random-size)])
      (make-hash (map cons
                      (build-list n (λ _ (k-rand)))
                      (build-list n (λ _ (v-rand)))))))

  (define (maybe-quote v)
    (if ((or/c symbol? list?) v) #`(quote #,v) #`#,v))

  (define (random-quasiquote)
    #``#,(let recur ([depth (random-size)])
           (if (= depth 0)
               #`#,(random-literal-value)
               #`#,(random-choice
                    (λ () `,(random-symbol))
                    random-literal-value
                    (λ () #`,#,(maybe-quote (random-literal-value)))
                    (λ () (random-list (λ () (recur (- depth 1)))))))))

  (define (random-identifier)
    #`#,(string->symbol
         (format "~a~a" (random-non-lowercase-char) (random-string))))

  (define-namespace-anchor ns)

  (parameterize ([current-namespace (namespace-anchor->namespace ns)])
    (test-case "φ boolean"
      (check-OK (φ #t OK) #t)
      (check-OK (φ #f OK) #f)
      (check-not-OK (φ #t FAIL) #f)
      (check-not-OK (φ #f FAIL) #t))

    (test-case "φ char"
      (for ([_ iterations])
        (define c (random-char))
        (define f (eval-syntax #`(φ #,c OK)))
        (check-OK f c)
        (check-not-OK f FAIL)))

    (test-case "φ integer"
      (for ([_ iterations])
        (define n (random-integer))
        (define f (eval-syntax #`(φ #,n OK)))
        (check-OK f n)
        (check-not-OK f FAIL)))

    (test-case "φ rational"
      (for ([_ iterations])
        (define n (random-rational))
        (define f (eval-syntax #`(φ #,n OK)))
        (check-OK f n)
        (check-not-OK f FAIL)))

    (test-case "φ real"
      (for ([_ iterations])
        (define n (random-real))
        (define f (eval-syntax #`(φ #,n OK)))
        (check-OK f n)
        (check-not-OK f FAIL)))

    (test-case "φ complex"
      (for ([_ iterations])
        (define n (random-complex))
        (define f (eval-syntax #`(φ #,n OK)))
        (check-OK f n)
        (check-not-OK f FAIL)))

    (test-case "φ string"
      (for ([_ iterations])
        (define s (random-string))
        (define f (eval-syntax #`(φ #,s OK)))
        (check-OK f s)
        (check-not-OK f FAIL)))

    (test-case "φ bytes"
      (for ([_ iterations])
        (define b (random-bytes))
        (define f (eval-syntax #`(φ #,b OK)))
        (check-OK f b)
        (check-not-OK f FAIL)))

    (test-case "φ symbol"
      (for ([_ iterations])
        (define s (random-symbol))
        (define f (eval-syntax #`(φ '#,s OK)))
        (check-OK f s)
        (check-not-OK f FAIL)))

    (test-case "φ quote"
      (for ([_ iterations])
        (define q (random-quote))
        (define f (eval-syntax #`(φ '#,q OK)))
        (check-OK f q)
        (check-not-OK f FAIL)))

    (test-case "φ wildcard"
      (for ([_ iterations])
        (define w (random-wildcard))
        (define f (eval-syntax #`(φ '#,w OK)))
        (check-OK f w)))

    (test-case "φ variable"
      (for ([_ iterations])
        (define x (random-variable))
        (define f (eval-syntax #`(φ #,x #,x)))
        (check-OK f OK)))

    (test-case "φ reference"
      (for ([_ iterations])
        (define r (random-reference))
        (define f (eval-syntax #`(φ #,r OK)))
        (check-OK f (eval-syntax #`#,r))
        (check-not-OK f FAIL)))

    (test-case "φ conditional"
      (for ([_ iterations])
        (define f (eval-syntax #`(φ x #:if x OK)))
        (check-OK f #t)
        (check-not-OK f #f)))

    (test-case "φ void"
      (check-OK (φ (void) OK) (void))
      (check-OK (φ (void) OK) (let ([v (void)]) v))
      (check-not-OK (φ (void) FAIL) FAIL))

    (test-case "φ constructor"
      (for ([_ iterations])
        (define δ-name (random-constructor))
        (eval-syntax #`(data #,δ-name))
        (define-values (δ f) (eval-syntax #`(values #,δ-name (φ #,δ-name OK))))
        (check-OK f δ)
        (check-not-OK f FAIL)))

    (test-case "φ instance"
      (for ([_ iterations])
        (define δ-name (random-constructor))
        (define vs (random-list random-literal-value))
        (eval-syntax #`(data #,δ-name))
        (define δ (eval-syntax #`#,δ-name))
        (define f (eval-syntax #`(φ (#,δ-name #,@(map maybe-quote vs))  OK)))
        (check-OK f (apply δ vs))
        (check-not-OK f FAIL)))

    (test-case "φ regexp"
      (for ([_ iterations])
        (let* ([xs (make-string (+ 1 (random-size)) #\x)]
               [ys (make-string (+ 1 (random-size)) #\y)]
               [as (make-string (+ 1 (random-size)) #\a)]
               [bs (make-string (+ 1 (random-size)) #\b)]
               [str (string-append xs as bs ys)]
               [rx-str (string-append "^" as bs "$")]
               [rx (eval-syntax #`(regexp #,rx-str))]
               [fs (list
                    (eval-syntax #`(φ #rx"a+b+" OK))
                    (eval-syntax #`(φ #rx"(a+)(b+)" OK))
                    (eval-syntax
                     #`(φ (#rx"a+b+" ab) (and (regexp-match #,rx ab) OK)))
                    (eval-syntax
                     #`(φ (#rx"(a+)(b+)" ass bss)
                         (and (regexp-match #,rx (string-append ass bss)) OK)))
                    (eval-syntax
                     #`(φ (#rx"(a+)(b+)" . ab)
                         (and (equal? ab (list #,as #,bs)) OK)))
                    (eval-syntax
                     #`(φ (#rx"(a+)(b+)" ass . bss)
                         (and (equal? ass #,as) (equal? bss (list #,bs)) OK))))]
               [ffs (list (eval-syntax #`(φ #rx"x+y+" OK))
                          (eval-syntax #`(φ (#rx"(a+)(b+)" _) OK)))])
          (for ([f fs])
            (check-OK f str)
            (check-not-OK f FAIL))
          (for ([f ffs])
            (check-not-OK f str)))))

    (test-case "φ pair"
      (for ([_ iterations])
        (define p (random-pair random-unquoted-literal-value))
        (define f (eval-syntax #`(φ #,p OK)))
        (check-OK f p)
        (check-not-OK f FAIL))
      (for ([_ iterations])
        (define p (random-pair random-literal-value))
        (define f
          (eval-syntax
           #`(φ (#,(maybe-quote (car p)) . #,(maybe-quote (cdr p))) OK)))
        (check-OK f p)
        (check-not-OK f FAIL)))

    (test-case "φ vector"
      (for ([_ iterations])
        (define v (random-vector random-literal-value))
        (define f (eval-syntax #`(φ #,v OK)))
        (check-OK f v)
        (check-not-OK f FAIL)))

    (test-case "φ box"
      (for ([_ iterations])
        (define b (random-box random-literal-value))
        (define f (eval-syntax #`(φ #,b OK)))
        (check-OK f b)
        (check-not-OK f FAIL)))

    (test-case "φ hash"
      (for ([_ iterations])
        (define h (random-hash random-hash-key random-literal-value))
        (define f (eval-syntax #`(φ #,h OK)))
        (check-OK f h)
        (check-not-OK f FAIL)))

    (test-case "φ struct"
      (for ([_ iterations])
        (define name (random-nonempty-symbol))
        (define fs (random-list random-variable))
        (define vs (map (λ _ (random-literal-value)) fs))
        (eval-syntax #`(struct #,name #,fs #:transparent))
        (define f-stx #`(φ (#,name #,(map list fs (map maybe-quote vs))) OK))
        (define s-stx #`(#,name #,@(map maybe-quote vs)))
        (define f (eval-syntax f-stx))
        (define s (eval-syntax s-stx))
        (with-check-info (['struct-id name]
                          ['fields fs]
                          ['values vs]
                          ['function (syntax->datum f-stx)]
                          ['instance (syntax->datum s-stx)])
          (check-OK f s)
          (check-not-OK f FAIL)))
      (for ([_ iterations])
        (define name (random-nonempty-symbol))
        (define fs (random-list random-variable))
        (define vs (map (λ _ (maybe-quote (random-literal-value))) fs))
        (eval-syntax #`(struct #,name #,fs))
        (define f (eval-syntax #`(φ (#,name #,@(map maybe-quote vs)) OK)))
        (define s (eval-syntax #`(#,name #,@(map maybe-quote vs))))
        (check-OK f s)
        (check-not-OK f FAIL))
      ;; (for ([_ iterations])
      ;;   (define name (random-nonempty-symbol))
      ;;   (define fs (random-list random-variable))
      ;;   (define vs (map (λ _ (maybe-quote (random-literal-value))) fs))
      ;;   (eval-syntax #`(struct #,name #,fs #:prefab))
      ;;   (define s (eval-syntax #`(#,name #,@(map maybe-quote vs))))
      ;;   (define f (eval-syntax #`(φ #,s OK)))
      ;;   (check-OK f s)
      ;;   (check-not-OK f FAIL))
      )

    (test-case "φ quasiquote"
      (for ([_ iterations])
        (define q (random-quasiquote))
        (define f (eval-syntax #`(φ #,q OK)))
        (check-OK f (eval-syntax q))
        (check-not-OK f FAIL)))

    ;; -------------------------------------------------------------------------

    (test-case "μ boolean"
      (define-syntax mt (μ #t OK))
      (define-syntax mf (μ #f OK))
      (check-OK* mt #t)
      (check-OK* mf #f)
      (check-not-OK* mt #f)
      (check-not-OK* mf #t))

    (test-case "μ char"
      (for ([_ iterations])
        (define c (random-char))
        (eval-syntax #`(define-syntax m (μ #,c OK)))
        (eval-syntax #`(check-OK* m #,c))
        (eval-syntax #`(check-not-OK* m FAIL))))

    (test-case "μ integer"
      (for ([_ iterations])
        (define n (random-integer))
        (eval-syntax #`(define-syntax m (μ #,n OK)))
        (eval-syntax #`(check-OK* m #,n))
        (eval-syntax #`(check-not-OK* m FAIL))))

    (test-case "μ rational"
      (for ([_ iterations])
        (define n (random-rational))
        (eval-syntax #`(define-syntax m (μ #,n OK)))
        (eval-syntax #`(check-OK* m #,n))
        (eval-syntax #`(check-not-OK* m FAIL))))

    (test-case "μ real"
      (for ([_ iterations])
        (define n (random-real))
        (eval-syntax #`(define-syntax m (μ #,n OK)))
        (eval-syntax #`(check-OK* m #,n))
        (eval-syntax #`(check-not-OK* m FAIL))))

    (test-case "μ complex"
      (for ([_ iterations])
        (define n (random-complex))
        (eval-syntax #`(define-syntax m (μ #,n OK)))
        (eval-syntax #`(check-OK* m #,n))
        (eval-syntax #`(check-not-OK* m FAIL))))

    (test-case "μ string"
      (for ([_ iterations])
        (define s (random-string))
        (eval-syntax #`(define-syntax m (μ #,s OK)))
        (eval-syntax #`(check-OK* m #,s))
        (eval-syntax #`(check-not-OK* m FAIL))))

    (test-case "μ bytes"
      (for ([_ iterations])
        (define b (random-bytes))
        (eval-syntax #`(define-syntax m (μ #,b OK)))
        (eval-syntax #`(check-OK* m #,b))
        (eval-syntax #`(check-not-OK* m FAIL))))

    (test-case "μ symbol"
      (for ([_ iterations])
        (define s (random-symbol))
        (eval-syntax #`(define-syntax m (μ '#,s OK)))
        (eval-syntax #`(check-OK* m '#,s))
        (eval-syntax #`(check-not-OK* m FAIL))))

    (test-case "μ quote"
      (for ([_ iterations])
        (define q (random-quote))
        (eval-syntax #`(define-syntax m (μ '#,q OK)))
        (eval-syntax #`(check-OK* m '#,q))
        (eval-syntax #`(check-not-OK* m FAIL))))

    (test-case "μ wildcard"
      (for ([_ iterations])
        (define w (random-wildcard))
        (eval-syntax #`(define-syntax m (μ #,w OK)))
        (eval-syntax #`(check-OK* m 0))))

    (test-case "μ variable"
      (for ([_ iterations])
        (define x (random-variable))
        (eval-syntax #`(define-syntax m (μ #,x #,x)))
        (eval-syntax #`(check-OK* m OK))))

    (test-case "μ identifier"
      (for ([_ iterations])
        (define x (random-identifier))
        (eval-syntax #`(define-syntax m (μ #,x OK)))
        (eval-syntax #`(check-OK* m #,x))
        (eval-syntax #`(check-not-OK* m FAIL))))

    (test-case "μ void"
      (eval-syntax #`(define-syntax m (μ (void) OK)))
      (eval-syntax #`(check-OK* m (void)))
      (eval-syntax #`(check-not-OK* m FAIL)))

    (test-case "μ conditional"
      (for ([_ iterations])
        (eval-syntax #`(define-syntax m (μ x #:if (var x) OK)))
        (eval-syntax #`(check-OK* m #t))
        (eval-syntax #`(check-not-OK* m #f))))

    (test-case "μ pair"
      (for ([_ iterations])
        (eval-syntax #`(define-syntax m1 (μ (_ _ _) OK)))
        (eval-syntax #`(define-syntax m2 (μ (_ . _) OK)))
        (eval-syntax #`(check-OK* m1 (1 2 3)))
        (eval-syntax #`(check-OK* m1 (1 2 3 . ())))
        (eval-syntax #`(check-OK* m2 (1 . 2)))
        (eval-syntax #`(check-OK* m2 (1 2 3)))
        (eval-syntax #`(check-OK* m2 (1 2 . 3)))
        (eval-syntax #`(check-not-OK* m1 FAIL))
        (eval-syntax #`(check-not-OK* m2 FAIL))))

    (test-case "μ vector"
      (for ([_ iterations])
        (define v (random-vector random-literal-value))
        (eval-syntax #`(define-syntax m (μ #,v OK)))
        (eval-syntax #`(check-OK* m #,v))
        (eval-syntax #`(check-not-OK* m FAIL))))

    (test-case "μ box"
      (for ([_ iterations])
        (define b (random-box random-literal-value))
        (eval-syntax #`(define-syntax m (μ #,b OK)))
        (eval-syntax #`(check-OK* m #,b))
        (eval-syntax #`(check-not-OK* m FAIL))))

    (test-case "μ struct"
      (for ([_ iterations])
        (define name (random-nonempty-symbol))
        (define fs (random-list random-variable))
        (define vs (map (λ _ (random-literal-value)) fs))
        (eval-syntax #`(struct #,name #,fs))
        (eval-syntax #`(define-syntax m (μ (#,name #,@vs) OK)))
        (eval-syntax #`(check-OK* m (#,name #,@vs)))
        (eval-syntax #`(check-not-OK* m FAIL))))

    (test-case "μ quasiquote"
      (for ([_ iterations])
        (define q (random-quasiquote))
        (eval-syntax #`(define-syntax m (μ #,q OK)))
        (eval-syntax #`(check-OK* m #,q))
        (eval-syntax #`(check-not-OK* m FAIL))))

    (test-case "μ ellipses"
      (for ([_ iterations])
        (define vs (random-list random-literal-value))
        (eval-syntax #`(define-syntax m (μ (x (... ...)) OK)))
        (eval-syntax #`(check-OK* m ()))
        (eval-syntax #`(check-OK* m #,vs))
        (eval-syntax #`(check-not-OK* m FAIL)))
      (for ([_ iterations])
        (define vs (cons (random-literal-value)
                         (random-list random-literal-value)))
        (eval-syntax #`(define-syntax m (μ (x (... ...+)) OK)))
        (eval-syntax #`(check-OK* m #,vs))
        (eval-syntax #`(check-not-OK* m ()))
        (eval-syntax #`(check-not-OK* m FAIL))))))
