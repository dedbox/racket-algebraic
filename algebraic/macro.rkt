#lang racket/base

(require algebraic/syntax
         racket/contract/base
         racket/syntax
         syntax/parse
         (for-template racket/base)
         (for-syntax algebraic/data/product
                     algebraic/syntax
                     racket/base
                     syntax/parse)
         (for-meta 2 racket/base))

(provide
 μ mu macro μ* mu* macro* var ...+
 (contract-out
  [macro? predicate/c]))

(struct mac (def impl)
  #:reflection-name 'macro
  #:property prop:procedure (λ (M . args) (apply (mac-impl M) args))
  #:methods gen:custom-write
  [(define (write-proc M port mode)
     (case mode
       [(#t #f) (fprintf port "#<macro>")]
       [else (display (syntax->datum (mac-def M)) port)]))])

(define macro? mac?)

(begin-for-syntax
  (define (make-μ type options+clause)
    (let ([options+clause* (syntax-e options+clause)])
      (let ([options (skip-clauses options+clause*)]
            [clause (datum->syntax options+clause (skip-options options+clause*))])
        #`(syntax-parser
            #,@options
            #,((make-clause (λ (a) #`(_ #,(argument a)))) clause)))))

  (define (make-μ* type options+clause)
    (let ([options+clause* (syntax-e options+clause)])
      (let ([options (skip-clauses options+clause*)]
            [clause (datum->syntax options+clause (skip-options options+clause*))])
        (with-syntax ([clause ((make-clause formals) clause)])
          #`(syntax-parser #,@options clause)))))

  (define (make-macro type options+clauses)
    (let ([options+clauses* (syntax-e options+clauses)])
      (let ([options (skip-clauses options+clauses*)]
            [clauses (skip-options options+clauses*)])
        #`(syntax-parser
            #,@options
            #,@(map (make-clause (λ (a) #`(_ #,(argument a)))) clauses)))))

  (define (make-macro* type options+clauses)
    (let ([options+clauses* (syntax-e options+clauses)])
      (let ([options (skip-clauses options+clauses*)]
            [clauses (skip-options options+clauses*)])
        (with-syntax ([(clause ...) (map (make-clause formals) clauses)])
          #`(syntax-parser #,@options clause ...)))))

  (define (skip-clauses options+clauses*)
    (let ([next (next-option options+clauses*)])
      (if (not next) null (append (car next) (skip-clauses (cdr next))))))

  (define (skip-options options+clauses*)
    (let ([next (next-option options+clauses*)])
      (if next (skip-options (cdr next)) options+clauses*)))

  (define (next-option options+clauses*)
    (and (keyword-syntax? (car options+clauses*))
         (case (keyword-syntax->string (car options+clauses*))
           [("track-literals"
             "disable-colon-notation")
            (cons (list (car options+clauses*)) (cdr options+clauses*))]
           [("context"
             "literals"
             "datum-literals"
             "literal-sets"
             "conventions"
             "local-conventions")
            (cons (list (car options+clauses*)
                        (cadr options+clauses*))
                  (cddr options+clauses*))]
           [else #f])))

  (define ((make-clause maker) clause)
    (let* ([clause* (syntax-e clause)]
           [tail (cdr clause*)])
      (with-syntax ([patt (maker (car clause*))]
                    [(alias-patt ...) (map maker (aliases tail))])
        #`[(~and patt alias-patt ...)
           #,@(side-conditions (consequents tail) (premises tail))
           #,@(post-conditions (conditions tail))
           #,(singleton (body tail))])))

  (define (side-conditions consequents premises)
    (if (null? premises)
        null
        (with-syntax ([patt (argument (car consequents))]
                      [expr (car premises)])
          (list* #'#:with #'patt #'#`expr
                 (side-conditions (cdr consequents) (cdr premises))))))

  (define (post-conditions cond-patts)
    (if (null? cond-patts)
        null
        (with-syntax ([patt (car cond-patts)])
          (list* #'#:post
                 #'(~fail #:unless patt (format "condition failed: ~a" 'patt))
                 (post-conditions (cdr cond-patts))))))

  (define (singleton xs)
    (datum->syntax
     (car xs)
     (list #'quasisyntax
           (if (null? (cdr xs))
               (car xs)
               (datum->syntax (car xs) (list* #'begin xs))))))

  (define (formals args)
    (syntax-case args ()
      [id (wildcard? #'id) #'_]
      [id (variable? #'id) #'(_ . id)]
      [(_ ...) #`(_ #,@(map argument (syntax-e args)))]
      [(head ... . tail)
       (with-syntax ([(patt ...) (map argument (syntax-e #'(head ...)))]
                     [rest-patt (argument #'tail)])
         #'(_ patt ... . rest-patt))]
      [_ (raise-syntax-error #f "invalid formals syntax" args)]))

  (define (argument arg)
    (let ([arg* (syntax-e arg)])
      (syntax-case arg (... ...+)
        [... arg]
        [...+ arg]
        [_ 
         (syntax-case arg (quote quasiquote)
           [_ (literal-data? arg) arg]
           [(quote datum) #`(quote (~datum datum))]

           [(quasiquote datum)
            (let quasi ([stx #'datum])
              (if (identifier? stx)
                  #`(~datum #,stx)
                  (syntax-case stx (unquote)
                    [(unquote patt) (argument #'patt)]
                    [(patt1 . patt2) #`(#,(quasi #'patt1) . #,(quasi #'patt2))]
                    [_ stx])))]

           ;; composite data
           ;;
           ;; #(patt ...)
           ;; #&patt
           [_ (vector? arg*)
              (with-syntax ([(patt ...) (map argument (vector->list arg*))])
                #'(~describe "vector" #(patt ...)))]
           [_ (box? arg*)
              (with-syntax ([patt (argument (unbox arg*))])
                #'(~describe "box" #&patt))]

           ;; bindings
           [_ (wildcard? arg) #'_]
           [_ (variable? arg) arg]

           ;; algebraic data
           ;;
           ;; Π
           ;; (Π patt ...)
           [Π1 (product-identifier? #'Π1)
               #'(~and (~var Π2 id)
                       (~fail #:unless (and (product-identifier? #'Π2)
                                            (equal? (syntax-local-value #'Π1)
                                                    (syntax-local-value #'Π2)))
                              (format "expected the product ~a" 'Π1)))]

           [(Π . _)
            (product-identifier? #'Π)
            (with-syntax ([patt (argument #'Π)])
              #`(~describe
                 (format "an instance of product ~a" 'Π)
                 (patt ~! #,@(if (list? arg*)
                                 (map argument (cdr arg*))
                                 (rest-args (cdr arg*))))))]

           ;; pattern aliases
           [(patt #:as alias) #`(~and #,(argument #'patt) #,(argument #'alias))]

           ;; pattern guards
           [(patt #:if expr)
            #`(~and #,(argument #'patt)
                    (~fail #:unless expr (format "condition failed: ~a" 'expr)))]

           ;; Racket structs
           ;;
           ;; (struct-id [field-id patt] ...)
           ;; (struct-id patt ...)
           [(S _ ...)
            (struct-identifier? #'S)
            (with-syntax ([(patt ...) (map argument (cdr arg*))])
              #'(~describe (format "an instance of struct ~a" 'S)
                           ((~describe "struct name" (~literal S))
                            (~describe "struct field" patt) ...)))]

           ;; unquoted lists
           [(_ ...) #`#,(map argument arg*)]
           [(_ . _) #`#,(cons (argument (car arg*)) (argument #`#,(cdr arg*)))]

           [_ (raise-syntax-error #f "invalid pattern syntax" arg)])])))

  (define (rest-args arg*)
    (if (pair? arg*)
        (cons (argument (car arg*)) (rest-args (cdr arg*)))
        (argument arg*))))

(define-syntax (μ stx)
  (syntax-case stx (...)
    [(_ . options+clause)
     #`(mac (quote-syntax #,stx) #,(make-μ 'μ #'options+clause))]))

(define-syntax (mu stx)
  (syntax-case stx ()
    [(_ . options+clause)
     #`(mac (quote-syntax #,stx) #,(make-μ 'mu #'options+clause))]))

(define-syntax (macro stx)
  (syntax-case stx ()
    [(_ . options+clauses)
     #`(mac (quote-syntax #,stx) #,(make-macro 'macro #'options+clauses))]))

(define-syntax (μ* stx)
  (syntax-case stx ()
    [(_ . options+clause)
     #`(mac (quote-syntax #,stx) #,(make-μ* 'μ* #'options+clause))]))

(define-syntax (mu* stx)
  (syntax-case stx ()
    [(_ . options+clause)
     #`(mac (quote-syntax #,stx) #,(make-μ* 'mu* #'options+clause))]))

(define-syntax (macro* stx)
  (syntax-case stx ()
    [(_ . options+clauses)
     #`(mac (quote-syntax #,stx) #,(make-macro* 'macro* #'options+clauses))]))

(define-syntax-rule (var id)
  (syntax-local-eval #'id))
