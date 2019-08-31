#lang racket/base

(require algebraic/pretty
         algebraic/private
         racket/contract/base
         racket/pretty
         racket/syntax
         syntax/parse
         syntax/transformer
         (for-template racket/base)
         (for-syntax algebraic/product
                     algebraic/private
                     algebraic/syntax
                     racket/base
                     syntax/parse
                     syntax/strip-context
                     syntax/transformer)
         (for-meta 2 racket/base))

(provide
 μ0 mu0 μ mu macro μ* mu* macro* μ-parser mu-parser μ*-parser mu*-parser
 macro-parser macro*-parser var ...+
 make-variable-like-transformer
 (contract-out
  [macro? predicate/c]))

(struct mac (def impl)
  #:reflection-name 'macro
  #:property prop:procedure (λ (M . args) (apply (mac-impl M) args))
  #:methods gen:custom-write
  [(define (write-proc M port mode)
     (case mode
       [(#t #f) (fprintf port "#<macro>")]
       [else (parameterize ([pretty-print-current-style-table
                             algebraic-pretty-print-style-table])
               (if (pretty-printing)
                   (pretty-print (syntax->datum (mac-def M)) port 1)
                   (print (syntax->datum (mac-def M)) port 1)))]))])

(define macro? mac?)

(begin-for-syntax
  (define (make-μ options+clause)
    (let ([options+clause* (syntax-e options+clause)])
      (let ([options (skip-clauses options+clause*)]
            [clause (datum->syntax options+clause (skip-options options+clause*))])
        #`(syntax-parser
            #,@options
            #,((make-clause (λ (a) #`(_ #,(argument a))) quasi-singleton)
               clause)))))

  (define (make-μ-parser options+clause)
    (let ([options+clause* (syntax-e options+clause)])
      (let ([options (skip-clauses options+clause*)]
            [clause (datum->syntax options+clause (skip-options options+clause*))])
        #`(syntax-parser
            #,@options
            #,((make-clause argument singleton) clause)))))

  (define (make-μ* options+clause)
    (let ([options+clause* (syntax-e options+clause)])
      (let ([options (skip-clauses options+clause*)]
            [clause (datum->syntax options+clause (skip-options options+clause*))])
        (with-syntax ([clause ((make-clause formals quasi-singleton) clause)])
          #`(syntax-parser #,@options clause)))))

  (define (make-μ*-parser options+clause)
    (let ([options+clause* (syntax-e options+clause)])
      (let ([options (skip-clauses options+clause*)]
            [clause (datum->syntax options+clause (skip-options options+clause*))])
        (with-syntax ([clause ((make-clause parser-formals singleton) clause)])
          #`(syntax-parser #,@options clause)))))

  (define (make-macro options+clauses)
    (let ([options+clauses* (syntax-e options+clauses)])
      (let ([options (skip-clauses options+clauses*)]
            [clauses (skip-options options+clauses*)])
        #`(syntax-parser
            #,@options
            #,@(map (make-clause (λ (a) #`(_ #,(argument a))) quasi-singleton)
                    clauses)))))

  (define (make-macro-parser options+clauses)
    (let ([options+clauses* (syntax-e options+clauses)])
      (let ([options (skip-clauses options+clauses*)]
            [clauses (skip-options options+clauses*)])
        #`(syntax-parser
            #,@options
            #,@(map (make-clause argument singleton) clauses)))))

  (define (make-macro* options+clauses)
    (let ([options+clauses* (syntax-e options+clauses)])
      (let ([options (skip-clauses options+clauses*)]
            [clauses (skip-options options+clauses*)])
        (with-syntax ([(clause ...)
                       (map (make-clause formals quasi-singleton) clauses)])
          #`(syntax-parser #,@options clause ...)))))

  (define (make-macro*-parser options+clauses)
    (let ([options+clauses* (syntax-e options+clauses)])
      (let ([options (skip-clauses options+clauses*)]
            [clauses (skip-options options+clauses*)])
        (with-syntax ([(clause ...)
                       (map (make-clause parser-formals singleton) clauses)])
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

  (define ((make-clause patt-maker body-maker) clause)
    (let* ([clause* (syntax-e clause)]
           [tail (cdr clause*)])
      (with-syntax ([patt (patt-maker (car clause*))]
                    [(alias-patt ...) (map patt-maker (aliases tail))])
        #`[(~and patt alias-patt ...)
           #,@(make-directives tail) #,(body-maker (body tail))])))

  (define (make-directives tail)
    (cond [(or (null? tail) (not (keyword-syntax? (car tail)))) null]
          [(keyword-syntax=? (car tail) "do") (do-directive (cdr tail))]
          [(keyword-syntax=? (car tail) "if") (if-directive (cdr tail))]
          [(keyword-syntax=? (car tail) "with") (with-directive (cdr tail))]
          [else null]))

  (define (do-directive tail)
    (with-syntax ([block (car tail)])
      (list* #'#:do #'block (make-directives (cdr tail)))))

  (define (if-directive tail)
    (with-syntax ([patt (car tail)])
      (list* #'#:post
             #'(~fail #:unless patt (format "condition failed: ~a" 'patt))
             (make-directives (cdr tail)))))

  (define (with-directive tail)
    (with-syntax ([patt (argument (car tail))]
                  [expr (cadr tail)])
      (list* #'#:with #'patt #'expr (make-directives (cddr tail)))))

  (define (quasi-singleton xs)
    (datum->syntax
     (car xs)
     (list #'quasisyntax
           (if (null? (cdr xs))
               (car xs)
               (datum->syntax (car xs) (list* #'begin xs))))))

  (define (singleton xs)
    (datum->syntax
     (car xs)
     (if (null? (cdr xs))
         (car xs)
         (datum->syntax (car xs) (list* #'begin xs)))))

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

  (define (parser-formals args)
    (syntax-case args ()
      [id (wildcard? #'id) #'_]
      [id (variable? #'id) #'(_ . id)]
      [(_ ...) (map argument (syntax-e args))]
      [(head ... . tail)
       (with-syntax ([(patt ...) (map argument (syntax-e #'(head ...)))]
                     [rest-patt (argument #'tail)])
         #'(patt ... . rest-patt))]
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

           ;; keywords
           [_ (keyword? arg*) arg]

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

;;; ----------------------------------------------------------------------------
;;; Macro Expressions

(define-syntax (μ0 stx)
  (syntax-case stx ()
    [(_ expr) (syntax/loc stx
                (...
                 (syntax-parser
                   [:id #`expr]
                   [(:id x ...) #`((#%expression expr) x ...)])))]
    [(_ expr exprs ...)
     (#%rewrite stx `(μ0 (begin . ,#'(expr exprs ...))))]))

(define-syntax (mu0 stx)
  (syntax-case stx ()
    [(_ expr exprs ...) (#%rewrite stx `(μ0 . ,#'(expr exprs ...)))]))

;;; ----------------------------------------------------------------------------
;;; Uni-Clausal Macros

;;; Univariate

(define-syntax (μ stx)
  (syntax-case stx ()
    [(_ . options+clause)
     #`(mac (quote-syntax #,stx #:local) #,(make-μ #'options+clause))]))

(define-syntax (mu stx)
  (syntax-case stx ()
    [(_ . options+clause)
     #`(mac (quote-syntax #,stx #:local) #,(make-μ #'options+clause))]))

(define-syntax (μ-parser stx)
  (syntax-case stx ()
    [(_ . options+clause) #`#,(make-μ-parser #'options+clause)]))

(define-syntax (mu-parser stx)
  (syntax-case stx ()
    [(_ . options+clause) #`#,(make-μ-parser #'options+clause)]))

;;; Multivariate

(define-syntax (μ* stx)
  (syntax-case stx ()
    [(_ . options+clause)
     #`(mac (quote-syntax #,stx #:local) #,(make-μ* #'options+clause))]))

(define-syntax (mu* stx)
  (syntax-case stx ()
    [(_ . options+clause)
     #`(mac (quote-syntax #,stx #:local) #,(make-μ* #'options+clause))]))

(define-syntax (μ*-parser stx)
  (syntax-case stx ()
    [(_ . options+clause) #`#,(make-μ*-parser #'options+clause)]))

(define-syntax (mu*-parser stx)
  (syntax-case stx ()
    [(_ . options+clause) #`#,(make-μ*-parser #'options+clause)]))

;;; ----------------------------------------------------------------------------
;;; Multi-clausal Macros

;;; Univariate

(define-syntax (macro stx)
  (syntax-case stx ()
    [(_ . options+clauses)
     #`(mac (quote-syntax #,stx #:local) #,(make-macro #'options+clauses))]))

(define-syntax (macro-parser stx)
  (syntax-case stx ()
    [(_ . options+clauses) #`#,(make-macro-parser #'options+clauses)]))

;;; Multivariate

(define-syntax (macro* stx)
  (syntax-case stx ()
    [(_ . options+clauses)
     #`(mac (quote-syntax #,stx #:local) #,(make-macro* #'options+clauses))]))

(define-syntax (macro*-parser stx)
  (syntax-case stx ()
    [(_ . options+clauses) #`#,(make-macro*-parser #'options+clauses)]))

;;; ----------------------------------------------------------------------------
;;; Helpers

(define-syntax-rule (var id)
  (syntax-local-eval #'id))
