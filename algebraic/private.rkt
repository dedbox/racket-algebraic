#lang racket/base

(require racket/contract/base
         racket/set
         racket/struct-info
         racket/syntax
         (for-template algebraic/product
                       racket/base))

(provide
 (contract-out
  ;; Syntax List Predicate
  [quote-pattern? predicate/c]
  [quasiquote-pattern? predicate/c]
  [unquote-pattern? predicate/c]
  [product-instance-pattern? predicate/c]
  [regexp-pattern? predicate/c]
  [condition-pattern? predicate/c]
  [alias-pattern? predicate/c]
  ;; Keyword Parsers
  [aliases (-> (or/c syntax? (listof syntax?)) (listof syntax?))]
  [conditions (-> (or/c syntax? (listof syntax?)) (listof syntax?))]
  [premises (-> (or/c syntax? (listof syntax?)) (listof syntax?))]
  [consequents (-> (or/c syntax? (listof syntax?)) (listof syntax?))]
  [blocks (-> (or/c syntax? (listof syntax?)) (listof syntax?))]
  [body (-> (or/c syntax? (listof syntax?)) (listof syntax?))]
  ;; Predicates
  [literal-data? predicate/c]
  [wildcard? predicate/c]
  [variable? predicate/c]
  [product-identifier? predicate/c]
  [struct-identifier? predicate/c]
  [keyword-syntax? predicate/c]
  ;; Helpers
  [keyword-syntax->string (-> any/c string?)]
  [keyword-syntax=? (-> syntax? string? boolean?)]
  [maybe-quote/ids (-> any/c any/c)]
  [struct-field-names (-> identifier? (listof symbol?))]
  [pattern-variables (-> syntax? (listof identifier?))]))

;;; Syntax Lists

(define (quote-pattern? arg*)
  (syntax-case #`#,arg* (quote)
    [(quote _) #t]
    [_ #f]))

(define (quasiquote-pattern? arg*)
  (syntax-case #`#,arg* (quasiquote)
    [(quasiquote _) #t]
    [_ #f]))

(define (unquote-pattern? arg*)
  (syntax-case #`#,arg* (unquote)
    [(unquote _) #t]
    [_ #f]))

(define (product-instance-pattern? arg*)
  (and (pair? arg*) (product-identifier? (car arg*))))

(define (regexp-pattern? arg*)
  (and (pair? arg*) (regexp? (syntax-e (car arg*)))))

(define (condition-pattern? arg*)
  (and (list? arg*)
       (pair? arg*)
       (pair? (cdr arg*))
       (pair? (cddr arg*))
       (null? (cdddr arg*))
       (keyword-syntax=? (cadr arg*) "if")))

(define (alias-pattern? arg*)
  (and (list? arg*)
       (pair? arg*)
       (pair? (cdr arg*))
       (pair? (cddr arg*))
       (null? (cdddr arg*))
       (keyword-syntax=? (cadr arg*) "as")))

;;; Keyword Parsers

(define ((keyword name head tail) clause*)
  (let ([next (next-keyword name clause*)])
    (if (and (pair? next) (keyword-syntax=? (car next) name))
        (cons (head next) ((keyword name head tail) (tail next)))
        null)))

(define aliases (keyword "as" cadr cddr))
(define conditions (keyword "if" cadr cddr))
(define consequents (keyword "with" cadr cdddr))
(define premises (keyword "with" caddr cdddr))
(define blocks (keyword "do" cadr cddr))

(define (body clause*)
  (next-keyword "" clause*))

(define (next-keyword name clause*)
  (cond [(keyword-syntax=? (car clause*) name) clause*]
        [(keyword-syntax? (car clause*))
         (case (keyword-syntax->string (car clause*))
           [("do" "as" "if") (next-keyword name (cddr clause*))]
           [("with") (next-keyword name (cdddr clause*))]
           [else (raise-syntax-error #f "unexpected keyword" (car clause*))])]
        [else clause*]))

;;; Predicates

(define (literal-data? stx)
  (let ([val (syntax-e stx)])
    (or (boolean? val)
        (char? val)
        (number? val)
        (string? val)
        (bytes? val))))

(define (wildcard? stx)
  (and (identifier? stx)
       (not (eq? (syntax->datum stx) '||))
       (char=? (first-char stx) #\_)))

(define (first-char stx)
  (string-ref (symbol->string (syntax->datum stx)) 0))

(define (variable? stx)
  (and (identifier? stx)
       (not (or (wildcard? stx)
                (product-identifier? stx)
                (struct-identifier? stx)))))

(define (product-identifier? stx)
  (and (identifier? stx)
       (identifier-binding stx)
       (product-transformer? (syntax-local-value stx (λ _ #f)))))

(define (struct-identifier? stx)
  (and (identifier? stx)
       (or (identifier-binding (format-id stx "struct:~a" stx))
           (identifier-template-binding (format-id stx "struct:~a" stx)))
       #t))

(define (keyword-syntax? stx)
  (keyword? (syntax->datum stx)))

;;; Helpers

(define (keyword-syntax->string stx)
  (and (keyword-syntax? stx)
       (keyword->string (syntax->datum stx))))

(define (keyword-syntax=? kw name)
  (equal? (keyword-syntax->string kw) name))

(define (maybe-quote/ids stx)
  (let ([stx* (syntax-e stx)])
    (cond [(and (list? stx*)
                (pair? stx*)
                (pair? (cdr stx*))
                (null? (cddr stx*))
                (free-identifier=? (car stx*) #'quote)) stx]
          [(list? stx*) (with-syntax ([(a ...) stx*]) #'(quote (a ...)))]
          [else stx])))

(define (struct-field-names struct-id)
  (define rx
    (regexp
     (string-append "^" (symbol->string (syntax->datum struct-id)) "-(.+)$")))
  (define (field-name id)
    (string->symbol
     (cadr (regexp-match rx (symbol->string (syntax->datum id))))))
  (map field-name
       (cadddr (extract-struct-info (syntax-local-value struct-id)))))

(define (pattern-variables stx)
  (define stx* (syntax-e stx))
  (define vars
    (syntax-case stx (quote quasiquote void)
      [_ (literal-data? stx) null]
      [(quote _) null]

      [(quasiquote datum)
       (let quasi ([stx** #'datum])
         (if (identifier? #'stx**)
             null
             (syntax-case #'stx** ()
               [(unquote patt) (pattern-variables #'patt)]
               [(patt1 . patt2) (append (quasi #'patt1) (quasi #'patt2))]
               [() null]
               [_ null])))]

      [_ (wildcard? stx) null]
      [_ (variable? stx) (list stx)]

      [_ (vector? stx*)
         (apply append (map pattern-variables (vector->list stx*)))]
      [_ (box? stx*) (pattern-variables (unbox stx*))]
      [_ (hash? stx*)
         (apply append (map pattern-variables (hash-values stx*)))]

      [Π (product-identifier? #'Π) null]
      [(Π x ...) (product-identifier? #'Π)
                 (apply append (map pattern-variables (syntax-e #'(x ...))))]

      [(patt1 #:as patt2) (append (pattern-variables #'patt1)
                                  (pattern-variables #'patt2))]

      [(patt #:if _) (pattern-variables #'patt)]

      [_ (regexp? stx*) null]
      [(rx x ...) (regexp? (syntax-e #'rx))
                  (apply append (map pattern-variables (syntax-e #'(x ...))))]

      [(S [field-id x] ...)
       (and (struct-identifier? #'S)
            (member (syntax->datum (car (syntax-e #'(field-id ...))))
                    (struct-field-names #'S)))
       (apply append (map pattern-variables (syntax-e #'(x ...))))]

      [(S x ...) (struct-identifier? #'S)
                 (apply append (map pattern-variables (syntax-e #'(x ...))))]

      [(void) null]

      [(_ ...) (apply append (map pattern-variables stx*))]
      [(_ . _) (append (pattern-variables (car stx*))
                       (pattern-variables (cdr stx*)))]

      [_ (raise-syntax-error #f "invalid pattern" stx)]))
  (set->list (apply set vars)))
