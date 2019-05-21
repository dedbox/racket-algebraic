#lang algebraic/racket/base

(require algebraic/private
         racket/pretty
         racket/stxparam
         racket/stxparam-exptime
         (for-syntax (except-in algebraic/racket/base id)
                     algebraic/syntax/list
                     racket/syntax
                     syntax/parse)
         (for-meta 2 algebraic/syntax/list
                   syntax/parse))

(provide (all-defined-out)
         (for-syntax (all-defined-out)))

;;; ----------------------------------------------------------------------------
;;; Class

(define-syntax class
  (μ* (class-id:id [member-id:id . maybe-def] ...+)
    #:as stmt
    #:if (syntax-andmap (|| syntax-null? (.. syntax-null? syntax-cdr))
                        #'(maybe-def ...))
    #:with clauses (syntax-cdr #'stmt)
    #:with (def ...) (syntax-map maybe-def->def #'(maybe-def ...))
    (begin
      (define-syntax class-id (class-transformer #'class-id #'clauses))
      (define-syntax-parameter member-id def) ...)))

(begin-for-syntax

  (define (maybe-def->def maybe-def)
    (with-syntax ([(stx* id) (generate-temporaries (list maybe-def maybe-def))])
      (if (syntax-null? maybe-def)
          #'(λ (stx*)
              (raise-syntax-error
               #f (format "no instance for ~a" 'class-id) stx*))
          #`(λ (stx*)
              (...
               (syntax-case stx* ()
                 [id (identifier? #'id) #'#,(syntax-car maybe-def)]
                 [(id expr ...) (identifier? #'id)
                                (syntax-cons #'#,(syntax-car maybe-def)
                                             #'(expr ...))]))))))

  (struct class-transformer (id clauses)
    #:transparent
    #:property prop:procedure
    (λ (C stx)
      (with-syntax ([name (class-transformer-id C)]
                    [clauses (class-transformer-clauses C)])
        #'(make-class #'name #'clauses)))))

(struct class (id clauses)
  #:transparent
  #:name algebraic:class
  #:constructor-name make-class
  #:property prop:procedure (λ _ (error "cannot invoke a class"))

  #:methods gen:custom-write

  [(define (write-proc C port mode)
     (case mode
       [(#t #f) (fprintf port "#<class:~a>" (syntax->datum (class-id C)))]
       [else (parameterize ([pretty-print-current-style-table
                             algebraic-pretty-print-style-table])
               ((current-print)
                `(class
                   ,(syntax->datum (class-id C))
                   ,@(syntax->datum (class-clauses C)))))]))])

;;; ----------------------------------------------------------------------------
;;; Instance

(begin-for-syntax

  (define-syntax instance
    (μ* (class-id:id [member-id:id member-def] ...+)
      #:as expr
      #:with clauses (syntax-cdr #'expr)
      (instance-transformer #'class-id #'clauses)))

  (struct instance-transformer (class-id clauses)
    #:transparent
    #:property prop:procedure
    (λ (I stx)
      (syntax-case stx ()
        [x (identifier? #'x)
           (with-syntax ([class-id (instance-transformer-class-id I)]
                         [clauses (instance-transformer-clauses I)])
             #'(make-instance #'class-id #'clauses))]))))

(struct instance (class-id clauses)
  #:transparent
  #:name algebraic:class:instance
  #:constructor-name make-instance
  #:property prop:procedure (λ _ (error "cannot invoke a class instance"))

  #:methods gen:custom-write

  [(define (write-proc I port mode)
     (case mode
       [(#t #f) (fprintf port "#<class:instance-of:~a>"
                         (syntax-e (instance-class-id I)))]
       [else (parameterize ([pretty-print-current-style-table
                             algebraic-pretty-print-style-table])
               ((current-print)
                `(instance ,(syntax->datum (instance-class-id I))
                   ,@(syntax->datum (instance-clauses I)))))]))])

;;; ----------------------------------------------------------------------------
;;; Run-time Instantiation

(begin-for-syntax

  (define (instance-id? stx)
    (instance-transformer? (syntax-local-value stx (λ _ #f))))

  (define (clauses-for instance-id)
    (instance-transformer-clauses (syntax-local-value instance-id)))

  (define implement (.. maybe-def->def syntax-list))

  (define (prefix-with prefix id)
    (format-id (syntax-local-introduce id) "~a~a" prefix id))

  (define (missing-id? member-ids)
    (define ids (syntax-e member-ids))
    (λ (class-member-id)
      (not (member-id? class-member-id ids))))

  (define (instance->class-member-ids instance-id)
    (define I (syntax-local-value instance-id))
    (define class-id (instance-transformer-class-id I))
    (define C (syntax-local-value class-id))
    (syntax-map syntax-car (class-transformer-clauses C)))

  (define (member-id? id ids)
    (member id ids free-identifier=?)))

(define-syntax with-instance
  (macro*
    [(instance-id:id expr ...)
     #:if (instance-id? #'instance-id)
     #:with ([member-id def] ...) (clauses-for #'instance-id)
     #:with (def* ...) (syntax-map implement #'(def ...))
     (syntax-parameterize ([member-id def*] ...) expr ...)]

    [([prefix:id instance-id:id] expr ...)
     #:as stmt
     #:if (instance-id? #'instance-id)
     #:with ([member-id def] ...) (clauses-for #'instance-id)
     #:with (def* ...) (syntax-map implement #'(def ...))
     #:with (def0 ...) (syntax-map syntax-parameter-value #'(member-id ...))
     #:with (prefixed-id ...) (syntax-map (>> prefix-with #'prefix) #'(member-id ...))
     #:with (missing-id ...) (syntax-filter (missing-id? #'(member-id ...))
                                            (instance->class-member-ids #'instance-id))
     #:with (prefixed-missing-id ...) (syntax-map (>> prefix-with #'prefix)
                                                  #'(missing-id ...))
     (syntax-parameterize ([member-id def*] ...)
       (let ([prefixed-id member-id] ...)
         (let ([prefixed-missing-id missing-id] ...)
           (syntax-parameterize ([member-id def0] ...)
             expr ...))))]))

(define-syntax with-instances
  (macro*
    [((spec) expr ...) (with-instance spec expr ...)]
    [((spec specs ...+) expr ...)
     (with-instance spec (with-instances (specs ...) expr ...))]))

;;; ----------------------------------------------------------------------------

(module+ test
  (require rackunit
           syntax/macro-testing)

  (class Eq [==] [/= (.. not ==)])

  (test-case "class"
    (check-pred class? Eq)
    (check-true (phase1-eval (class-transformer? (syntax-local-value #'Eq))))
    (check-exn exn:fail:syntax? (λ () (convert-compile-time-error ==)))
    (check-exn exn:fail:syntax? (λ () (convert-compile-time-error /=))))

  (define-syntax EqEq (instance Eq [== eq?]))
  (define-syntax StringEq (instance Eq [== string=?]))

  (test-case "instance"
    (check-true (phase1-eval (instance-transformer? (syntax-local-value #'EqEq))))
    (check-true (phase1-eval (instance-transformer? (syntax-local-value #'StringEq))))
    (check-exn exn:fail:syntax? (λ () (convert-compile-time-error ==)))
    (check-exn exn:fail:syntax? (λ () (convert-compile-time-error /=))))

  (test-case "with-instance"
    (with-instance EqEq
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

  (test-case "with-instance alias"
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
      (check-false (S:/= "Z" "Z")))))
