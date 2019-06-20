#lang racket/base

(require algebraic/prelude
         algebraic/private
         racket/pretty
         (for-syntax algebraic/macro
                     (except-in algebraic/prelude id)
                     algebraic/syntax-list
                     racket/base
                     racket/list
                     racket/syntax
                     syntax/id-set
                     syntax/id-table
                     syntax/parse
                     syntax/strip-context
                     syntax/transformer)
         (for-meta 2 algebraic/macro
                   racket/base
                   syntax/parse
                   syntax/strip-context))

(provide (all-defined-out)
         (for-syntax (all-defined-out)))

;;; ----------------------------------------------------------------------------
;;; Class

(define-syntax class
  (macro*
    #:datum-literals (minimal)

    [(class-id:id [member-id:id . maybe-def] ... minimal ([min-id:id ...+] ...))
     #:if (syntax-andmap null-or-singleton? #'(maybe-def ...))
     #:with N (datum->syntax this-syntax (syntax-length #'(member-id ...)))
     (define-syntaxes (class-id member-id ...)
       ($ values (class-transformer #'class-id
                                    #'(member-id ...)
                                    #'(maybe-def ...)
                                    #'([min-id ...] ...))
          (make-list N #,(abstract-member (syntax-e #'class-id)))))]

    [(class-id:id x ...)
     #,(replace-context this-syntax #'(class class-id x ... minimal ()))]))

(begin-for-syntax

  (define-syntax class-helper
    (μ expr
      #:with (stx) (generate-temporaries (list this-syntax))
      (...
       (λ (stx)
         (syntax-case stx ()
           [id (identifier? #'id) (replace-context stx #'expr)]
           [(_ x ...) (replace-context stx #'(expr x ...))])))))

  (define null-or-singleton? (|| syntax-null? (.. syntax-null? syntax-cdr)))

  (define (abstract-member class-id)
    (with-syntax ([(stx) (generate-temporaries #'(stx))]
                  [id #`'#,class-id])
      #'(λ (stx) (raise-syntax-error #f (format "no instance for ~a" id) stx))))

  (struct class-transformer (id member-ids maybe-defs min-ids)
    #:transparent
    #:property prop:procedure
    (λ (C stx)
      (syntax-case stx ()
        [id (identifier? #'id)
            #`(make-class #'#,(class-transformer-id C)
                          #'#,(class-transformer-member-ids C)
                          #'#,(class-transformer-maybe-defs C)
                          #'#,(class-transformer-min-ids C))]
        [(_ _ ...) (raise-syntax-error #f "Classes cannot be invoked" stx)]))))

(struct class (id member-ids maybe-defs min-ids)
  #:transparent
  #:name algebraic:class
  #:constructor-name make-class

  #:methods gen:custom-write

  [(define (write-proc C port mode)
     (case mode
       [(#t #f) (fprintf port "#<class:~a>" (syntax->datum (class-id C)))]
       [else (parameterize ([pretty-print-current-style-table
                             algebraic-pretty-print-style-table])
               (print `(class ,(syntax->datum (class-id C))
                         ,@(map ::
                                (syntax->datum (class-member-ids C))
                                (syntax->datum (class-maybe-defs C)))
                         ,@(if (null? (syntax-e (class-min-ids C)))
                               null
                               `(minimal ,(syntax->datum (class-min-ids C)))))
                      port 1))]))])

;;; ----------------------------------------------------------------------------
;;; Instance

(begin-for-syntax

  (define-syntax instance
    (macro*
      #:datum-literals (extends)
      [(class-id:id extends (base-id ...) [member-id:id member-def] ...)
       (let ([C (syntax-local-value #'class-id)])
         (with-syntax ([(E-id (... ...))
                        (map car (extended-members (syntax-e #'(base-id ...))))])
           (unless (or (syntax-null? (class-transformer-min-ids C))
                       (syntax-ormap
                        (has-min-ids? (syntax-append #'(E-id (... ...))
                                                     #'(member-id ...)))
                        (class-transformer-min-ids C)))
             (raise-syntax-error
              #f "does not satisfy a minimal definition" #'#,this-syntax)))
         (instance-transformer #'class-id
                               #'(base-id ...)
                               #'(member-id ...)
                               #'(member-def ...)))]
      [(class-id:id [member-id:id member-def] ...)
       #,(replace-context
          this-syntax
          #'(instance class-id extends () [member-id member-def] ...))]))

  (define ((has-min-ids? member-ids) ids)
    (syntax-andmap (<< member (syntax-e member-ids) free-identifier=?) ids))

  (struct instance-transformer (class-id base-ids member-ids defs)
    #:transparent
    #:property prop:procedure
    (λ (I stx)
      (syntax-case stx ()
        [x (identifier? #'x)
           #`(make-instance #'#,(instance-transformer-class-id I)
                            #'#,(instance-transformer-base-ids I)
                            #'#,(instance-transformer-member-ids I)
                            #'#,(instance-transformer-defs I))]))))

(struct instance (class-id base-ids member-ids defs)
  #:transparent
  #:name algebraic:class:instance
  #:constructor-name make-instance

  #:methods gen:custom-write

  [(define (write-proc I port mode)
     (case mode
       [(#t #f) (fprintf port "#<class-instance ~a>"
                         (syntax-e (instance-class-id I)))]
       [else (parameterize ([pretty-print-current-style-table
                             algebraic-pretty-print-style-table])
               (print
                `(instance ,(syntax->datum (instance-class-id I))
                   ,@(if (null? (syntax-e (instance-base-ids I)))
                         null
                         `(extends ,(map syntax->datum
                                         (syntax-e (instance-base-ids I)))))
                   ,@(map list
                          (syntax->datum (instance-member-ids I))
                          (syntax->datum (instance-defs I))))
                port 1))]))])

;;; ----------------------------------------------------------------------------
;;; Run-time Instantiation

(define-syntax with-instances
  (macro*
    [((spec) expr ...)
     #,(replace-context this-syntax #'(with-instance spec expr ...))]

    [((spec specs ...+) expr ...)
     #,(replace-context
        this-syntax
        #'(with-instance spec (with-instances (specs ...) expr ...)))]))

(define-syntax with-instance
  (macro*
    [(instance-id:id expr ...+)
     #,(replace-context this-syntax #'(with-instance [|| instance-id] expr ...))]
    [([prefix:id instance-id:id] expr ...+)
     #:if (instance-id? #'instance-id)
     #:do [(define members (instance-members #'instance-id))
           (define ids (map car members))
           (define re-context (>> replace-context this-syntax))]
     #:with (id ...) (map re-context ids)
     #:with (id/prefix ...) (map (prepend this-syntax #'prefix) ids)
     #:with (def ...) (map (.. re-context cadr) members)
     (letrec-values
         ([(id/prefix ...)
           (letrec-syntax ([id (μ0 def)] ...)
             (values id ...))])
       expr ...)]))

(define-syntax instantiate
  (macro*
    [(instance-id:id)
     #,(replace-context this-syntax #'(instantiate || instance-id))]
    [(prefix:id instance-id:id)
     #:do [(define ids (map car (instance-members #'instance-id)))]
     #:with (id/prefix ...) (map (prepend this-syntax #'prefix) ids)
     #,(replace-context
        this-syntax
        #'(define-values (id/prefix ...)
            (with-instance [prefix instance-id] (values id/prefix ...))))]))

(begin-for-syntax

  (define (instance-id? stx)
    (instance-transformer? (syntax-local-value stx (λ _ #f))))

  (define (maybe-implement class-id maybe-def)
    (and (not (syntax-null? maybe-def)) (syntax-car maybe-def)))

  (define ((no-instance? I-ids) C-member)
    (not (member (car C-member) I-ids free-identifier=?)))

  (define ((prepend ctx id1) id2)
    (format-id ctx "~a~a" (syntax->datum id1) (syntax->datum id2)))

  (define (instance-members instance-id)
    (let* ([I (syntax-local-value instance-id)]
           [I-ids (syntax-e (instance-transformer-member-ids I))]
           [I-defs (syntax-e (instance-transformer-defs I))]
           [I-members (map list I-ids I-defs)]
           [class-id (instance-transformer-class-id I)]
           [C (syntax-local-value class-id)]
           [C-ids (syntax-e (class-transformer-member-ids C))]
           [C-defs (map (>> maybe-implement class-id)
                        (syntax-e (class-transformer-maybe-defs C)))]
           [E-members (filter (no-instance? I-ids)
                              (extended-members
                               (syntax-e (instance-transformer-base-ids I))))]
           [C-members (filter
                       (&& cdr (no-instance?
                                (++ I-ids (map car E-members))))
                       (map list C-ids C-defs))])
      (++ I-members C-members E-members)))

  (define (extended-members base-ids)
    (define (bases instance-id)
      (let* ([I (syntax-local-value instance-id)]
             [I-base-ids (syntax-e (instance-transformer-base-ids I))])
        (++ I-base-ids ($ ++ (map bases I-base-ids)))))
    (define base-instance-ids
      (free-id-set->list
       (immutable-free-id-set
        (++ base-ids ($ ++ (map bases base-ids))))))
    (define base-members
      (make-immutable-free-id-table
       (map (>> $ ::)
            ($ ++ (map instance-members base-instance-ids)))))
    (free-id-table-map base-members list)))

;;; ----------------------------------------------------------------------------

(module+ test
  (require rackunit
           syntax/macro-testing)

  (class Eq
    [== (.. not /=)]
    [/= (.. not ==)]
    minimal ([==] [/=]))

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

  (instantiate E: EqEq)
  (instantiate S: StringEq)

  (test-case "instantiate"
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
