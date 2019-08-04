#lang racket/base

(require algebraic/prelude
         algebraic/pretty
         algebraic/syntax-list
         racket/pretty
         racket/splicing
         racket/stxparam
         (for-syntax algebraic/function
                     algebraic/macro
                     (except-in algebraic/prelude id)
                     algebraic/syntax
                     algebraic/syntax-list
                     racket/provide-transform
                     racket/base
                     racket/syntax
                     syntax/id-set
                     syntax/id-table
                     syntax/parse
                     syntax/strip-context)
         (for-meta 2 algebraic/macro
                   algebraic/syntax
                   racket/base
                   syntax/parse))

(provide class? class with-instance with-instances splicing-with-instance
         splicing-with-instances instantiate instantiate-out
         define-class-helper
         (for-syntax class-id? class-helper instance instance-id?))

;;; ----------------------------------------------------------------------------
;;; Class

(define-syntax class
  (macro*
    #:datum-literals (minimal)
    [(class-id:id [member-id:id . maybe-def] ... minimal ([min-id:id ...+] ...))
     #:if (syntax-andmap null-or-singleton? #'(maybe-def ...))
     #:with (stx ...) (generate-temporaries #'(member-id ...))
     (begin
       (define-syntax class-id
         (class-transformer #'class-id
                            #'(member-id ...)
                            #'(maybe-def ...)
                            #'([min-id ...] ...)))
       (define-syntax-parameter member-id
         (λ (stx)
           (raise-syntax-error
            #f (format "no instance for ~a" 'class-id) stx)))
       ...)]
    [(class-id:id x ...)
     #,(let ([xs (syntax-e #'(x ...))])
         (#%rewrite this-syntax `(class ,#'class-id ,@xs minimal ())))]))

(define-for-syntax null-or-singleton?
  (|| syntax-null? (.. syntax-null? syntax-cdr)))

(begin-for-syntax
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
       [else (let ([v `(class ,(syntax->datum (class-id C))
                         ,@(map ::
                                (syntax->datum (class-member-ids C))
                                (syntax->datum (class-maybe-defs C)))
                         ,@(if (syntax-null? (class-min-ids C))
                               null
                               `(minimal ,(syntax->datum (class-min-ids C)))))])
               (if (pretty-printing)
                   (parameterize ([pretty-print-current-style-table
                                   algebraic-pretty-print-style-table])
                     (pretty-print v port 1))
                   (print v port 1)))]))])

(define-for-syntax (class-id? stx)
  (and (identifier? stx)
       (class-transformer? (syntax-local-value stx (λ _ #f)))))

;;; ----------------------------------------------------------------------------
;;; Class Helper

(begin-for-syntax
  (define-syntax class-helper
    (μ expr
      (...
       (λ (stx)
         (syntax-parse stx
           [:id (#%rewrite stx 'expr)]
           [(f:id x ...)
            (#%rewrite stx `((#%expression ,#'f) . ,#'(x ...)))]))))))

(define-syntax define-class-helper
  (macro*
    [(a:id def) (define-syntax a (class-helper def))]
    [((f:id x ...) body ...+)
     (define-syntax f (class-helper (λ (x ...) body ...)))]))

;;; ----------------------------------------------------------------------------
;;; Instance

(begin-for-syntax
  (define-syntax instance
    (macro*
      #:datum-literals (extends)
      [(class-id:id extends (instance-id:id ...) [member-id:id member-def] ...)
       (begin
         (unless (class-id? #'class-id)
           (raise-syntax-error #f "Not a class" #'class-id))
         (let ([C (syntax-local-value #'class-id)])
           (unless (minimal-instance? (class-transformer-min-ids C)
                                      #'(member-id ...)
                                      #'(instance-id ...))
             (raise-syntax-error #f "Not a minimal definition" #'#,this-syntax))
           (with-syntax ([([id def] (... ...))
                          (instance-members #'class-id
                                            #'(member-id ...)
                                            #'(member-def ...)
                                            #'(instance-id ...))])
             (instance-transformer #'class-id
                                   #'(member-id ...)
                                   #'(member-def ...)
                                   #'(instance-id ...)
                                   #'(id (... ...))
                                   #'(def (... ...))))))]
      [(class-id:id [member-id:id member-def] ...)
       #,(#%rewrite this-syntax
           `(instance ,#'class-id extends ()
                      . ,#'([member-id member-def] ...)))])))

(define-for-syntax (minimal-instance? min-ids member-ids instance-ids)
  (define ext-members (extended-members (syntax-e instance-ids)))
  (or (syntax-null? min-ids)
      (syntax-ormap (has-min-ids? (++ (map car ext-members)
                                      (syntax-e member-ids)))
                    min-ids)))

(define-for-syntax ((has-min-ids? member-ids) ids)
  (syntax-andmap (<< member member-ids free-identifier=?) ids))

(define-for-syntax (maybe-implement class-id maybe-def)
  (and (not (syntax-null? maybe-def)) (syntax-car maybe-def)))

(define-for-syntax ((no-instance? I-ids) C-member)
  (not (member (car C-member) I-ids free-identifier=?)))

(define-for-syntax (instance-members class-id
                                     member-ids
                                     member-defs
                                     instance-ids)
  (let* ([I-ids (syntax-e member-ids)]
         [I-defs (syntax-e member-defs)]
         [I-members (map list I-ids I-defs)]
         [C (syntax-local-value class-id)]
         [C-ids (syntax-e (class-transformer-member-ids C))]
         [C-defs (map (>> maybe-implement class-id)
                      (syntax-e (class-transformer-maybe-defs C)))]
         [E-members (filter (no-instance? I-ids)
                            (extended-members (syntax-e instance-ids)))]
         [C-members (filter
                     (&& cdr (no-instance? (++ I-ids (map car E-members))))
                     (map list C-ids C-defs))])
    (++ I-members C-members E-members)))

(define-for-syntax (extended-members base-ids)
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
          ($ ++ (map (φ base-id
                       (let ([I* (syntax-local-value base-id)])
                         (instance-members (instance-transformer-class-id I*)
                                           (instance-transformer-member-ids I*)
                                           (instance-transformer-member-defs I*)
                                           (instance-transformer-base-ids I*))))
                     base-instance-ids)))))
  (free-id-table-map base-members list))

(begin-for-syntax
  (struct instance-transformer
    (class-id member-ids member-defs base-ids ids defs)
    #:transparent
    #:property prop:procedure
    (λ (I stx)
      (syntax-case stx ()
        [x (identifier? #'x)
           #`(make-instance #'#,(instance-transformer-class-id I)
                            #'#,(instance-transformer-member-ids I)
                            #'#,(instance-transformer-member-defs I)
                            #'#,(instance-transformer-base-ids I))]))))

(struct instance (class-id member-ids member-defs base-ids)
  #:transparent
  #:name algebraic:class:instance
  #:constructor-name make-instance
  #:methods gen:custom-write
  [(define (write-proc I port mode)
     (case mode
       [(#t #f) (fprintf port "#<class-instance ~a>"
                         (syntax-e (instance-class-id I)))]
       [else (let ([v `(instance ,(syntax->datum (instance-class-id I))
                         ,@(if (null? (syntax-e (instance-base-ids I)))
                               null
                               `(extends
                                 ,(map syntax->datum
                                       (syntax-e (instance-base-ids I)))))
                         ,@(map list
                                (syntax->datum (instance-member-ids I))
                                (syntax->datum (instance-member-defs I))))])
               (if (pretty-printing)
                   (parameterize ([pretty-print-current-style-table
                                   algebraic-pretty-print-style-table])
                     (pretty-print v port 1))
                   (print v port 1)))]))])

(define-for-syntax (instance-id? stx)
  (and (identifier? stx)
       (instance-transformer? (syntax-local-value stx (λ _ #f)))))

;;; ----------------------------------------------------------------------------
;;; Run-time Instantiation

(define-syntax with-instance
  (macro*
    [(instance-id:id body ...+)
     #,(#%rewrite this-syntax
         `(with-instance [|| ,#'instance-id] . ,#'(body ...)))]
    [([prefix:id instance-id:id] body ...+)
     #:if (instance-id? #'instance-id)
     #:do [(define I (syntax-local-value #'instance-id))]
     #:with (id ...) (instance-transformer-ids I)
     #:with (def ...) (instance-transformer-defs I)
     #:with (id/prefix ...) (syntax-map (prepend #'prefix #'prefix) #'(id ...))
     (let-values ([(id/prefix ...) (syntax-parameterize ([id (μ0 def)] ...)
                                     (values id ...))])
       body ...)]))

(define-syntax with-instances
  (macro*
    [((spec) body ...+)
     #,(#%rewrite this-syntax `(with-instance ,#'spec . ,#'(body ...)))]
    [((spec0 spec ...+) body ...+)
     #,(#%rewrite this-syntax
         `(with-instance ,#'spec0
            (with-instances ,#'(spec ...) . ,#'(body ...))))]))

(define-syntax splicing-with-instance
  (macro*
    [(instance-id:id body ...+)
     #,(#%rewrite #'instance-id
         `(splicing-with-instance [|| ,#'instance-id] . ,#'(body ...)))]
    [([prefix:id instance-id:id] body ...+)
     #:if (instance-id? #'instance-id)
     #:do [(define I (syntax-local-value #'instance-id))]
     #:with (id ...) (instance-transformer-ids I)
     #:with (def ...) (instance-transformer-defs I)
     #:with (id/prefix ...) (syntax-map (prepend #'prefix #'prefix) #'(id ...))
     (splicing-let-values ([(id/prefix ...)
                            (syntax-parameterize ([id (μ0 def)] ...)
                              (values id ...))])
       body ...)]))

(define-syntax splicing-with-instances
  (macro*
    [((spec) body ...+)
     #,(#%rewrite this-syntax `(splicing-with-instance ,#'spec . #'(body ...)))]
    [((spec0 spec ...+) body ...+)
     #,(#%rewrite this-syntax
         `(splicing-with-instance ,#'spec0
            (splicing-with-instances ,#'(spec ...) . ,#'(body ...))))]))

(define-syntax instantiate
  (macro*
    [(instance-id:id)
     #:if (eq? (syntax-local-context) 'top-level)
     #,(#%rewrite this-syntax `(instantiate || ,#'instance-id))]
    [(prefix:id instance-id:id)
     #:if (eq? (syntax-local-context) 'top-level)
     #:if (instance-id? #'instance-id)
     #:do [(define I (syntax-local-value #'instance-id))]
     #:with (id ...) (instance-transformer-ids I)
     #:with (def ...) (instance-transformer-ids I)
     #:with (id/prefix ...) (syntax-map (prepend this-syntax #'prefix) #'(id ...))
     (define-values (id/prefix ...)
       (with-instance instance-id (values id ...)))]
    [_ #,(raise-syntax-error
          #f (format "Not in a top-level context (~a)" (syntax-local-context))
          this-syntax)]))

(define-for-syntax ((prepend ctx prefix) id)
  (format-id ctx "~a~a" (syntax->datum prefix) (syntax->datum id)))

;;; ----------------------------------------------------------------------------
;;; Exporting

(define-syntax instantiate-out
  (make-provide-pre-transformer
   (λ (stx modes)
     (syntax-parse stx
       [(_ prefix:id instance-id:id)
        #:do [(syntax-local-lift-module-end-declaration
               #'(provide-instantiate prefix instance-id))]
        #'(combine-out)]
       [(_ instance-id:id)
        #:do [(syntax-local-lift-module-end-declaration
               #'(provide-instantiate instance-id))]
        #'(combine-out)]))))

(define-syntax provide-instantiate
  (macro*
    [(prefix:id instance-id:id)
     #:if (instance-id? #'instance-id)
     #:do [(define I (syntax-local-value #'instance-id))
           (define ids (syntax-e (instance-transformer-ids I)))]
     #:with (id/prefix ...) (map (prepend #'prefix #'prefix) ids)
     (provide id/prefix ...)]
    [(instance-id:id)
     #:if (instance-id? #'instance-id)
     #:do [(define I (syntax-local-value #'instance-id))]
     #:with (id ...) (instance-transformer-ids I)
     (provide id ...)]))
