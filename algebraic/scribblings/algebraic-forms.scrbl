#lang scribble/manual

@title[#:tag "ref:forms"]{Syntactic Forms}

@require{./algebraic-includes.rkt}

@require[
  @for-label[
    (except-in algebraic/racket/base
               λ lambda case-λ case-lambda let let* letrec let-values let*-values
               letrec-values case define define-values
               #%module-begin)
    algebraic/racket/base/forms
    racket/contract/base
    syntax/transformer
  ]
]

@(define base-forms-eval (algebraic-evaluator))
@(define-syntax example (algebraic-example/locations base-forms-eval))

@(example #:hidden
   (require algebraic/racket/base/forms))

@; #############################################################################

@defmodule[algebraic/racket/base/forms]

This section describes alternative core Racket syntax forms with variable
binding sites generalized to @tech{function} patterns.

@deftogether[(
@defform[(λ formals fun-directive ... body ...+)]
@defform[(lambda formals fun-directive ... body ...+)]
)]{

  Produces a @tech{function}.

  See @racket[φ*] for a description of @var[formals] and @var[fun-directive]s.

  Example:
  @example[
    (data XYZ (X Y Z))
    ((λ ((X x) (Y y)) (+ x y))
     (X 1)
     (Y 2))
  ]
}

@deftogether[(
@defform[(case-λ [formals fun-directive ... body ...+] ...)]
@defform[(case-lambda [formals fun-directive ... body ...+] ...)]
)]{

  Produces a @tech{function}.

  See @racket[φ*] for a description of @var[formals] and @var[fun-directive]s.

  Example:
  @example[
    (let ([f (case-λ
               [() 10]
               [((X x)) x]
               [((X x) (Y y)) (list y x)]
               [r r])])
      (list (f)
            (f (X 1))
            (f (X 1) (Y 2))
            (f 1 2 3)))
  ]
}

@defform*[((let ([patt val-expr] ...) body ...+)
           (let proc-id ([patt init-expr] ...) body ...+))]{

  The first form evaluates the @var[val-expr]s left-to-right and then matches
  the values against the @var[patt]s. It then evaluates the @var[body]s, in
  which the variables of the @var[patt]s are bound. The last @var[body]
  expression is in tail position with respect to the @racket[let] form.

  Examples:
  @example[
    (let ([(X x) (X 5)]) x)
    (let ([(X x) (X 5)])
      (let ([(X x) (X 2)]
            [(Y y) (Y x)])
        (list y x)))
  ]

  The second form evaluates the @var[init-expr]s; the resulting values become
  arguments in an application of a @tech{function} @racket[(λ (patt ...) body
  ...)], where @var[proc-id] is bound within the @var[body]s to the
  @tech{function} itself.

  Example:
  @example[
    (let fac ([(X n) (X 10)])
      (if (zero? n)
          1
          (* n (fac (X (sub1 n))))))
  ]
}

@defform[(let* ([patt val-expr] ...) body ...+)]{

  Like @racket[let], but evaluates the @var[val-expr]s one by one, matching
  each value against the corresponding @var[patt] as soon as the value is
  available. The varibles of @var[patt] are bound in the remaining
  @var[val-expr]s as well as the @var[body]s.

  Example:
  @example[
    (let* ([(X x) (X 1)]
           [(Y y) (Y (+ x 1))])
      (list y x))
  ]
}

@defform[(letrec ([patt val-expr] ...) body ...+)]{

  Like @racket[let], including left-to-right evaluation of the
  @var[val-expr]s, but the @rtech{locations} for all pattern variables are
  created first, all pattern variables are bound in all @var[val-expr]s as
  well as the @var[body]s, and each pattern variable is initialized
  immediately after the corresponding @var[val-expr] is evaluated.

  Example:
  @example[
    (letrec ([(X is-even?)
              (X (φ n ((|| zero? (.. is-odd? sub1)) n)))]
             [(Y is-odd?)
              (Y (φ n ((&& (.. not zero?) (.. is-even? sub1)) n)))])
      (is-odd? 11))
  ]
}

@defform[(let-values ([(patt ...) val-expr] ...) body ...+)]{

  Like @racket[let], except that each @var[val-expr] must produce as many
  values as corresponding @var[patt]s, otherwise the
  @racket[exn:fail:contract] exception is raised.

  Example:
  @example[
    (let-values ([((X x) (Y y)) (id (X (quotient 10 3))
                                    (Y (remainder 10 3)))])
      (list y x))
  ]
}

@defform[(let*-values ([(patt ...) val-expr] ...) body ...+)]{

  Like @racket[let*], except that each @var[val-expr] must produce as many
  values as corresponding @var[patt]s.

  Example:
  @example[
    (let*-values ([((X x) (Y y)) (id (X (quotient 10 3))
                                     (Y (remainder 10 3)))]
                  [((Z z)) (Z (list y x))])
      z)
  ]
}

@defform[(letrec-values ([(patt ...) val-expr] ...) body ...+)]{

  Like @racket[letrec], except that each @var[val-expr] must produce as many
  values as corresponding @var[patt]s.

  Example:
  @example[
    (letrec-values
        ([((X is-even?) (Y is-odd?))
          (id (X (φ n ((|| zero? (.. is-odd? sub1)) n)))
              (Y (φ n ((|| (<< = 1) (.. is-even? sub1)) n))))])
      (is-odd? 11))
  ]
}

@defform[
  (case val-expr case-clause ...)
  #:grammar [(case-clause [patt fun-directive ... then-body ...+]
                          [else then-body ...+])]
]{

  Evaluates @var[val-expr] and uses the result to select a @var[case-clause].
  The selected clause is the first one whose @var[patt] matches the result of
  @var[val-expr]. If no such @var[patt] is present, the @racket[else] clause
  is selected; if no @racket[else] @var[case-clause] is present, either, then
  the result of the @racket[case] form is @list[void-const].

  For the selected @var[case-clause], the results of the last @var[then-body],
  which is in tail position with respect to the @racket[case] form, are the
  results for the whole @racket[case] form.

  A @var[case-clause] that starts with @racket[else] must be the last
  @var[case-clause].

  Examples:
  @example[
    (case (+ 7 5)
      [n #:if ((member-of 1 2 3) n) 'small]
      [n #:if ((member-of 10 11 12) n) 'big])
    (case (- 7 5)
      [n #:if ((member-of 1 2 3) n) 'small]
      [n #:if ((member-of 10 11 12) n) 'big])
    (case (string-append "do" "g")
      [s #:if ((member-of "cat" "dog" "mouse") s) "animal"]
      [else "mineral or vegetable"])
    (case (list 'y 'x)
      [ℓ #:if ((member-of '(a b) '(x y)) ℓ) 'forwards]
      [ℓ #:if ((member-of '(b a) '(y x)) ℓ) 'backwards])
  ]
}

@defform[
  (case-values val-expr case-clause ...)
  #:grammar [(case-clause [(patt ...) fun-directive ... then-body ...+]
                          [else then-body ...+])]
]{

  Multi-valued variant of @racket[case].

  Example:
  @example[
    (case-values (id 1 2 3)
      [(a b c) (+ a b c)]
      [else -1])
  ]
}

@defform*[
  ((define patt expr)
   (define (id patts) body ...+))
  #:grammar [(patts (code:line patt ...)
                    (code:line patt ... #,(racketparenfont ".") rest-patt))]
]{

  The first form matches @var[patt] against the result of @var[expr] and, if
  the match succeeds, @rtech{binds} the variables of @var[patt] to the
  corresponding parts of the result of @var[expr].

  The second form @rtech{binds} @var[id] to a @tech{function}.

  Examples:
  @example[
    (define (X x) (X 10))
    x
  ]

  @example[
    (define (f (X x))
      (+ x 1))
    (f (X 10))
  ]
}
