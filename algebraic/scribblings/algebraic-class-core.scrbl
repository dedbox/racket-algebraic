#lang scribble/manual

@title[#:tag "class:core"]{Defining Classes and Instances}

@require{./algebraic-includes.rkt}

@require[
  @for-label[
    algebraic/racket/base
    racket/contract/base
  ]
]

@define[class-eval (algebraic-evaluator)]
@define-syntax[example (algebraic-example/locations class-eval)]

@; @example[#:hidden
@;   (require algebraic/class)
@; ]

@; #############################################################################

A @deftech{class} is an abstract collection of names with optional default
values.

Algebraic Racket's @tech{class}es are a lot like Haskell's type classes
without the typing constraints. They allow abstract design patterns like
monads with do-notation to be expressed simply and in a generic way that makes
sense for Racket.

The @racket[class] form declares an abstract @tech{class} and its members.

@example[
  (class Eq
    [== (.. not /=)]
    [/= (.. not ==)]
    minimal ([==] [/=]))
]

Using an uninstantiated member is a syntax error.

@example[
  (eval:error ==)
  (eval:error (/= 1 0))
]

The @racket[instance] form produces an object that holds @tech{class} member
definitions.

@example[
  (define-syntax StringEq (instance Eq [== string=?]))
  (define-syntax NumberEq (instance Eq [/= (|| < >)]))
]

Attempting to define an incomplete instance is a syntax error.

@example[
  (eval:error (define-syntax BadEq (instance Eq)))
]

The @racket[with-instance] form temporarily instantiates the members of an
instance.

@example[
  (with-instance StringEq
    (and (== "abcde" "abcde")
         (/= "hello" "world")))
]

@example[
  (with-instance NumberEq
    (and (== 1 1)
         (/= 2 3)))
]

The @racket[with-instance] form can prefix the names it binds so multiple
instances of the same class can be used together.

@example[
  (with-instance [S: StringEq]
    (with-instance [N: NumberEq]
      (and (S:== "abc" "abc")
           (N:== 123 123))))
]

The @racket[with-instances] form offers a cleaner syntax for working with
multiple instances.

@example[
  (define-syntax EqEq (instance Eq [== eq?]))
  (with-instances (EqEq
                   [S: StringEq]
                   [N: NumberEq])
    (and (== + +) (S:== "?" "?") (N:== 0 0)))
]

@; #############################################################################

@defform[
  (class class-id member-decl-or-def ...+ maybe-minimal)
  #:grammar [(member-decl-or-def [member-id]
                                 [member-id def-expr])
             (maybe-minimal (code:line)
                            (code:line #,(racketidfont "minimal") ([member-id ...+] ...)))]
]{

  Creates a new @tech{class} and @gtech{transformer bindings} related to it.

  A @racket[class] form with @var[n] members defines @var[n+1] names:

  @itemlist[

    @item{@var[class-id], a @deftech{class descriptor} that represents the
      @tech{class}.}

    @item{for each @var[member-id], a @gtech{transformer binding} that raises
      a syntax error.}

  ]

  Example:
  @example[
    (class Continuation
      [call]
      [abort (φ x (error (format "abort: ~a" x)))]
      minimal ([call]))
    (eval:error (call))
    (eval:error (abort 'no-op))
  ]

  The @racketid[minimal] directive, when present, constrains valid instances
  to only those that provide at least one of the minimal @var[member-id] sets.

  Example:
  @example[
    (eval:error
     (define-syntax BadContinuation
       (instance Continuation [abort void])))
  ]
}

@defform[
  (instance class-id maybe-extends [member-id def-expr] ...+)
  #:grammar [(maybe-extends (code:line)
                            (code:line #,(racketidfont "extends") (instance-id ...)))]
]{

  Produces an @deftech{instance descriptor} that represents a run-time
  implementation of the @tech{class} bound to @var[class-id] with its
  @var[member-id]s extended or overridden by @var[def-expr]s.

  The @racketid[extends] directive, when present, recursively imports the
  members of the @var[instance-id]s and the instances they extend.

  Examples:
  @example[
    (define current-esc (make-parameter #f))
    (define current-con (make-parameter #f))
    (define ((current-abort param msg) . xs)
      (if (param) ($ (param) xs) (error msg)))
  ]

  @example[
    (define-syntax EscapeContinuation
      (instance Continuation
        [call (φ f (call/ec (φ esc (parameterize ([current-esc esc]) (f)))))]
        [abort (current-abort current-esc "no escape continuation")]))
  ]

  @example[
    (define-syntax CurrentContinuation
      (instance Continuation
        [call (φ f (call/cc (φ con (parameterize ([current-con con]) (f)))))]
        [abort (current-abort current-con "no current continuation")]))
  ]
}

@deftogether[(
@defform[(with-instance instance-id/optional-prefix expr ...+)]
@defform[(with-instances (instance-id/optional-prefix ...) expr ...+)
         #:grammar [(instance-id/optional-prefix instance-id
                                                 [prefix-id instance-id])]]
)]{

  Evaluates the @var[exprs] with the members of the @var[class-id]s
  instantiated according to the instances bound to the @var[instance-id]s.

  If any @var[prefix-id]s are given, they are prepended to the names of the
  members defined by the corresponding @var[instance-id]s.

  Examples:
  @example[
    (with-instance EscapeContinuation
      (call (λ () (println 'START) (abort 'ESCAPE) (println 'DONE))))
  ]

  @example[
    (with-instance CurrentContinuation
      (call (λ () (abort 1 2) 3)))
  ]
}

@defform[(class-helper expr)]{

  Produces a @rtech{syntax transformer} that injects the @var[expr] into the
  caller's lexical context such that any @tech{class} members it contains can
  be resolved at run time.

  Example:
  @example[
    (define-syntax return (class-helper abort))
  ]

  @example[
    (with-instance EscapeContinuation
      (call (λ ()
              (for ([i (in-naturals)])
                (when (and (> i 10) (even? i))
                  (return i))))))
  ]
}
