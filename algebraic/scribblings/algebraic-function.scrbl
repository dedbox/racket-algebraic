#lang scribble/manual

@title[#:tag "ref:fun"]{Functions}

@require{./algebraic-includes.rkt}

@require[
  @for-label[
    algebraic/racket/base
    racket/contract/base
  ]
]

@; #############################################################################

@defmodule[algebraic/function]

The bindings documented in this section are provided by
@racketmodname[algebraic/function] @racketmodname[algebraic/racket/base].

A @deftech{function} is a procedure that either deconstructs or rejects a
fully-evaluated argument or argument list. Functions are created with the
single-argument @racket[φ] (or @racket[phi]) and @racket[function] forms, or
the multi-argument variants @racket[φ*] (or @racket[phi*]) and
@racket[function*].

The @racket[φ] (@racket[phi]) form creates a @tech{function} of exactly one
argument with exactly one clause.

@example[
  (data Peano (Zero Succ))
  (define inc (φ a (Succ a)))
  (define dec (φ (Succ b) b))
  (function? inc)
  (values (inc Zero)
          (dec (Succ (Succ Zero))))
]

The @racket[φ*] (@racket[phi*]) form creates a @tech{function} of any number
of arguments with exactly one clause.

@example[
  (define cmp
    (φ* (a b)
      ((cond [(number? a) <] [(char? a) char<?]) a b)))
  (cmp 1 2)
  (cmp #\x #\y)
]

The @racket[function] form creates a @tech{function} of exactly one argument
with one or more clauses.

@example[
  (define peano (function [0 Zero] [n (Succ (peano (- n 1)))]))
  (define num (function [Zero 0] [(Succ p) (+ 1 (num p))]))
  (peano 3)
  (num (Succ (Succ (Succ Zero))))
]

The @racket[function*] form creates a @tech{function} of any number of
arguments with one or more clauses.

@example[
  (define add
    (function* [(a Zero) a]
               [(a (Succ b)) (Succ (add a b))]))
  (num (add (peano 3) (peano 2)))
]

@tech{Functions} created by @racket[function*] can have clauses with no
arguments, and the number of arguments for each clause can vary.

@example[
  (define num-args
    (function* [() 0]
               [(_ . rest) (+ 1 (apply num-args rest))]))
  (num-args)
  (num-args - -)
  (num-args - - - - -)
]

@; -----------------------------------------------------------------------------

@deftogether[(
@defform[(φ patt fun-directive ... body ...+)]
@defform[(phi patt fun-directive ... body ...+)]
@defform/subs[
  #:literals (quasiquote unquote quote void)
  (function [patt fun-directive ... body ...+] ...+)
  [(patt literal-data
         (quasiquote #,(var qfp))
         wildcard-id
         variable-id
         #,(racketparenfont "#(" (var patt) " ...)")
         #,(racketparenfont "#&" (var patt))
         #,(racketparenfont "#hash([" (var key) " . " (var patt) "] ...)")
         product-id
         (product-id patt ...)
         (product-id patt ... . patt)
         (patt #:as alias-patt)
         (patt #:if condition-expr)
         regexp
         (regexp patt ...+)
         (regexp patt ... . patt)
         (struct-id [field-id patt] ...)
         (struct-id patt ...)
         (void)
         (patt ...)
         (patt ... . patt))
   (literal-data boolean
                 character
                 number
                 string
                 bytes)
   (qfp (unquote patt)
        (qfp . qfp)
        datum)
   (fun-directive (code:line #:as alias-patt)
                  (code:line #:if condition-expr)
                  (code:line #:with consequent-patt premise-expr))]
])]{

  Creates a @tech{function} of one argument. When multiple clauses are given,
  they are attempted in the order specified.

  If @racket[#:as] @var[alias-patt]s are specified, they must all match the
  original input for the overall match to succeed.

  Optional @racket[#:if] @var[condition-expr]s specify that the pattern should
  only match if the @var[condition-expr]s produce true values.
  @var[condition-expr] is in the scope of all of the variables bound in
  @var[patt] and any preceding @racket[#:as] directives.

  Example:
  @example[
    (letrec ([fib (function
                    [n #:if (< n 2) 1]
                    [n (+ (fib (- n 1)))
                          (fib (- n 2))])])
      (map fib '(0 1 2 3 4 5 6)))
  ]

  An optional @racket[#:with] @var[consequent-patt] @var[premise-expr]
  evaluates the @var[premise-expr] in the context of all the variables of
  @var[patt] and the @var[alias-patt]s, if any. If the result matches
  @var[consequent-patt], the pattern's variables are added to the environment
  of subsequent side conditions. If the @racket[#:with] match fails, the
  overall match also fails.

  Multiple @racket[#:with] directives are evaluated independently from each
  other.

  Example:
  @example[
    ((φ (#rx"^([^ ]+) ([^ ]+) HTTP/([^\r\n]+)" method uri version)
       #:with #rx"^(?:GET|PUT|POST)$" method
       #:with (#rx"^(.+)\\?(.+)$" path params) uri
       #:with #rx"^[0-9]\\.[0-9]$" version
       (list method path params version))
     "GET /r/s?q=123&p=4 HTTP/1.0\r\n\r\n")
  ]

  A @var[patt] has one of the following forms:

  @specsubform[literal-data]{

    A Racket literal value: @racket[#t], @racket[#f], @var[character],
    @var[number], @var[string], or @var[bytes], or @tt{(quote @var[datum])}.

    Matches an @racket[equal?] constant.

    Example:
    @example[
      ((φ "one" 1) "one")
      ((φ 'two 2) 'two)
    ]
  }

  @specsubform[
    #:literals (quasiquote)
    (quasiquote #,(var qfp))
  ]{

    Introduces a @deftech{quasiquoted function pattern}, wherein all
    identifiers match symbols and @racket[unquote] escapes back to normal
    patterns.

    Example:
    @example[
      ((φ `(x y . ,('! a b)) (+ a b))
       '(x y ! 1 2))
    ]
  }

  @specsubform[wildcard-id]{

    An identifier whose name begins with an underscore ``@racketid[_]''.

    Matches anything and makes no bindings.

    Example:
    @example[
      ((φ _ 1) 0)
      (eval:error ((φ __x __x) 0))
    ]
  }

  @specsubform[variable-id]{

    An identifier that is not a @var[wildcard-id], @var[product-id], or
    @var[struct-id].

    Matches anything, and binds the identifier to the matching value in the
    @var[body]s. If a variable binding is used multiple times within a
    pattern, the corresponding matches must be the same according to
    @racket[match-equality-test].

    Example:
    @example[
      ((φ x x) 1)
      ((φ (x x) x) '(2 2))
      (eval:error ((φ (x x) x) '(3 4)))
    ]
  }

  @specsubform[#,(racketparenfont "#(" (var patt) " ...)")]{

    Matches @var[patt]s against the elements of a @rtech{vector}.

    Example:
    @example[
      ((φ #(a b c) (+ a b c)) (vector 1 2 3))
    ]
  }

  @specsubform[#,(racketparenfont "#&" (var patt))]{

    Matches @var[patt] against the contents of a @rtech{box}.

    Example:
    @example[
      ((φ #&x x) (box 1))
    ]
  }

  @specsubform[#,(racketparenfont "#hash([" (var key) " . " (var patt) "] ...)")]{

    Matches against a @rtech{hash table}'s key-value pairs, where @var[key] is
    a bare identifier or a @var[literal]. Any key-value pair of the hash table
    may be omitted, and such pairs can occur in any order.

    Example:
    @example[
      ((φ #hash([x . a] ["y" . b]) (list a b))
       (hash "y" 1 #t 2 'x 3))
    ]
  }

  @specsubform[product-id]{

    Matches a @tech{product} @tech{constructor} named @var[product-id].

    Example:
    @example[
      (data Ps+Qs (P Q))
      ((φ P 1) P)
    ]
  }

  @defsubform*[
    #:kind " "
    #:link-target? #f
    #:id [product-id (var product-id)]
    [(product-id patt ...)
     (product-id patt ... . patt)]
  ]{

    Matches an @tech{instance} of the @tech{product} bound to @var[product-id]
    with @tech{fields} that match @var[patt]s.

    Example:
    @example[
      ((φ (P x . xs) (list x xs)) (P 1 2 3))
    ]

  }

  @specsubform[(patt #:as alias-patt)]{

    Matches @var[patt] if @var[alias-patt] also matches the same value.

    Example:
    @example[
      ((φ ((P x) #:as y) (list x y)) (P 1))
    ]
  }

  @specsubform[(patt #:if condition-expr)]{

    Matches @var[patt] if @var[condition-expr] produces a true value.
    @var[condition-expr] is in the scope of all of the variables bound in
    @var[patt].

    Example:
    @example[
      ((φ (n #:if (> n 0)) '+++) 5)
      (eval:error ((φ (n #:if (> n 0)) '+++) -3))
    ]
  }

  @defsubform*[
    #:kind " "
    #:link-target? #f
    #:id [regexp (var regexp)]
    [regexp
     (regexp patt ...+)
     (regexp patt ... . patt)]
  ]{

    Matches @var[regexp] (a @rtech{regexp value} or byte-@rtech{regexp value})
    to a portion of its argument (a string, byte string, path, or input port)
    with @racket[regexp-match].

    Example:
    @example[
      (values
       ((φ #rx"x+y+" 1) "--xxyy++")
       ((φ #rx"a+b+" 2) (open-input-string "--aabb++")))
    ]

    If any @var[patt]s are given, they are matched against the results. If one
    or more capturing groups is present, the initial ``whole-match'' element
    of the result list is dropped before attempting to match @var[patt]s.

    Example:
    @example[
      (values
       ((φ (#rx"x+y+" xy) xy) "--xxyy++")
       ((φ (#rx"(a+)(b+)" as bs) (list as bs)) "--aabb++"))
      (eval:error ((φ (#rx"(a+)(b+)" as bs cs) 'OK) "--aabb++"))
    ]
  }

  @defsubform*[
    #:kind " "
    #:link-target? #f
    #:id [struct-id (var struct-id)]
    [(struct-id [field-id patt] ...)
     (struct-id patt ..)]
  ]{

    Matches an instance of a structure type named @var[struct-id], where each
    field in the instance matches the corresponding @var[patt].

    Example:
    @example[
      (struct F (a b c))
      ((φ (F x y z) (+ x y z)) (F 1 2 3))
    ]

    If @var[field-id]s are present, any field of @var[struct-id] may be
    omitted, and such fields can occur in any order.

    Example:
    @example[
      (struct tree (val left right))
      ((φ (tree [val a]
                [left (tree [right #f] [val b] [left #f])]
                [right #f])
         (list a b))
       (tree 0 (tree 1 #f #f) #f))
      ((φ (tree a (tree b #f #f) #f) (list a b))
       (tree 0 (tree 1 #f #f) #f))
    ]
  }

  @specsubform[
    #:literals (void)
    (void)
  ]{

    Matches a @seclink["void" #:doc '(lib
    "scribblings/reference/reference.scrbl")]{void} value.

  }

  @defsubform*[
    #:kind " "
    #:link-target? #f
    #:id [patt (var patt)]
    [(patt ...)
     (patt ... . patt)]
  ]{

    Matches @var[patt]s against the elements of a list.

    Example:
    @example[
      ((φ (a b c) (+ a b c)) '(1 2 3))
    ]

    If the pattern contains a delimited @racketparenfont{.}, the final
    @var[patt] is matched against the argument's tail.

    Example:
    @example[
      ((φ (a . b) (list a b)) '(1))
    ]
  }
}

@deftogether[(
@defform[(φ* formals fun-directive ... body ...+)]
@defform[(phi* formals fun-directive ... body ...+)]
@defform/subs[
  (function* [formals fun-directive ... body ...+] ...+)
  [(formals (patt ...)
            (patt ...+ . rest-patt)
            rest-patt)]
])]{

  Creates a @tech{function} of any number of arguments. The @var[formals]
  determine the number of arguments a @tech{function} accepts.

  A @var[formals] has one of the following forms:

  @specsubform[(patt ...)]{

    The function accepts as many argument values as the number of @var[patt]s.
    Each @var[patt] is matched against an argument value by position.

    Example:
    @example[
      (define fact
        (function* [(n) (fact n 1)]
                   [(0 a) a]
                   [(n a) (fact (- n 1) (* a n))]))
      (map fact '(0 1 2 3 4 5 6))
    ]
  }

  @specsubform[(patt ...+ . rest-patt)]{

    The function accepts at least as many arguments as the number of
    @var[patt]s. When the function is applied, the @var[patt]s are matched
    against argument values by position, and all leftover arguments are placed
    into a list that is matched against @var[rest-patt].

    Example:
    @example[
      ((function* [(x y . zs) (list x y zs)]) 1 2 3 4)
    ]
  }

  @specsubform[rest-patt]{

    The function accepts any number of arguments and places them into a list
    that is matched against @var[rest-patt].

    Example:
    @example[
      ((function* [xs (reverse xs)]) 1 2 3 4)
    ]
  }
}

@defproc[(function? [v any/c]) boolean?]{

  Returns @racket[#t] if @var[v] is a @tech{function}.

}
