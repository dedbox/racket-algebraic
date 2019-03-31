#lang scribble/manual

@title[#:style '(toc)]{Algebraic structures for untyped Racket}
@author{@author+email["Eric Griffis" "dedbox@gmail.com"]}

@require{./algebraic-includes.rkt}

@(require (for-label (except-in algebraic/racket/base fun?)
                     racket/contract/base
                     racket/match
                     (except-in syntax/parse pattern)))

@; #############################################################################

This package provides @hash-lang[algebraic/racket/base] which extends
@hash-lang[racket/base] with free-form, lexically scoped
@seclink["sec:data"]{algebraic data structures} along with several
@seclink["sec:functions"]{function} and @seclink["sec:macros"]{macro} forms
with uniform and compact destructuring syntax.

@hash-lang[algebraic/racket/base] synthesizes and specializes the
functionality of @racket[struct], @racket[match-lambda], and
@racket[syntax-parser] to streamline the funtional programming experience in
vanilla Racket in two key areas:

@subsubsub*section{Consistent Syntax}

The destructuring syntax for algebraic @racket[data] and most other data is
the same for all @tech{function} and @tech{macro} forms.

@subsubsub*section{Full Transparency}

Algebraic data @tech{constructors} are like type tags. When applied to an
argument list, they produce an @tech{instance}---a @deftech{list} or ordered
sequence of unnamed fields with the @tech{constructor} at its head. They are
easy to print and easy to parse, like @rtech{prefab} structs. The main
difference is algebraic @tech{constructors} are lexically scoped and have a
natural ordering.

@table-of-contents[]

@; =============================================================================

@section{Overview}

@; -----------------------------------------------------------------------------

@subsection[#:tag "sec:data"]{Data}

The @racket[data] form defines a variety of procedures and syntax
@rtech{transformers} for working with named @tech{products} and @tech{sums}.

A @deftech{product} identifies a family of structures comprising a @tech{list}
of @deftech{fields}, and a @deftech{sum} is a @tech{list} of @tech{products}.

@example[
  (data Peano (Zero Succ))
]

In this example, @racket[Peano] is a @tech{sum} of the @tech{products}
@racket[Zero] and @racket[Succ].

Each @tech{product} name is bound to a @deftech{constructor}, a function that
creates @tech{instances} of the named @tech{product}. An @deftech{instance} is
a concrete expression of the @tech{product} as a tagged tuple of run-time
values or expansion-time syntax fragments.

@example[
  (Succ Zero)
  (instance? (Succ Zero))
]

Equality is decided structurally for @tech{constructors} and their
@tech{instances}.

@example[
  (equal? Succ Succ)
  (equal? (Succ Zero) (Succ Zero))
  (equal? (Succ Zero) (Succ (Succ Zero)))
]

The @racket[data] form also defines several membership predicates.

@example[
  (Succ? Succ)
  ((sum Peano?) Succ)
  ((sum Peano?) (sum Peano))
]

To prevent name clashes in types like @racket[Unit], @tech{sum} bindings are
defined in their own @rtech{namespace}. The @racket[sum] form merely adds the
appropriate @rtech{scope} to an identifier.

@; -----------------------------------------------------------------------------

@subsection[#:tag "sec:functions"]{Functions}

A @deftech{function} is a procedure that either deconstructs or rejects a
fully-evaluated argument or argument list. Functions are created with the
single-argument @racket[φ] (or @racket[phi]) and @racket[function] forms, or
their multi-argument variants @racket[φ*] (or @racket[phi*]) and
@racket[function*].

The @racket[φ] (@racket[phi]) form creates a @tech{function} of exactly one
argument with exactly one clause.

@example[
  (define inc (φ a (Succ a)))
  (define dec (φ (Succ b) b))
  (function? inc)
  (inc Zero)
  (dec (Succ (Succ Zero)))
]

The @racket[φ*] (@racket[phi*]) form creates a @tech{function} of any number
of arguments with exactly one clause.

@example[
  (define cmp
    (φ* (a b)
      ((cond [(number? a) <] [(char? a) char<?]) a b)))
  (cmp 1 2)
  (cmp #\y #\x)
]

The @racket[function] form creates a @tech{function} of exactly one argument
with one or more clauses.

@example[
  (define peano
    (function [0 Zero]
              [n (Succ (peano (- n 1)))]))
  (define num
    (function [Zero 0]
              [(Succ p) (+ 1 (num p))]))
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

@subsection[#:tag "sec:macros"]{Macros}

A @deftech{macro} is a syntax @rtech{transformer} that either deconstructs or
rejects an argument or argument list at @rtech{expansion} time. Macros are
created with the single-argument @racket[μ] (or @racket[mu]) and
@racket[macro] forms, or the multi-argument variants @racket[μ*] (or
@racket[mu*]) and @racket[macro*].

The @racket[μ] (@racket[mu]) form creates a @tech{macro} of exactly one
argument with exactly one clause.

@example[
  (define-syntax infix (μ (a op b) (op a b)))
  (infix (5 - 3))
]

The @racket[μ*] (@racket[mu*]) form creates a @tech{macro} of any number of
arguments with exactly one clause.

@example[
  (define-syntax --> (μ* (p q) (or (not p) q)))
  (for*/list ([p '(#t #f)] [q '(#t #f)]) (--> p q))
]

The @racket[macro] form creates a @tech{macro} of exactly one argument with
one or more clauses.

@example[
  (define-syntax bin (macro [#f 0] [_ 1]))
  (bin #f)
  (bin (values #f))
]

The @racket[macro*] form creates a @tech{macro} of any number of arguments
with one or more clauses.

@example[
  (define-syntax and2 (macro* [(a b) ((function [#f #f] [_ b]) a)]))
  (define-syntax and*
    (macro* [() #t]
            [(a b) (and2 a b)]
            [(a bs ...) (and2 a (and* bs ...))]))
  (and* 2 #f 0)
  (and* 2 1 0)
]

@tech{Macros} are designed to simplify mundane meta-programming tasks. The
following example is a run-time implementation of the ``power'' function from
@cite{Taha2004}:

@example[
  (define f-power
    (function*
      [(0 _) 1]
      [(n x) (* x (f-power (- n 1) x))]))
  (map (curry f-power 3) '(0 1 2 3 4 5 6))
]

With @racket[unsyntax] and the @racket[var] form, applications of the power
function can be unrolled at expansion time.

@example[#:escape UNSYNTAX
  (define-syntax m-power
    (macro*
      [(0 _) 1]
      [(1 x) x]
      [(n:nat x) (* x (m-power #,(- (var n) 1) x))]))
  (define-syntax m-power3 (μ y (m-power 3 y)))
  (map (φ x (m-power3 x)) '(0 1 2 3 4 5 6))
]

With @racket[quote] and @racket[local-expand], the generated code can be
exposed.

@example[#:escape UNSYNTAX
  (define-syntax q-power
    (macro*
      [(0 _) 1]
      [(1 x) x]
      [(n:nat x)
       '#,(local-expand #`(* x (q-power #,(- (var n) 1) x))
                        'expression null)]))
  (q-power 3 2)
]

@; =============================================================================

@include-section["algebraic-tutorials.scrbl"]

@; @section[#:tag "tut"]{Tutorial Series: From Models to Interpreters}

@; =============================================================================

@section[#:tag "ref"]{API Reference}

@defmodulelang[algebraic/racket/base]

@; -----------------------------------------------------------------------------

@subsection[#:tag "ref:sums-products"]{Sums and Products}

@defform[
  (data sum-decl ...+)
  #:grammar [(sum-decl (code:line sum-id (product-id ...)))]
]{

  Creates a new @tech{sum} on a @tech{list} of @tech{products} and binds
  variables related to them.

  A @tech{sum} with @var[n] @tech{products} defines 3+3@var[n] names:

  @itemlist[
    @item{for each @var[sum-id]:
      @itemlist[

        @item{a @rtech{transformer} binding that encapsulates information
          about the @tech{sum} declaration.}

        @item{a @tech{sum} structure that can be printed to retrieve its
          definition.}

      ]
    }

    @item{@var[sum-id]@tt{?}, for each @var[sum-id]; a predicate that returns
      @racket[#t] for the @tech{sum} bound to @var[sum-id], its
      @tech{constructors} or their @tech{instances}, and @racket[#f] for any
      other value.}

    @item{for each @var[product-id]:
      @itemlist[

        @item{a @rtech{transformer} binding that encapsulates information
          about the @tech{product} declaration.}

        @item{a @tech{constructor} that takes any number of arguments and
          returns a new @tech{instance} of the @tech{product}.}

      ]
    }

    @item{@var[product-id]@tt{?}, for each @var[product-id]; a predicate that
      returns @racket[#t] for the @tech{constructor} bound to @var[product-id]
      or its instances and @racket[#f] for any other value.}

  ]

  Example:
  @example[
    (data Unit (Unit))
    (Unit? Unit)
    (Unit? (Unit))
    ((sum Unit?) Unit)
    ((sum Unit?) (sum Unit))
  ]
}

@defform[(sum id)]{

  Adds the @tech{sum} scope to @var[id].

  To prevent clashes between @tech{sums} and @tech{products} with the same
  name, @tech{sum} bindings are defined in their own namespace. This form adds
  a scope to @var[id] that represents the @tech{sum} namespace.

  Example:
  @example[
    (data Either (Left Right))
    (eval:error (Either? Left))
    ((sum Either?) Left)
    (eval:error ((sum Either?) Either))
    ((sum Either?) (sum Either))
  ]
}

@defproc[(data-less-than? [Π1 product?] [Π2 product?]) boolean?]{

  Returns @racket[#t] if the arguments are in the defined order.

  Examples:
  @example[
    (data ABC (A B C))
    (values (data-less-than? A C) (data-less-than? C A))
  ]

  @example[
    (data XYZ (X Y Z))
    (sort (list Z Y X) data-less-than?)
  ]
}

@defproc[(sum? [v any/c]) boolean?]{

  Returns @racket[#t] if @var[v] is a @tech{sum}.

}

@defproc[(product? [v any/c]) boolean?]{

  Returns @racket[#t] if @var[v] is a @tech{constructor}.

}

@defproc[(instance? [v any/c]) boolean?]{

  Returns @racket[#t] if @var[v] is an @tech{instance}.

}

@; -----------------------------------------------------------------------------

@subsection[#:tag "ref:functions"]{Functions}

@deftogether[(
@defform[(φ patt fun-directive ... body ...+)]
@defform/subs[
  #:literals (quasiquote unquote quote void)
  (phi patt fun-directive ... body ...+)
  [(patt literal
         wildcard-id
         variable-id
         product-id
         (product-id patt ...)
         (product-id patt ... . patt)
         reference-id
         (patt #:if condition-expr)
         (patt #:as alias-patt)
         regexp
         (regexp patt ...+)
         (regexp patt ... . patt)
         symbol
         (patt ...)
         (patt ... . patt)
         (struct-id ([field patt] ...))
         (struct-id patt ...)
         (quasiquote #,(var qfp))
         (void)
         #,(racketparenfont "#(" (var patt) ")")
         #,(racketparenfont "#&" (var patt))
         #,(racketparenfont "#hash([" (var key) " . " (var patt) "] ...)"))
   (literal boolean
            character
            number
            string
            bytes
            (quote #,(var datum)))
   (qfp literal
        id
        ()
        (qfp . qfp)
        (unquote patt))
   (fun-directive (code:line #:if condition-expr)
                  (code:line #:as alias-patt)
                  (code:line #:with consequent-patt premise-expr))]
])]{

  Creates a @tech{function} of one argument with one clause.

  If @racket[#:as] @var[alias-patt]s are specified, they must all match the
  original input for the overall match to succeed.

  Optional @racket[#:if] @var[condition-expr]s specify that the pattern should
  only match if the @var[condition-expr]s produce true values.
  @var[condition-expr] is in the scope of all of the variables bound in
  @var[patt] and any preceding @racket[#:as] directives.

  Example:
  @example[
    (data SZ (S Z))
    (let ([f (function
               [(S x y) #:if (not x) #:if (not y) 0]
               [(S x y) #:if (not (and x y)) (or x y)]
               [(S x y) #:if x #:if y (+ x y)])])
      (map f (list (S 1 2) (S #f 2) (S 1 #f) (S #f #f))))
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

  @specsubform[literal]{

    A Racket literal value: @racket[#t], @racket[#f], @var[character],
    @var[number], @var[string], @var[bytes], or @tt{(quote @var[datum])}.

    Matches an @racket[equal?] constant.

    Example:
    @example[
      ((φ "one" 1) "one")
    ]
  }

  @specsubform[wildcard-id]{

    An identifier whose name begins with an underscore ``@racketid[_]''.

    Matches anything, without binding any identifiers.

    Example:
    @example[
      ((φ _ 1) 0)
      (eval:error ((φ __x __x) 0))
    ]
  }

  @specsubform[variable-id]{

    An identifier whose name begins with a
    @racketlink[char-lower-case?]{lowercase} character.

    Matches anything, and binds the identifier to the matching value in the
    @var[body]s. If a variable binding is used multiple times within a
    pattern, the corresponding matches must be the same according to
    @racket[match-equality-test].

    Example:
    @example[
      ((φ x x) 1)
      ((φ (S x x) x) (S 2 2))
      (eval:error ((φ (S x x) x) (S 3 4)))
    ]
  }

  @specsubform[product-id]{

    Matches a @tech{constructor} named @var[product-id].

    Example:
    @example[
      ((φ S 1) S)
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
      ((φ (S x . xs) (list x xs)) (S 1 2 3))
    ]

  }

  @specsubform[reference-id]{

    A @rtech{bound} identifier that is not a @var[wildcard-id],
    @var[variable-id], or @var[constructor-id].

    Matches the bound value.

    Example:
    @example[
      ((φ + 'Plus) +)
      (eval:error ((φ + 'Plus) -))
    ]
  }

  @specsubform[(patt #:if condition-expr)]{

    Matches @var[patt] if @var[condition-expr] produces a true value.
    @var[cond-expr] is in the scope of all of the variables bound in
    @var[patt].

    Example:
    @example[
      ((φ (n #:if (> n 0)) '+++) 5)
      (eval:error ((φ (n #:if (> n 0)) '+++) -3))
    ]
  }

  @specsubform[(patt #:as alias-patt)]{

    Matches @var[patt] if @var[alias-patt] also matches the same value.

    Example:
    @example[
      ((φ ((S x) #:as y) (list x y)) (S 1))
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

    Matches @var[regexp] (a @rtech{regexp value} or byte-@rtech{regexp-value})
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

  @specsubform[symbol]{

    An unquoted datum literal that is not a @var[wildcard-id],
    @var[variable-id], or @var[product-id].

    Matches a symbol.

    Example:
    @example[
      ((φ $ 1) '$)
      (eval:error ((φ $ 1) $))
    ]
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

  @defsubform*[
    #:kind " "
    #:link-target? #f
    #:id [struct-id (var struct-id)]
    [(struct-id ([field patt] ...))
     (struct-id patt ..)]
  ]{

    Matches an instance of a structure type named @var[struct-id], where each
    field in the instance matches the corresponding @var[patt].

    Example:
    @example[
      (struct F (a b c))
      ((φ (F x y z) (+ x y z)) (F 1 2 3))
    ]

    If @var[field]s are present, any field of @var[struct-id] may be omitted,
    and such fields can occur in any order.

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
    #:literals (quasiquote)
    (quasiquote #,(var qfp))
  ]{

    Introduces a @deftech{quasiquoted function pattern}, wherein all
    identifiers match symbols and @racket[unquote] escapes back to normal
    patterns.

    Example:
    @example[
      ((φ `(x y . ,(S a b)) (+ a b))
       (list* 'x 'y (S 1 2)))
    ]
  }

  @specsubform[
    #:literals (void)
    (void)
  ]{

    Matches a @seclink["void" #:doc '(lib
    "scribblings/reference/reference.scrbl")]{void} value.

  }

  @specsubform[#,(racketparenfont "#(" (var patt) " ...)")]{

    Matches @var[patt]s against the elements of a @rtech{vector}.

    Example:
    @example[
      ((φ #(a b c) (+ a b c)) (vector 1 2 3))
    ]
  }

  @specsubform[#,(racketparenfont "#&" (var patt))]{

    Matches @var[patt] against the element of a @rtech{box}.

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
}

@defform[(function [patt fun-directive ... body ...+] ...+)]{

  Creates a @tech{function} of one argument with at least one clause. When
  multiple clauses are given, they are attempted in the order specified.

  Example:
  @example[
    (define fib
      (function [n #:if (< n 2) 1]
                [n (+ (fib (- n 1))
                      (fib (- n 2)))]))
    (map fib '(0 1 2 3 4 5 6))
  ]

}

@deftogether[(
@defform[(φ* formals fun-directive ... body ...+)]
@defform/subs[
  (phi* formals fun-directive ... body ...+)
  [(formals (patt ...)
            (patt ...+ . rest-patt)
            rest-patt)]
])]{

  Creates a @tech{function} of any number of arguments with one clause. The
  @var[formals] determine the number of arguments.

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

@defform[(function* [formals fun-directive ... body ...+] ...+)]{

  Creates a @tech{function} of any number of arguments with one or more
  clauses. When multiple clauses are given, they are attempted in the order
  specified.

}

@defproc[(function? [v any/c]) boolean?]{

  Returns @racket[#t] if @var[v] is a @tech{function}.

}

@; -----------------------------------------------------------------------------

@subsection[#:tag "ref:macros"]{Macros}

@deftogether[(
@defform[(μ mac-patt mac-directive ... body ...+)]
@defform/subs[
  #:literals (void quasiquote unquote)
  (mu mac-patt mac-directive ... body ...+)
  [(mac-patt literal
             wildcard-id
             variable-id
             id-literal
             (mac-patt #:if condition-expr)
             (mac-patt #:as alias-mac-patt)
             (struct-id mac-patt ...)
             (quasiquote #,(var qmp))
             (mac-patt ...)
             (mac-patt ...+ . mac-patt)
             (mac-patt ooo . mac-patt))
   (qmp (unquote mac-patt)
        (qmp . qmp)
        datum)
   (ooo ...
        ...+)
   (mac-directive (code:line #:with consequent-mac-patt premise-expr)
                  (code:line #:if condition-expr)
                  (code:line #:as alias-mac-patt))]
])]{

  Creates a @tech{macro} of one argument with one clause.

  A @var[mac-patt] is a @var[literal] or @var[wildcard-id] as defined for
  @racket[φ], or one of the following forms:

  @specsubform[variable-id]{

    An identifier whose name begins with a
    @racketlink[char-lower-case?]{lowercase} character.

    Matches anything, and binds the pattern variable to the matching sub-term
    in the @var[body]s. If the identifier is of the form
    @var[id:syntax-class-id], it is an @tech[#:doc '(lib
    "syntax/scribblings/syntax.scrbl")]{annotated pattern variable} and only
    matches forms described by the @stech{syntax class} bound to
    @var[syntax-class-id]. Otherwise, it matches anything.

    Example:
    @example[
      (define-syntax m (μ x:id (identifier? #'x)))
      (m a)
      (eval:error (m 3))
    ]
  }

  @specsubform[id-literal]{
    
    An identifier that is not a @var[wildcard-id] or @var[variable-id].

    Matches an identifier literal.

    Example:
    @example[
      (define-syntax m (μ ++ "plus plus"))
      (m ++)
      (eval:error (m --))
    ]
  }

  @specsubform[(mac-patt #:if condition-expr)]{

    First matches @var[mac-patt], then evaluates the @var[condition-expr] in
    the context of all previous variable bindings. If the value is
    @racket[#f], the match fails.

    Example:
    @example[
      (define-syntax m (μ (x #:if (number? (var x))) (+ x 2)))
      (m 1)
      (eval:error (m #f))
    ]
  }

  @specsubform[(mac-patt #:as alias-mac-patt)]{

    First matches @var[mac-patt], then matches @var[alias-mac-patt] against
    the same argument. If either pattern fails, the match fails.

    Example:
    @example[
      (let-syntax ([calc (μ ((x + y) #:as sum)
                           (append 'sum `(= ,(+ x y))))])
        (calc (1 + 2)))
    ]
  }

  @specsubform[(struct-id mac-patt ...)]{

    Matches a sequence of terms, where the first element @var[struct-id] names
    a structure type and subsequent elements match the corresponding
    @var[mac-patt].

    Example:
    @example[
      (struct F (a b c))
      (define-syntax m (μ (F x y z) (+ x y z)))
      (m (F 1 2 3))
    ]
  }

  @specsubform[
    #:literals (quasiquote)
    (quasiquote #,(var qmp))
  ]{

    Introduces a @deftech{quasiquoted macro pattern}, in which identifiers
    match symbols and @racket[unquote] escapes back to normal macro patterns.

    Example:
    @example[
      (define-syntax m (μ `(x ,y) y))
      (m (x #t))
      (eval:error (m (z #t)))
    ]
  }

  @specsubform[(mac-patt ...)]{

    Matches a parenthesized sequence of @var[mac-patt]s.

    Example:
    @example[
      (define-syntax swap (μ (a b) (b a)))
      (swap (0 S))
      (instance? (swap (0 S)))
    ]
  }

  @specsubform[(mac-patt ...+ . mac-patt)]{

    Matches a term with a list head and a tail separated by a delimited
    @racketparenfont{.}.

    Example:
    @example[
      (define-syntax m (μ (x y . z) (list x y z)))
      (m (1 2 . 3))
    ]
  }

  @specsubform[(head-mac-patt ooo . tail-mac-pat)]{

    Matches any term that can be decomposed into a list head matching some
    number of repetitions of @var[head-mac-patt] followed by a list tail
    matching @var[tail-mac-patt].

    Example:
    @example[
      (define-syntax m
        (macro* [(x ... (y ...) z ...+)
                 (list* x ... y ... z ...)]))
      (values
       (m 1 2 (3 4) 5 6)
       (m (3 4) 5 6))
      (eval:error (m 1 2 (3 4)))
    ]
  }

  The following pattern directives may appear any number of times in a macro
  clause:

  @specsubform[(code:line #:with mac-patt expr)]{

    Evaluates @var[expr] in the context of all pattern bindings and matches
    the result against @var[mac-patt]. The @var[expr] is implicitly
    @racket[quasisyntax]ed, so @var[unsyntax] and @var[unsyntax-splicing]
    escape to an expression within the transformer environment.

    Example:
    @example[#:escape UNSYNTAX
      (let-syntax ([m (macro
                        [x #:with (a) #,(list 10)
                           #:with b 1
                           (+ x a b)])])
        (m 100))
    ]
  }

  @specsubform[(code:line #:if condition-expr)]{

    Evaluates the @var[condition-expr] in the context of all previous variable
    bindings. If the value is @racket[#f], the match fails.

    Example:
    @example[#:escape UNSYNTAX
      (define-syntax m-fib
        (macro [n:nat #:if (< (var n) 2) 1]
               [n:nat (+ (m-fib #,(- (var n) 1))
                         (m-fib #,(- (var n) 2)))]))
      (values
       (m-fib 0) (m-fib 1) (m-fib 2)
       (m-fib 3) (m-fib 4) (m-fib 5) (m-fib 6))
      (eval:error (let ([a 7]) (m-fib a)))
    ]
  }

  @specsubform[(code:line #:as alias-mac-patt)]{

    Matches the original argument list against @var[alias-mac-patt].

    Example:
    @example[
      (let-syntax ([calc (μ* (x + y) #:as sum
                           (append 'sum `(= ,(+ x y))))])
        (calc 1 + 2))
    ]
  }
}

@defform[(macro [mac-patt mac-directive ... body ...+] ...+)]{

  Creates a @tech{macro} of one argument with one or more clauses. When
  multiple clauses are given, they are attempted in the order specified.

}

@deftogether[(
@defform[(μ* mac-formals mac-directive ... body ...+)]
@defform/subs[
   (mu* mac-formals mac-directive ... body ...+)
   [(mac-formals (mac-patt ...)
                 (mac-patt ...+ . rest-mac-patt)
                 rest-mac-patt)]
])]{

  Creates a @tech{macro} with any number of arguments and one clause. The
  @var[mac-formals] determine the number of arguments in the same way as for
  @racket[φ*], except with @var[mac-patt]s instead of @var[patt]s.

}

@defform[(macro* [mac-formals mac-directive ... body ...+] ...+)]{

  Creates a @tech{macro} with any number of arguments and at least one clause.
  When multiple clauses are given, they are attempted in the order specified.

}

@defform[(var id)]{

  Returns the value bound to @var[id] in the transformer environment.

}

@; @section{Experimental Features}

@; =============================================================================

@bibliography[
  @; @bib-entry[
  @;   #:key "Krishnamurthi2001"
  @;   #:title "Linguistic Reuse"
  @;   #:author "Krishnamurthi, Sriram"
  @;   #:location "PhD dissertation, Rice University"
  @;   #:date "2001"
  @; ]

  @bib-entry[
    #:key "Taha2004"
    #:title "A gentle introduction to multi-stage programming"
    #:author "Taha, Walid"
    #:location @list{In @emph{Domain-Specific Program Generation} (pp 30-50). Springer, Berlin, Heidelberg, 2004}
  ]
]
