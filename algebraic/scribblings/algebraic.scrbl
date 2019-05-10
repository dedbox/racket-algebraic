#lang scribble/manual

@title[#:style '(toc)]{Algebraic Racket}

@author{@author+email["Eric Griffis" "dedbox@gmail.com"]}

@require{./algebraic-includes.rkt}

@require[
  @for-label[
    algebraic/racket/base
    racket/contract/base
    racket/match
    (except-in syntax/parse pattern)
  ]
]

@; #############################################################################

The @hash-lang[algebraic/racket/base] extends @hash-lang[racket/base] with
free-form, lexically scoped @seclink["sec:data"]{algebraic data structures}
along with several forms for creating @seclink["sec:functions"]{functions} and
@seclink["sec:macros"]{macros} with a uniform and compact destructuring
syntax.

@hash-lang[algebraic/racket/base] streamlines the functional Racket
programming experience in two key areas:

@subsubsub*section{Consistent Syntax}

The destructuring syntax for algebraic @racket[data] and most other data is
the same for all @tech{function} and @tech{macro} forms.

@subsubsub*section{Transparent, Lexically Scoped Data}

Algebraic data @tech{constructors} are like type tags. When applied to an
argument list, they produce an @tech{instance}---a list of unnamed fields with
the @tech{constructor} at its head. They are easy to print and easy to parse,
just like @rtech{prefab} structs, except algebraic @tech{constructors} are
lexically scoped and have a natural ordering.

@table-of-contents[]

@; =============================================================================

@section{Overview}

@; -----------------------------------------------------------------------------

@subsection[#:tag "sec:data"]{Data}

The @racket[data] form defines several procedures and syntax
@rtech{transformers} for working with named @tech{product} and @tech{sum}
data structurs.

A @deftech{product} identifies a family of structures comprising a list of
@deftech{fields}, and a @deftech{sum} is a list of @tech{products}.

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

To prevent @tech{sum} and @tech{product} names from clashing, the @tech{sum}
bindings are defined in their own @rtech{namespace}. Use the @racket[sum] form
to add the appropriate @rtech{scope} to an identifier.

@; -----------------------------------------------------------------------------

@subsection[#:tag "sec:functions"]{Functions}

A @deftech{function} is a procedure that either deconstructs or rejects a
fully-evaluated argument or argument list. Functions are created with the
single-argument @racket[φ] (or @racket[phi]) and @racket[function] forms, or
the multi-argument variants @racket[φ*] (or @racket[phi*]) and
@racket[function*].

The @racket[φ] (@racket[phi]) form creates a @tech{function} of exactly one
argument with exactly one clause.

@example[
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
  (infix (1 - (infix ((infix (2 + (infix (3 / 4)))) * (infix (5 - 6))))))
]

The @racket[μ*] (@racket[mu*]) form creates a @tech{macro} of any number of
arguments with exactly one clause.

@example[
  (define-syntax infi* (μ* (a op b) (op a b)))
  (infi* 1 - (infi* (infi* 2 + (infi* 3 / 4)) * (infi* 5 - 6)))
]

The @racket[macro] form creates a @tech{macro} of exactly one argument with
one or more clauses.

@example[
  (define-syntax infixr
    (macro
      [(a op b) (op (infixr a) (infixr b))]
      [a a]))
  (infixr (1 - ((2 + (3 / 4)) * (5 - 6))))
]

The @racket[macro*] form creates a @tech{macro} of any number of arguments
with one or more clauses.

@example[#:escape UNSYNTAX
  (define-syntax infixr*
    (macro*
      [(a op b ...) (op (infixr* a) (infixr* b ...))]
      [((a ...)) (infixr* a ...)]
      [(a) a]))
  (infixr* 1 - (2 + 3 / 4) * (5 - 6))
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

With @racket[quote] and @racket[local-expand], we can expose the generated
code.

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

@section[#:tag "ref"]{API Reference}

This section of the manual defines the core Algebraic Racket constructs.

@defmodulelang[algebraic/racket/base]

The bindings defined in this manual are exported by the
@racketmodname[algebraic/racket/base] language.

@; -----------------------------------------------------------------------------

@subsection[#:tag "ref:sums-products"]{Sums and Products}

@defmodule[algebraic/data]

The bindings documented in this section are provided by the
@racketmodname[algebraic/data] and @racketmodname[algebraic/racket/base]
libraries.

@defform[
  (data sum-decl ...+)
  #:grammar [(sum-decl (code:line sum-id (product-id ...)))]
]{

  Creates a new @tech{sum} on a list of @tech{products} and binds variables
  related to them.

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
  a @rtech{scope} to @var[id] that represents the @tech{sum}
  @rtech{namespace}.

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

  Returns @racket[#t] if the arguments belong to the same @tech{sum} and are
  in order.

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

@defproc[(data->list [arg any/c]) (listof any/c)]{

  If @var[arg] is a @tech{sum}, returns its @tech{products} in the order they
  were defined.

  If @var[arg] is an @tech{instance}, returns its @tech{constructor} followed
  by its @tech{fields}.

  Any other @var[arg] is returned as a singleton list.

  @example[
    (data ABC (A B C))
    (data->list (sum ABC))
    (data->list (A 1 2 3))
    (data->list 123)
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

@defmodule[algebraic/function]

The bindings documented in this section are provided by the
@racketmodname[algebraic/function] and @racketmodname[algebraic/racket/base]
libraries.

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

@; -----------------------------------------------------------------------------

@subsection[#:tag "ref:macros"]{Macros}

@defmodule[algebraic/macro]

The bindings documented in this section are provided by the
@racketmodname[algebraic/macro] and @racketmodname[algebraic/racket/base]
libraries.

@deftogether[(
@defform[(μ parse-option ... mac-patt mac-directive ... body ...+)]
@defform[(mu parse-option ... mac-patt mac-directive ... body ...+)]
@defform/subs[
  #:literals (void quasiquote unquote)
  (macro parse-option [mac-patt mac-directive ... body ...+] ...+)
  [(mac-patt literal-data
             (quasiquote #,(var qmp))
             wildcard-id
             variable-id
             #,(racketparenfont "#(" (var mac-patt) " ...)")
             #,(racketparenfont "#&" (var mac-patt))
             (struct-id mac-patt ...)
             (mac-patt #:as alias-mac-patt)
             (mac-patt #:if condition-expr)
             (mac-patt ooo . mac-patt)
             (mac-patt ...)
             (mac-patt ...+ . mac-patt))
   (qmp (unquote mac-patt)
        (qmp . qmp)
        datum)
   (ooo ...
        ...+)
   (mac-directive (code:line #:as alias-mac-patt)
                  (code:line #:if condition-expr)
                  (code:line #:with consequent-mac-patt premise-expr))]
])]{

  Creates a @tech{macro} of one argument with one clause. When multiple
  clauses are given, they are attempted in the order specified.

  Any @var[parse-option]s are passed to @racket[syntax-parse] unaltered. See
  @racket[syntax-parse] for details.

  A @var[mac-patt] is a @var[literal-data] or @var[wildcard-id] as defined for
  @racket[φ], or one of the following forms:

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

  @specsubform[#,(racketparenfont "#(" (var mac-patt) " ...)")]{

    Matches @var[mac-patt]s against the elements of a @rtech{vector}.

    Example:
    @example[
      (let-syntax ([m (μ #(a b c) (+ a b c))])
        (m #(1 2 3)))
    ]
  }

  @specsubform[#,(racketparenfont "#&" (var mac-patt))]{

    Matches @var[mac-patt] against the contents of a @rtech{box}.

    Example:
    @example[
      (let-syntax ([m (μ #&x x)])
        (m #&1))
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

  @specsubform[(mac-patt #:as alias-mac-patt)]{

    First matches @var[mac-patt], then matches @var[alias-mac-patt] against
    the same argument. If either pattern fails, the match fails.

    Example:
    @example[
      (let-syntax ([calc (μ ((x + y) #:as z) `(z = ,(+ x y)))])
        (calc (1 + 2)))
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

  @specsubform[(mac-patt ...)]{

    Matches a parenthesized sequence of @var[mac-patt]s.

    Example:
    @example[
      (data Ss (S))
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

  The following pattern directives may appear any number of times in a macro
  clause:

  @specsubform[(code:line #:as alias-mac-patt)]{

    Matches the original argument list against @var[alias-mac-patt].

    Example:
    @example[
      (let-syntax ([calc (μ* (x + y) #:as z `(,@'z = ,(+ x y)))])
        (calc 1 + 2))
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
      (eval:error (let ([a 7]) (m-fib a)))
      (eval-syntax #`(list #,@(for/list ([n 7]) #`(m-fib #,n))))
    ]
  }

  @specsubform[(code:line #:with consequent-mac-patt premise-expr)]{

    Evaluates @var[premise-expr] in the context of all pattern bindings and
    matches the result against @var[consequent-mac-patt]. The
    @var[premise-expr] is implicitly @racket[quasisyntax]ed, so @var[unsyntax]
    and @var[unsyntax-splicing] escape to an expression within the transformer
    environment.

    Example:
    @example[#:escape UNSYNTAX
      (let-syntax ([m (macro
                        [x #:with (a) (list #'10)
                           #:with b #'1
                           (+ x a b)])])
        (m 100))
    ]
  }
}

@deftogether[(
@defform[(μ* parse-option ... mac-formals mac-directive ... body ...+)]
@defform[(mu* parse-option ... mac-formals mac-directive ... body ...+)]
@defform/subs[
  (macro* parse-option ... [mac-formals mac-directive ... body ...+] ...+)
  [(mac-formals (mac-patt ...)
                (mac-patt ...+ . rest-mac-patt)
                rest-mac-patt)]
])]{

  Creates a @tech{macro} with any number of arguments. The @var[mac-formals]
  determine the number of arguments in the same way as for @racket[φ*], except
  with @var[mac-patt]s instead of @var[patt]s. When multiple clauses are
  given, they are attempted in the order specified.

}

@defform[(var id)]{

  Returns the value bound to @var[id] in the transformer environment.

}

@; =============================================================================

@include-section["algebraic-tutorials.scrbl"]

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
