#lang scribble/manual

@title[#:tag "ref:data"]{Data}

@require{./algebraic-includes.rkt}

@require[
  @for-label[
    algebraic/racket/base
    racket/contract/base
  ]
]

@; #############################################################################

@defmodule[algebraic/data]

The bindings documented in this section are provided by the
@racketmodname[algebraic/data] and @racketmodname[algebraic/racket/base]
libraries.

The @racket[data] form defines several procedures and syntax
@rtech{transformers} for working with named @tech{product} and @tech{sum} data
structurs.

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
