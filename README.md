# Algebraic Racket
[![Racket Package](https://img.shields.io/badge/raco%20pkg-algebraic-red.svg)](https://pkgd.racket-lang.org/pkgn/package/algebraic)
[![Documentation](https://img.shields.io/badge/read-docs-blue.svg)](http://docs.racket-lang.org/algebraic/)
[![Build Status](https://travis-ci.org/dedbox/racket-algebraic.svg?branch=master)](https://travis-ci.org/dedbox/racket-algebraic)
[![Coverage Status](https://coveralls.io/repos/github/dedbox/racket-algebraic/badge.svg?branch=master)](https://coveralls.io/github/dedbox/racket-algebraic?branch=master)

A Racket 7+ extension for untyped algebraic data structures.

## What is Algebraic Racket?

Algebraic structures provide the operational under-pinnings for algebraic
data types. What's missing is the static typing constraints.

The current release provides a `#lang algebraic/racket/base` that extends
`#lang racket/base` with:

  1. First class, lexically scoped, naturally ordered data constructors,
     and

  2. A consistent destructuring syntax for functions and macros.

### Algebraic Data

As the name implies, Algebraic Racket works with two kinds of structure:
sums and products.

```
(data Maybe (Just Nothing))
```

This defines a sum named `Maybe` and two constituent products named `Just`
and `Nothing`. It also defines three membership predicates named `Maybe?`,
`Just?`, and `Nothing?` for recognizing products and their instances.

The elements of a sum are ordered by position so they can be compared and
sorted.

```
> (data A-Z (A B C D E F G H I J K L M N O P Q R S T U V W X Y Z))
> (let ([Πs (shuffle (data->list A-Z))])
    (values Πs (sort Πs data-less-than?)))
'(G O M W V C Q H T K F S Y U Z A B R J N E P X I L D)
'(A B C D E F G H I J K L M N O P Q R S T U V W X Y Z)
```

```
> (data Z-A (Z Y X W V U T S R Q P O N M L K J I H G F E D C B A))
> (let ([Πs (shuffle (data->list Z-A))])
    (values Πs (sort Πs data-less-than?)))
'(R C U N Y Z X L A K D H B J S V E G I W O M P Q F T)
'(Z Y X W V U T S R Q P O N M L K J I H G F E D C B A)
```

The products `Just` and `Nothing` can be matched against directly or they
can be used to construct instances. An instance is a transparent data
structure that resembles a tagged tuple.

Algebraic Racket imposes no restrictions on the type or number of
arguments accepted by a product.

```
> (values (Just) (Just 1) (Just 1 2))
(Just)
(Just 1)
(Just 1 2)
```

### Destructuring Syntax

Algebraic Racket provides `φ`/`phi` and `function` forms (and multi-arg
variants) which extend the variable binding sites of the `λ`/`lambda` form
with support for pattern-based destructuring.

```
> (define maybe
    (function*
      [(n _ Nothing) n]
      [(_ f (Just x)) (f x)]))
> (values
   (maybe #f values (Just 123))
   (maybe #f values Nothing))
123
#f
```

It also provides `μ`/`mu` and `macro` forms for syntax transformers.

```
> (define-syntax unroll-pow
    (μ* (b:number p:number) '(* #,@(make-list (var p) #'b))))
> (unroll-pow 2 5)
'(* 2 2 2 2 2)
```

#### Destructuring Patterns

Patterns for literal data are designed to look and feel as similar as
possible to the terms they match, including:

- Plain old Racket structs

- Unquoted literal values: boolean, number, string, bytes, char

- Unquoted containers that look like literals: pair, list, vector, hash

- Quoted data, including symbols

- Quasiquoted data with escapes

Other notable features include:

- Pattern guards, aliases, rest args, and wildcard/variable naming
  conventions all have a consistent look and feel across all function and
  macro forms.

- Macros also support ellipsis patterns and the `:syntax-class` naming
  convention.

- Regular expression patterns for functional string processing.

## Installation and Use

Algebraic Racket is distributed in the
[`algebraic`](https://pkgd.racket-lang.org/pkgn/package/algebraic) package
in the official Racket package repository. It can be installed from
DrRacket's package manager, or with `raco pkg` from the command line.

```
raco pkg install algebraic
```

To start using Algebraic Racket, set the initial line of your Racket
source file to:

```
#lang algebraic/racket/base
```

The package is fully documented, including a tutorial series of
interpreters developed for and with Algebraic Racket.

## Related Work

Why not just use `<something else that isn't Haskell>`?

### Plain old Racket structs

Prefab struct types are globally scoped, which has been a problem for me
in the past. Non-prefab structs are better in that regard, except basic
features like type hierarchies and functional updaters have a tendency to
interfere with each other.

Algebraic data constructors are lexically scoped and have no fixed arity.
Instances of algebraic data are always transparent.

Despite their differences, plain old Racket structs work pretty well with
Algebraic Racket's destructuring syntax. They're always there when you
need them.

### Hackett

Hackett is an excellent platform for pure, lazy functional programming in
the Racket software ecosystem. If I needed pure and lazy evaluation
semantics today, I'd try Hackett before considering Haskell seriously
again. Having said that, I don't usually want pure and lazy evaluation
semantics.

Wouldn't it be nice if I could import just the features I wanted from
Hackett and ignore the rest? That is the promise of language-oriented
programming, after all. For all I know, it's possible now.

Unfortunately, it isn't yet possible to mix and match off-the-shelf
features to comprise a tool as robust as Hackett (at least, that isn't
Hackett itself). Algebraic Racket is a step in that direction.

### Typed Racket

Typed Racket is a robust platform with a broad scope, and it keeps getting
better. The type system offers the static analyses I care about and the
documentation is excellent. After spending some time building interpreters
in Typed Racket, I decided I still wanted something else for two reasons:

1. Fumbling with types for common Racket idioms (like `apply`) became a
   distraction, and

2. Uncompiled code loads very slowly.

This may say more about my ignorance of Typed Racket than anything else.
Nonetheless, Algebraic Racket takes advantage of my intuition for Haskell
syntax while staying responsive enough for rapid iterative exploratory
programming.

### Redex

Redex is a domain-specific language for semantics engineering. It does a
lot more for the semantics engineer than Algebraic Racket ever will, like
typesetting and testing automation.

On the other hand, developing full applications with Redex is probably not
for the faint of heart. With Algebraic Racket, you get all the bells and
whistles of `#lang racket/base` and then some.

## Project Goals

The Algebraic project aims to provide a complete language-oriented toolkit
for high-performance algebraic data processing. It has two primary
objectives:

1. Supply and document the special forms and syntax classes comprising
   Algebraic Racket

2. Support the development of modular type systems and other static
   analyses for algebraic data as libraries

## Racket Version Restriction

Algebraic Racket is currently implemented with features only available
since Racket version 6.90.0.25: `~?` and `~@`.

## Contributing

Pull requests are welcome. For major changes, please start a thread on
[racket-users](https://groups.google.com/forum/#!forum/racket-users) or
open an [issue](https://github.com/dedbox/racket-algebraic/issues) to
discuss what you would like to change.
