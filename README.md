# Algebraic Racket
[![Racket Package](https://img.shields.io/badge/raco%20pkg-algebraic-red.svg)](https://pkgd.racket-lang.org/pkgn/package/algebraic)
[![Documentation](https://img.shields.io/badge/read-docs-blue.svg)](http://docs.racket-lang.org/algebraic/)
[![Build Status](https://travis-ci.org/dedbox/racket-algebraic.svg?branch=master)](https://travis-ci.org/dedbox/racket-algebraic)
[![Coverage Status](https://coveralls.io/repos/github/dedbox/racket-algebraic/badge.svg?branch=master)](https://coveralls.io/github/dedbox/racket-algebraic?branch=master)

A Racket 7.0+ extension for untyped algebraic data structures.

## Why Algebraic Racket?

I love working with Racket, and I implement a lot of interpreters. Mostly,
I use them to explore programming language models as I design them. For
years, I'd use Haskell for this because of its algebraic data types and
destructuring syntax. At the same time, I usually prefer Racket's eager
effectful semantics and standard library over Haskell's, so I created
Algebraic Racket to bridge the gap.

## What is Algebraic Racket?

This release provides a `#lang algebraic/racket/base` that adds two
features to `#lang racket/base`:

  1. First class, lexically scoped data constructors, and
  2. A consistent destructuring syntax for functions and macros.

### Algebraic data structures

Algebraic data structures provide the operational under-pinnings for
algebraic data types. What's missing is the static typing constraints and
the analyses they enable. As implied by the name, Algebraic Racket works
with two kinds of structures: sums and products.

Sum and product structures are defined together.

```
(data Maybe (Just Nothing))
```

This defines a sum named `Maybe` and two constituent product constructors
named `Just` and `Nothing`. It also defines a membership predicate
`Maybe?` and two predicates `Just?` and `Nothing?` for identifying a
product and its instances.

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

The product constructors `Just` and `Nothing` can be matched against
directly, or they can be applied to argument lists to produce instances of
the product type. An instance is a transparent data structure that
resembles a tagged tuple. Algebraic Racket imposes no restrictions on the
kind or number of arguments accepted by product constructors.

```
> (values (Just) (Just 1) (Just 1 2))
(Just)
(Just 1)
(Just 1 2)
```

### Destructuring syntax

Algebraic Racket introduces `phi` and `function` forms (and some variants)
which extend the variable binding sites of the `lambda` forms with support
for functional pattern matching and destructuring.

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

Similarly, it introduces `mu` and `macro` forms for defining syntax
transformers.

```
> (define-syntax unroll-pow
    (μ* (b p) '(* #,@(make-list (var p) #'b))))
> (unroll-pow 2 5)
'(* 2 2 2 2 2)
```

#### Destructuring Patterns

Literal data patterns are designed to look and feel as similar as possible
to the terms they match, including:

- Algebraic data
- Plain old Racket structs
- Unquoted literal values: boolean, number, string, bytes, char
- Unquoted containers that look like literals: pair, list, vector, hash
- Quoted data, including symbols
- Quasiquoted data with escapes

Other notable features include pattern guards, rest args / ellipsis
patterns, intuitive naming conventions for wildcards and variables, and
support for the `:syntax-class` macro variable naming convention.

Additionally, regular expression literals in patterns will match a string,
byte string, or input port; and can destructure captured sub-matches.

## Installation and Use

Algebraic Racket is distributed as the `algebraic` package in the official
Racket package repository. It can be installed from DrRacket's package
manager, or with `raco pkg` from the command line.

```
raco pkg install algebraic
```

The package is fully documented and includes a tutorial series of
interpreters developed for and with Algebraic Racket.

To start using Algebraic Racket, set the initial line of your Racket
source file to:

```
#lang algebraic/racket/base
```

## Related Work

When discussing Algebraic Racket, someone inevitably asks a question of
the form, "Why not just use <something else that isn't Haskell>?".

### Plain old Racket structs

Prefab struct types are globally scoped, which has been a problem for me
in the past. Transparent and opaque structs are better in that regard,
except basic features like type hierarchies and functional updaters have a
tendency to interfere with each other.

With that in mind, plain old Racket structs work pretty well with
Algebraic Racket.

### Hackett

Hackett is an excellent platform for pure, lazy functional programming in
Racket land. If I needed pure and lazy evaluation semantics today, I'd
probably reach for Hackett before even thinking about Haskell. Having said
that, I don't usually want pure and lazy evaluation semantics.

Before starting this project, I thought, "wouldn't it be nice if I could
import just the features I wanted from Hackett and ignore the rest?" That
is the promise of language-oriented programming. Unfortunately, it isn't
yet possible to mix and match off-the-shelf features to comprise a tool as
robust as Hackett. Algebraic Racket is a step in that direction.

### Typed Racket

I spent some time building interpreters in Typed Racket and decided I'd
prefer something else for two reasons:

1. the type system turns simple Racket idioms (like using `apply`) into
distractions, and

2. uncompiled code runs very slow.

In my experience, these factors discourage rapid exploration of the
programming language design space.

### Redex

Redex is a domain-specific language for semantics engineering, and
Algebraic Racket is a small extension to a general purpose language I
happen to like building interpreters with. On the other hand, Redex does a
lot of things Algebraic Racket doesn't, like typesetting and automated
testing.

## Racket Version Restriction

Algebraic Racket is currently implemented with features only available in
Racket 7.0 or above: `~?` and `~@`. This restriction may be dropped in a
future release if there is sufficient demand for compatibility with older
versions of Racket.

## Contrbuting

Pull requests are welcome. For major changes, please open an issue first
to discuss what you would like to change.

Please make sure to update tests as appropriate.
