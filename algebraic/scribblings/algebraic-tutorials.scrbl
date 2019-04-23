#lang scribble/manual

@title[#:tag "tut" #:style '(toc)]{Tutorial Series: From Models to Interpreters}

@require{./algebraic-includes.rkt}

A core use case for @algebraic-mod is fast and cheap interpreter development.
With the right tooling, throw-away interpreters can be an easy way to explore
and validate language design choices quickly.

As it happens, Haskell is already pretty good at this. Algebraic data types
and functional destructuring syntax make interpreters easy to read and write,
and its type system keeps track of pervasive breaking changes for you.
@algebraic-mod addresses the untyped half of this equation---algebraic data
@emph{structures} (ADTs sans typing constraints) with destructuring syntax.

In this series, we will implement three interpreters based on the models
originally used to design @algebraic-mod itself:

@itemlist[
  #:style 'ordered

  @item{A @emph{core} model based on the untyped λ-calculus,}

  @item{An @emph{extended} syntax that compiles down to core constructs, and}

  @item{A @emph{hosted} variant that borrows additional constructs from the
  implementing platform.}

]

The tutorials are written in an informal style with code and discussion mixed
together. If you prefer source code, it can be read all in one place
@hyperlink["https://github.com/dedbox/racket-algebraic/tree/master/model"]{on
github}.

@local-table-of-contents[]

@include-section["algebraic-tutorial-core.scrbl"]
@include-section["algebraic-tutorial-ext.scrbl"]
@include-section["algebraic-tutorial-host.scrbl"]
