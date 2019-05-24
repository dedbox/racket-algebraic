#lang scribble/manual

@title[#:tag "ref"]{Algebraic Racket}

@require{./algebraic-includes.rkt}

@; #############################################################################

@defmodulelang[algebraic/racket/base]

Algebraic Racket is an extension for @hash-lang[racket/base] that provides
free-form, lexically scoped @seclink["ref:data"]{algebraic data structures}
along with several forms for creating @seclink["ref:fun"]{functions} and
@seclink["ref:mac"]{macros} with a uniform and compact destructuring syntax.

It streamlines the functional Racket programming experience in two key areas:

@subsubsub*section{Consistent Syntax}

The destructuring syntax for algebraic data and most other data is the same
for all @tech{function} and @tech{macro} forms.

@subsubsub*section{Transparent, Lexically Scoped Data}

Algebraic data constructors are like type tags. When applied to an argument
list, they produce an @tech[#:key "product instance"]{instance}---a list of
unnamed fields with the constructor at its head. They are easy to print and
easy to parse, just like @rtech{prefab} structs, except algebraic data is
lexically scoped and has a natural ordering.

@; #############################################################################

@include-section{./algebraic-data.scrbl}
@include-section{./algebraic-function.scrbl}
@include-section{./algebraic-macro.scrbl}
