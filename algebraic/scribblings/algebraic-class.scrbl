#lang scribble/manual

@title[#:tag "class" #:style 'toc]{Classes: Ad Hoc Polymorphism}

@require{./algebraic-includes.rkt}

@; #############################################################################

@defmodule[algebraic/class]

This module provides a simple dynamic dispatch mechanism for ad hoc
polymorphism.

@table-of-contents[]

@; #############################################################################

@include-section{./algebraic-class-core.scrbl}
@include-section{./algebraic-class-base.scrbl}
@include-section{./algebraic-class-data.scrbl}
