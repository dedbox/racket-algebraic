#lang scribble/manual

@title[#:style 'no-sidebar]{The Algebraic Collection}

@author{@author+email["Eric Griffis" "dedbox@gmail.com"]}

@require{./algebraic-includes.rkt}

@table-of-contents[]

@include-section{./algebraic-racket.scrbl}
@include-section{./algebraic-prelude.scrbl}
@include-section{./algebraic-linter.scrbl}
@include-section{./algebraic-syntax-list.scrbl}
@include-section{./algebraic-class.scrbl}
@include-section{./algebraic-tutorial.scrbl}

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
