#lang scribble/manual

@title[#:tag "linter"]{The Linter}

@require{./algebraic-includes.rkt}

@require[
  @for-label[
    (except-in algebraic/racket/base #%module-begin)
  ]
]

@define[linter-eval (module-language-evaluator 'algebraic/racket/base/linted)]
@define-syntax[example (algebraic-example linter-eval)]

@; #############################################################################

A case study in static program analysis with Algebraic Racket.

@defmodulelang[algebraic/racket/base/linted]

Adding @racketid[/linted] to the end of the @shlang line's module path enables
non-fatal code quality checks at expansion time.

@itemlist[

  @item{Shorten prelude function names}

  @item{Simplify functions according to their arguments}

]

Example:
@example[
  ((compose
    (curry apply append)
    (curry map cons '(1 2 3)))
   '((4) (5) (6)))

  ((λ (x) (+ x 2)) 1)

  (let ([f (φ* (x y z) (* x y z))])
    (f 2 3 4))
]

@nested[#:style 'code-inset]{
@verbatim{
; eval:1:2: linter: Prelude function can be shortened to `..'
;   in: compose
; eval:2:3: linter: Prelude function can be shortened to `>>'
;   in: curry
; eval:2:9: linter: Prelude function can be shortened to `$'
;   in: apply
; eval:2:15: linter: Prelude function can be shortened to `++'
;   in: append
; eval:3:3: linter: Prelude function can be shortened to `>>'
;   in: curry
; eval:3:13: linter: Prelude function can be shortened to `::'
;   in: cons
; eval:5:1: linter: Univariate λ can be shortened to φ
;   in: (λ (x) (+ x 2))
; eval:6:9: linter: Simple φ* can be shortened to λ
;   in: (φ* (x y z) (* x y z))
}}
