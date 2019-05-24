#lang scribble/manual

@title[#:tag "tut:ext" #:style '(quiet no-sidebar)]{A Syntax Extension}

@require{./algebraic-includes.rkt}
@require[
  texmath
  @for-label[
    algebraic/racket/base
    racket/function
  ]
]

@define[core-eval (module-language-evaluator 'algebraic/model/core)]
@define[ext-eval (module-language-evaluator 'algebraic/model/ext)]
@define-syntax[core-example (algebraic-example core-eval)]
@define-syntax[ext-example (algebraic-example ext-eval)]

@; #############################################################################

Part 2 of 3 in the @secref{tut}.

@table-of-contents[]

@defmodulelang[algebraic/model/ext]

In @secref{tut:core}, we built an s-expression interpreter as a small-step
term evaluator based loosely on the λ-calculus. To help encode complex terms
as runnable s-expressions, we used a manual term rewriting technique. This
time, we're going to simplify the surface syntax dramatically by building that
technique and one more into the @id[parse] and @id[print] @tech{functions}.

@subsubsub*section{Syntax}

@tabular[
  #:style full-width
  #:column-properties '(center)
  @list[
    @list[
      @tabular[
        @list[
          @list[
            @abs[
              @list{@${t} ⩴ ⋯ | @${t}⋯@${t} | @${t};⋯;@${t} | φ}
              [@${p} @${t}]
              [ "⋮"   "⋮"  ]
              [@${p} @${t}]
            ]
            @abs[
              @list{@~ | μ}
              [@${p} @${t}]
              [ "⋮"   "⋮"  ]
              [@${p} @${t}]
            ]
          ]
        ]
      ]
      @list{@${p} ⩴ ⋯ | @${p}⋯@${p} | @${p};⋯;@${p}}
    ]
  ]
]

@subsubsub*section{Evaluation Semantics}

@tabular[
  #:style full-width
  #:column-properties '(center) 
  @list[
    @list[
      @tabular[
        @list[
          @list[@list{@${t_1} @${t_2} ⋯ @${t_n} @~ ↝ @~ @${t_1} (@${t_2} ⋯ @${t_n})}]
          @list[@list{@${t_1};@${t_2};⋯;@${t_n} @~ ↝ @~ @${t_1};(@${t_2};⋯;@${t_n})}]
        ]
      ]
      @tabular[
        @list[
          @list[@list{@${p_1} @${p_2} ⋯ @${p_n} @~ ↝ @~ @${p_1} (@${p_2} ⋯ @${p_n})}]
          @list[@list{@${p_1};@${p_2};⋯;@${p_n} @~ ↝ @~ @${p_1};(@${p_2};⋯;@${p_n})}]
        ]
      ]
    ]
    @list[
      @list{@${n} ≥ 2}
      'cont
    ]
    @list[
      @tabular[
        @list[
          @list[
            @abs[
              "φ"
              [@${p_1} @${t_1}]
              [@${p_2} @${t_2}]
              [  "⋮"      "⋮"  ]
              [@${p_n} @${t_n}]
            ]
            @~ " ↝ " @~
            @abs[
              @list{φ@${p_1}.@${t_1};φ}
              [@${p_2} @${t_2}]
              [  "⋮"      "⋮"  ]
              [@${p_n} @${t_n}]
            ]
          ]
        ]
      ]
      @tabular[
        @list[
          @list[
            @abs[
              "μ"
              [@${p_1} @${t_1}]
              [@${p_2} @${t_2}]
              [  "⋮"      "⋮"  ]
              [@${p_n} @${t_n}]
            ]
            @~ " ↝ " @~
            @abs[
              @list{μ@${p_1}.@${t_1};μ}
              [@${p_2} @${t_2}]
              [  "⋮"      "⋮"  ]
              [@${p_n} @${t_n}]
            ]
          ]
        ]
      ]
    ]
  ]
]

@tabular[
  #:style full-width
  #:column-properties '(center)
  @list[
    @list[
      @tabular[
        @list[
          @list[
            @abs["φ" [@${p_1} @${t_1}]]
            @~ " ↝ " @~
            @list{φ@${p_1}.@${t_1}}
          ]
        ]
      ]
      @tabular[
        @list[
          @list[
            @abs["μ" [@${p_1} @${t_1}]]
            @~ " ↝ " @~
            @list{μ@${p_1}.@${t_1}}
          ]
        ]
      ]
    ]
  ]
]

@subsubsub*section{Special Forms}

@tabular[
  #:style full-width
  #:column-properties '(center)
  @list[
    @list[
      @list{fix = φ@${f}.(φ@${x}.@${f} φ@${y}.(@${x} @${x}) @${y}) (φ@${x}.@${f} φ@${y}.(@${x} @${x}) @${y})}
    ]
  ]
]

@tabular[
  #:style full-width
  #:column-properties '(center)
  @list[
    @list[
      @abs[
        "let ≈ μ"
        [@list{(@${p} @${t})} @${body} @list{(φ@${p}.@${body}) @${t}}]
        [@list{(@${p} @${t};@${cs})} @${body} @list{let (@${p} @${t}) let @${cs} @${body}}]
      ]
    ]
  ]
]

@tabular[
  #:style full-width
  #:column-properties '(center)
  @list[
    @list[
      @abs[
        "letrec ≈ μ"
        [   @list{(@${p} @${t})}     @${body} @list{let (@${p} fix φ@${p}.@${body}) @${t}}]
        [@list{(@${p} @${t};@${cs})} @${body} @list{letrec (@${p} @${t}) letrec @${cs} @${body}}]
      ]
    ]
  ]
]

@subsubsub*section{Example Programs}

@tabular[
  #:style full-width
  #:column-properties '(center)
  @list[
    @list[
      @abs[
        @list{add = φ}
        [@${a}       @Zero        @${a}]
        [@${a} @list{@Succ @${b}} @list{@Succ add @${a} @${b}}]
      ]
      @abs[
        @list{mul = φ}
        [@${a}       @Zero        @Zero]
        [@${a} @list{@Succ @${b}} @list{add @${a} mul @${a} @${b}}]
      ]
    ]
  ]
]

@tabular[
  #:style full-width
  #:column-properties '(center)
  @list[
    @list[
      @abs[
        @list{not = φ}
        [@False @True ]
        ["_"    @False]
      ]
      @tabular[
        @list[
          @list[
            @abs[
              @list{and = μ(@${a} @${b}).φ}
              [@False @False]
              ["_"    @${b} ]
            ] @${ a}
          ]
        ]
      ]
    ]
  ]
]

@tabular[
  #:style full-width
  #:column-properties '(center)
  @list[
    @list[
      @tabular[
        @list[
          @list[
            @abs[
              @list{or = μ(@${a} @${b}).φ}
              [@False @${b}]
              [@${x}  @${x}]
            ] @${ a}
          ]
        ]
      ]
      @tabular[
        @list[
          @list[
            @abs[
              @list{xor = μ(@${a} @${b}).φ}
              [@False @${b}]
              [@${x}  @list{and (not @${b}) @${x}}]
            ] @${ a}
          ]
        ]
      ]
    ]
  ]
]

@tabular[
  #:style full-width
  #:column-properties '(center)
  @list[
    @list[
      @abs[
        @list{list = μ}
        [@${x} "◊" @list{@Cons(@${x};@Nil)}]
        [@${x} @${xs} @list{@Cons(@${x};list @${xs})}]
      ]
    ]
  ]
]

@tabular[
  #:style full-width
  #:column-properties '(center)
  #:cell-properties '((vcenter))
  @list[
    @list[
      @tabular[
        @list[
          @list[
            @abs[
              @list{reverse = φ@${xs}.letrec @list[LP]@${rev} φ}
              [           @Nil            @${a} @${a}]
              [@list{@Cons(@${y};@${ys})} @${a} @list{@${rev} @${ys} @Cons(@${y};@${a})}]
            ]
            @list{@RP @${rev} @${xs} @Nil}
          ]
        ]
      ]
    ]
  ]
]

@; =============================================================================

@section[#:tag "tut:ext:syntax"]{The Extended Syntax}

The point of this extension is to make @secref{tut:core} easier to use. With a
more forgiving syntax, we can write more substantial programs. To get there,
we'll implement two simple rewriting tricks. First, we'll eliminate some
parentheses by generalizing the abstraction, application, and sequence forms
to any number of clauses or sub-terms. Then, we'll get rid of boilerplate by
pre-defining @id[let], @id[letrec], and @id[fix] in the parser. Our new
@id[parse] and @id[print] @tech{functions} will map between the new surface
syntax and the existing AST, so we can re-use the core @id[interpret]
@tech{function} without modification.

@; -----------------------------------------------------------------------------

@subsection[#:tag "tut:ext:prelims"]{Preliminaries}

We'll be re-using a good chunk of the core interpreter, so it makes sense to
import those things up front. It's a lot of stuff to keep track of, so we'll
just import all of @racketmodname[algebraic/model/core] except the forms that
would clobber Racket's default behavior. We should also exclude the names
we'll be re-defining.

@algebraic-code{
  (require (except-in algebraic/model/core
                      #%app #%datum #%module-begin #%top-interaction
                      parse show algebraic))
}

@; -----------------------------------------------------------------------------

@subsection[#:tag "tut:ext:currying"]{Application & Sequence ``Currying''}

@tabular[
  #:style full-width
  #:column-properties '(center)
  @list[
    @list[
      @list{@${t} ⩴ ⋯ | @${t}⋯@${t} | @${t};⋯;@${t}}
      @list{@${p} ⩴ ⋯ | @${p}⋯@${p} | @${p};⋯;@${p}}
    ]
  ]
]

In @secref{tut:core}, applications and sequences have exactly two sub-terms and
all sub-terms must be parenthesized. This design keeps the core language small
and easy to implement, but it also makes core programs hard to read and write.

@tabular[
  #:style full-width
  #:column-properties '(center) 
  @list[
    @list[
      @tabular[
        @list[
          @list[@list{@${t_1} @${t_2} ⋯ @${t_n} @~ ↝ @~ @${t_1} (@${t_2} ⋯ @${t_n})}]
          @list[@list{@${t_1};@${t_2};⋯;@${t_n} @~ ↝ @~ @${t_1};(@${t_2};⋯;@${t_n})}]
        ]
      ]
      @list{@${n} ≥ 2}
      @tabular[
        @list[
          @list[@list{@${p_1} @${p_2} ⋯ @${p_n} @~ ↝ @~ @${p_1} (@${p_2} ⋯ @${p_n})}]
          @list[@list{@${p_1};@${p_2};⋯;@${p_n} @~ ↝ @~ @${p_1};(@${p_2};⋯;@${p_n})}]
        ]
      ]
    ]
  ]
]

An extended application or sequence (term or pattern) with more than two
sub-terms (or sub-patterns) becomes an equivalent series of core applications or
sequences. If we view these rewrites as premise-less inference rules, or
@emph{tautologies}, they're easy to transcribe into the clauses of a recursive
@tech{function}.

@algebraic-code|{
  (define (parse t)
    (define term
      (function
        ...
        [('$ t1 t2 . ts) (TSeq (term t1) (term `($ ,t2 ,@ts)))]
        [('$ t1) (term t1)]
        [(t1 t2 . ts) (TApp (term t1) (term `(,t2 ,@ts)))]
        [(t1) (term t1)]
        ...))
    (define patt
      (function
        ...
        [('$ p1 p2 . ps) (PSeq (patt p1) (patt `($ ,p2 ,@ps)))]
        [('$ p1) (patt p1)]
        [(p1 p2 . ps) (PApp (patt p1) (patt `(  ,p2 ,@ps)))]
        [(p1) (patt p1)]
        ...))
    (term t))
}|

This formulation has a subtle yet significant quirk. In curried languages,
function application is usually left associative. To get an idea why, consider a
two-argument λ-abstraction:

@centered{λ@${xy}.@${f} @${x} @${y}}

Currying transforms this two-argument function into an equivalent series of
single-argument functions:

@centered{λ@${x}.λ@${y}.@${f} @${x} @${y}}

Now imagine @${f} is bound to a similarly curried two-argument λ-abstraction.
When @${f} is applied to @${x}, it produces another λ-abstraction which, when
applied to @${y}, completes the series and evaluates the original body of @${f}.
Because each application produces the next left-hand side of the series, we have
less parentheses with left-associative applications.

In Haskell, the expression @code{f x y} parses as if it was written @code{(f x)
y} but @code{1::2::[]} parses as @code{1::(2::[])}. Haskell makes this
distinction because left-associative applications are the backbone of the
popular @hyperlink["https://en.wikipedia.org/wiki/Tacit_programming"]{point-free
style} but list construction is usually right associative.

@margin-note{

  The @racket[curry] and @racket[compose] functions can be helpful, and
  @racket[define] has a currying shorthand notation as well, but they are much
  less satisfying to use than what Haskell offers. The
  @hyperlink["https://pkgs.racket-lang.org/package/compose-app"]{compose-app}
  and @hyperlink["https://pkgs.racket-lang.org/package/fancy-app"]{fancy-app}
  packages can be helpful in that regard.

}

In Racket, function application @emph{is} list construction and therefore also
right associative, so the same expression @racket[(f x y)] parses as if it were
written @racketparenfont{(}@id[f] @racketparenfont{.} @racketparenfont{(}@id[x]
@racketparenfont{.} @racketparenfont{(}@id[y] @racketparenfont{.}
@racketparenfont{())))}.

When Racket encounters a function application, it evaluates the entire argument
list before invoking the function. If we want to resolve the head position of
the list with a series of unary functions, we must express that desire in the
code or load an extension to do it for us; Racket won't do it automatically.
Since our interpreter is helping us design an extension for Racket and not
Haskell, our programs will contain less parentheses if extended applications
also associate to the right.

@; -----------------------------------------------------------------------------

@subsection[#:tag "tut:ext:abstractions"]{Multi-clause Abstractions}

@tabular[
  #:style full-width
  #:column-properties '(center)
  @list[
    @list[
      @tabular[
        @list[
          @list[
            @abs[
              @list{@${t} ⩴ ⋯ | φ}
              [@${p} @${t}]
              [ "⋮"   "⋮"  ]
              [@${p} @${t}]
            ]
            @abs[
              @list{@~ | μ}
              [@${p} @${t}]
              [ "⋮"   "⋮"  ]
              [@${p} @${t}]
            ]
          ]
        ]
      ]
    ]
  ]
]

Extended abstractions with multiple clauses are more compact and easier to read
than their core counterparts. Each row of an extended function or macro
corresponds to a core abstraction clause.

A row [@${p} | @${t}] can be read:

@inset{

  If the argument matches @${p}, then evaluate @${t} with the variables of
  @${p} in context.

}

If an extended abstraction has more than one clause, the clauses will be tried
in order from top to bottom until a match succeeds.

We need to transform an extended abstraction into @secref{tut:core} constructs
that behave the same way: a uniform sequence of core abstraction clauses.

@tabular[
  #:style full-width
  #:column-properties '(center)
  @list[
    @list[
      @tabular[
        @list[
          @list[
            @abs[
              "φ"
              [@${p_1} @${t_1}]
              [@${p_2} @${t_2}]
              [  "⋮"      "⋮"  ]
              [@${p_n} @${t_n}]
            ]
            @~ " ↝ " @~
            @abs[
              @list{φ@${p_1}.@${t_1};φ}
              [@${p_2} @${t_2}]
              [  "⋮"      "⋮"  ]
              [@${p_n} @${t_n}]
            ]
          ]
        ]
      ]
      @list{@${n} ≥ 2}
      @tabular[
        @list[
          @list[
            @abs[
              "μ"
              [@${p_1} @${t_1}]
              [@${p_2} @${t_2}]
              [  "⋮"      "⋮"  ]
              [@${p_n} @${t_n}]
            ]
            @~ " ↝ " @~
            @abs[
              @list{μ@${p_1}.@${t_1};μ}
              [@${p_2} @${t_2}]
              [  "⋮"      "⋮"  ]
              [@${p_n} @${t_n}]
            ]
          ]
        ]
      ]
    ]
  ]
]

@tabular[
  #:style full-width
  #:column-properties '(center)
  @list[
    @list[
      @tabular[
        @list[
          @list[
            @abs["φ" [@${p_1} @${t_1}]]
            @~ " ↝ " @~
            @list{φ@${p_1}.@${t_1}}
          ]
        ]
      ]
      @tabular[
        @list[
          @list[
            @abs["μ" [@${p_1} @${t_1}]]
            @~ " ↝ " @~
            @list{μ@${p_1}.@${t_1}}
          ]
        ]
      ]
    ]
  ]
]

Once again, we can implement these rewrites as tautologies.

@algebraic-code|{
  (define (parse t)
    (define term
      (function
        ...
        [('fun [p . t]) (term `(φ ,p ,@t))]
        [('mac [p . t]) (term `(μ ,p ,@t))]
        [('fun c . cs) (TSeq (term `(fun ,c)) (term `(fun ,@cs)))]
        [('mac c . cs) (TSeq (term `(mac ,c)) (term `(mac ,@cs)))]
        ...))
    (define patt ...)
    (term t))
}|

On the surface, a @id[fun] or @id[mac] is a non-empty list of pattern-term
pairs. A @id[fun] with only one clause becomes a function clause, and a @id[fun]
with more than one clause becomes a sequence of function clauses. If @id[t] is a
list, its outer parentheses can be dropped.

@; -----------------------------------------------------------------------------

@subsection[#:tag "tut:ext:bindings"]{Local Bindings}

In @secref{tut:core}, we developed a technique for encoding example programs in
terms our interpreter could evaluate. Starting with an ideal pseudo-syntax that
included hypothetical @id[let] and @id[letrec] forms, we manually rewrote the
ideal forms into terms in the surface syntax. We need these forms to define
pretty much everything else, so hard wiring them into our parser is an obvious
win.

@id[fix] is a straight-forward function expression.

@tabular[
  #:style full-width
  #:column-properties '(center)
  @list[
    @list[
      @list{fix = φ@${f}.(φ@${x}.@${f} φ@${y}.(@${x} @${x}) @${y}) (φ@${x}.@${f} φ@${y}.(@${x} @${x}) @${y})}
    ]
  ]
]

The remaining transformations for @id[let] and @id[letrec] cannot be expressed
with functions and macros directly, but we can take advantage of the extended
syntax to explain what goes wrong and how we'll handle it.

@tabular[
  #:style full-width
  #:column-properties '(center)
  @list[
    @list[
      @abs[
        "let ≈ μ"
        [@list{(@${p} @${t})} @${body} @list{(φ@${p}.@${body}) @${t}}]
        [@list{(@${p} @${t};@${cs})} @${body} @list{let (@${p} @${t}) let @${cs} @${body}}]
      ]
    ]
  ]
]

@margin-note{

  This definition of @id[let] behaves like Racket's @racket[let*] and not like
  @racket[let], though the latter can also be approximated in the calculus.

}

A @id[let] form behaves like a macro that takes one or more pattern-term clauses
and produces a nested series of function applications which evaluates and
deconstructs one term at a time.

We can't just define @id[let] with a macro because it would try to use a @Term
(@${p}) as the @Patt of a function abstraction (φ@${p}.@${body}). To avoid this
wrinkle, our parser will rewrite the surface syntax and then re-apply itself to
the result.

@tabular[
  #:style full-width
  #:column-properties '(center)
  @list[
    @list[
      @abs[
        "letrec ≈ μ"
        [   @list{(@${p} @${t})}     @${body} @list{let (@${p} fix φ@${p}.@${body}) @${t}}]
        [@list{(@${p} @${t};@${cs})} @${body} @list{letrec (@${p} @${t}) letrec @${cs} @${body}}]
      ]
    ]
  ]
]

The @id[letrec] form is similar to the @id[let] form, except it also @id[fix]es
the @${t}s for simple recursive definitions.

@algebraic-code|{
  (define (parse t)
    (define term
      (function
        ...
        ['fix (term '(φ f (φ x f φ y (x x) y) (φ x f φ y (x x) y)))]
        [('let () . body) (term body)]
        [('let ([p . t] . cs) . body) (term `((φ ,p let ,cs ,@body) ,@t))]
        [('letrec () . body) (term body)]
        [('letrec ([p . t] . cs) . body)
         (term `((φ ,p letrec ,cs ,@body) fix φ ,p ,@t))]
        ...))
    (define patt ...)
    (term t))
}|

This binds the names @id[let], @id[letrec], and @id[fix] globally.

@; -----------------------------------------------------------------------------

@subsection[#:tag "tut:ext:parse"]{The New Parser}

Together, the new clauses form the basis of our syntax extension. We'll round
out our new @id[parse] @tech{function} by folding in clauses for the remaining
core constructs.

@algebraic-code|{
  (define (parse t)
    (define term
      (function
        ['fix (term '(φ f (φ x f φ y (x x) y) (φ x f φ y (x x) y)))]
        [('fun [p . t]) (term `(φ ,p ,@t))]
        [('mac [p . t]) (term `(μ ,p ,@t))]
        [('fun c . cs) (TSeq (term `(fun ,c)) (term `(fun ,@cs)))]
        [('mac c . cs) (TSeq (term `(mac ,c)) (term `(mac ,@cs)))]
        [('let () . body) (term body)]
        [('let ([p . t] . cs) . body) (term `((φ ,p let ,cs ,@body) ,@t))]
        [('letrec () . body) (term body)]
        [('letrec ([p . t] . cs) . body)
         (term `((φ ,p letrec ,cs ,@body) fix φ ,p ,@t))]
        [('φ p1 . t2) (values-> TFun (α-rename (patt p1) (term t2)))]
        [('μ p1 . t2) (values-> TMac (α-rename (patt p1) (term t2)))]
        [('$ t1 t2 . ts) (TSeq (term t1) (term `($ ,t2 ,@ts)))]
        [('$ t1) (term t1)]
        [(t1 t2 . ts) (TApp (term t1) (term `(,t2 ,@ts)))]
        [(t1) (term t1)]
        [x #:if (con-name? x) (TCon x)]
        [x #:if (var-name? x) (TVar x)]
        ['◊ TUni]))
    (define patt
      (function
        [('$ p1 p2 . ps) (PSeq (patt p1) (patt `($ ,p2 ,@ps)))]
        [('$ p1) (patt p1)]
        [(p1 p2 . ps) (PApp (patt p1) (patt `(  ,p2 ,@ps)))]
        [(p1) (patt p1)]
        [x #:if (con-name? x) (PCon x)]
        [x #:if (var-name? x) (PVar x)]
        ['_ PWil]
        ['◊ PUni]))
    (term t))
}|

@; -----------------------------------------------------------------------------

@subsection[#:tag "tut:ext:print"]{The New Printer}

The extended printer is a little more interesting than its predecessor. To print
extended applications and sequences, we'll need to transform the abstract syntax
back into ordinary lists.

@algebraic-code{
  (define app->list
    (function
      [(TApp t1 t2) (cons (show t1) (app->list t2))]
      [(PApp p1 p2) (cons (show p1) (app->list p2))]
      [a (list (show a))]))

  (define seq->list
    (function
      [(TSeq t1 t2) (cons (show t1) (seq->list t2))]
      [(PSeq p1 p2) (cons (show p1) (seq->list p2))]
      [a (list (show a))]))
}

The @id[app->list] @tech{function} takes a two-argument @tech[#:key "product
instance"]{instance} of @TApp (or @PApp) and @racket[cons]es the left sub-term
(or sub-pattern) onto the result of recursively applying itself to the right
sub-term (or sub-pattern).

The @id[seq->list] @tech{function} does the same thing for @TSeqs and @PSeqs.

These two @tech{functions} are the workhorses of the new printer. For the most
part, it has the same structure as the old printer. With two extra clauses for
the extended abstractions, it'll look like this:

@algebraic-code{
  (define (show a)
    (define term
      (function
        [(TSeq (TFun _ _) _) ...]
        [(TSeq (TMac _ _) _) ...]
        [(TSeq _ _) ...]
        [(TApp _ _) ...]
        [(TFun p1 t2) ...]
        [(TMac p1 t2) ...]
        [(TVar x1) (α-restore x1)]
        [(TCon δ1) δ1]
        [TUni '◊]))
    (define patt
      (function
        [(PSeq _ _) ...]
        [(PApp _ _) ...]
        [(PVar x1) (α-restore x1)]
        [(PCon δ1) δ1]
        [PWil '_]
        [PUni '◊]))
    (term a))
}

Applications are the simplest non-trivial case. To @id[show] a series of @TApps
(or @PApps), we just need to list out the left-hand sides and that's exactly
what @id[app->list] does. Sequences get similar treatment, except we also
prepend a @id[$] to the list.

@algebraic-code|{
  (define (show a)
    (define term
      (function
        ...
        [(TSeq _ _) #:as t `($ ,@(seq->list t))]
        [(TApp _ _) #:as t (app->list t)]
        ...))
    (define patt
      (function
        ...
        [(PSeq _ _) #:as p `($ ,@(seq->list p))]
        [(PApp _ _) #:as p (app->list p)]
        ...))
    (term a))
}|

Here, we bind the whole term to an alias (@racket[#:as] @${t} or @${p}) and then
pass it to the helper which does most of the work.

The simple abstractions (@id[φ], @id[μ]) are slightly more involved. If the body
term is an application, we can drop the outer parentheses.

@algebraic-code|{
  (define (show a)
    (define term
      (function
        ...
        [(TFun p1 t2) `(φ ,(patt p1) ,@(app->list t2))]
        [(TMac p1 t2) `(μ ,(patt p1) ,@(app->list t2))]
        ...))
    (define patt ...)
    (term a))
}|

And finally, we'll transform sequences of abstractions into lists of extended
abstraction clauses with the symbol @id[fun] or @id[mac] prepended.

@algebraic-code|{
  (define (show a)
    (define term
      (function
        ...
        [(TSeq (TFun _ _) _) #:as t `(fun ,@(fun->list t))]
        [(TSeq (TMac _ _) _) #:as t `(mac ,@(mac->list t))]
        ...))
    (define patt ...)
    (define fun->list
      (function
        [(TFun p1 t2) `([,(patt p1) ,@(app->list t2)])]
        [(TSeq t1 t2) (append (fun->list t1) (fun->list t2))]))
    (define mac->list
      (function
        [(TMac p1 t2) `([,(patt p1) ,@(app->list t2)])]
        [(TSeq t1 t2) (append (mac->list t1) (mac->list t2))]))
    (term a))
}|

The @id[fun->list] and @id[mac->list] helpers do pretty much the same thing as
@id[app->list] or @id[seq->list] but for sequences of abstraction clauses. They
both need the @id[patt] combinator, so we'll just leave them inside the
@id[show] @tech{function}.

The full extended parser:

@algebraic-code|{
  (define (show a)
    (define term
      (function
        [(TSeq (TFun _ _) _) #:as t `(fun ,@(fun->list t))]
        [(TSeq (TMac _ _) _) #:as t `(mac ,@(mac->list t))]
        [(TSeq _ _) #:as t `($ ,@(seq->list t))]
        [(TApp _ _) #:as t (app->list t)]
        [(TFun p1 t2) `(φ ,(patt p1) ,@(app->list t2))]
        [(TMac p1 t2) `(μ ,(patt p1) ,@(app->list t2))]
        [(TVar x1) (α-restore x1)]
        [(TCon δ1) δ1]
        [TUni '◊]))
    (define patt
      (function
        [(PSeq _ _) #:as p `($ ,@(seq->list p))]
        [(PApp _ _) #:as p (app->list p)]
        [(PVar x1) (α-restore x1)]
        [(PCon δ1) δ1]
        [PWil '_]
        [PUni '◊]))
    (define fun->list
      (function
        [(TFun p1 t2) `([,(patt p1) ,@(app->list t2)])]
        [(TSeq t1 t2) (append (fun->list t1) (fun->list t2))]))
    (define mac->list
      (function
        [(TMac p1 t2) `([,(patt p1) ,@(app->list t2)])]
        [(TSeq t1 t2) (append (mac->list t1) (mac->list t2))]))
    (term a))
}|

@; -----------------------------------------------------------------------------

@subsection[#:tag "tut:ext:hash-lang"]{Putting It Back Together}

The extended interpreter is nearly finished. Before we can use it, it needs two
more things.

First, we need to combine our new @id[parse] and @id[show] @tech{functions} with
the core @id[interpret] @tech{function} in a new @id[algebraic] form.

@algebraic-code|{
  (define-syntax algebraic (μ t (show (interpret (parse 't)))))
}|

Second, we need a @racketidfont{reader} sub-module enabling
@hash-lang[algebraic/model/ext] as a @gtech{module language} with the default
s-expression @gtech{reader}.

@algebraic-code{
  (module reader syntax/module-reader
    algebraic/model/ext)
}

And that completes the extended interpreter. Let's kick the tires.

@; =============================================================================

@section[#:tag "tut:ext:examples"]{Examples}

The extended syntax may be more compact and easier to read than the core syntax,
but is it any easier to write?

@; -----------------------------------------------------------------------------

@subsection[#:tag "tut:ext:example:numbers"]{Numbers}

@tabular[
  #:style full-width
  #:column-properties '(center)
  @list[
    @list[
      @abs[
        @list{add = φ}
        [@${a}       @Zero        @${a}]
        [@${a} @list{@Succ @${b}} @list{@Succ add @${a} @${b}}]
      ]
      @abs[
        @list{mul = φ}
        [@${a}       @Zero        @Zero]
        [@${a} @list{@Succ @${b}} @list{add @${a} mul @${a} @${b}}]
      ]
    ]
  ]
]

This may look a little jarring at first. Where did @emph{all} the inner
parentheses go? With right-associative applications, the pattern (@${a} @Succ
@${b}) is equivalent to (@${a} (@Succ @${b})) and the term (@Succ add @${a}
@${b}) is equivalent to (@Succ (add @${a} @${b})).

It's pretty easy to accept that (add @${a} @${b}) is the argument to @Succ
because there's only one argument in the argument list. But what about the
corresponding term for @id[mul]? If applications were left associative, the term
(add @${a} mul @${a} @${b}) would have to be re-written as (add @${a} (mul @${a}
@${b})).

@Succ took only one argument, but @id[add] takes two. The parser knows that the
second argument to @id[add] is (@id[mul] @${a} @${b}) and not @id[mul] or
(@id[mul] @${a}) because @emph{the tail of the argument list is the final
argument}. All arguments with sub-terms except the last must be parenthesized.

Let's slow down a little and figure out what's really going on. Say we are
evaluating the term:

@centered{add @Zero mul @Zero @Zero}

Before the ``add'' function takes control, its argument list (@Zero mul @Zero
@Zero) is evaluated in the following manner:

@itemlist[
  #:style 'ordered

  @item{@Zero is already a value---a constructor. Evaluation continues
  @emph{down} the rest of the argument list:

  @centered{(mul @Zero @Zero)}}

  @item{The name ``mul'' reduces to an anonymous function. The argument list is
  now:

  @centered{((φ mul ...) @Zero @Zero)}}

  @item{The trailing @Zeros are already values. Every element of the argument
  list is now known to be a value. Evaluation continues back @emph{up} the
  argument list, which hasn't changed:

  @centered{((φ mul ...) @Zero @Zero)}}

  @item{((φ mul ...) @Zero @Zero) reduces to @Zero. Evaluation continues back
  @emph{up} the argument list:

  @centered{(@Zero @Zero)}}

  @item{The argument list (@Zero @Zero) is now fully evaluated.}

]

This leaves (add @Zero @Zero) as the next step of the computation.

To state the obvious, the tail of a list is also a list, and a list with a
function at its head is a function application. As evaluation focuses down the
tail of the argument list, any argument (except the last) that is a function
will be applied to the remaining arguments.

Note that extended sequences do not have this property. Every term with
sub-terms in a sequence must be parenthesized.

Encoding Peano arithmetic in the extended calculus is as easy as 1 + 2 = 3

@ext-example[
  (letrec ([add fun [(a Zero) a] [(a Succ b) Succ add a b]])
    add (Succ Zero) Succ Succ Zero)
]

or 2 × 3 = 6.

@ext-example[
  (letrec ([add fun [(a Zero)    a] [(a Succ b)  Succ add a b]]
           [mul fun [(a Zero) Zero] [(a Succ b) add a mul a b]])
    mul (Succ Succ Zero) (Succ Succ Succ Zero))
]

These definitions are a lot more realistic. They almost look like real
@algebraic-mod programs.

@; -----------------------------------------------------------------------------

@subsection[#:tag "tut:ext:example:booleans"]{Booleans}

@tabular[
  #:style full-width
  #:column-properties '(center)
  @list[
    @list[
      @abs[
        @list{not = φ}
        [@False @True ]
        ["_"    @False]
      ]
      @tabular[
        @list[
          @list[
            @abs[
              @list{and = μ(@${a} @${b}).φ}
              [@False @False]
              ["_"    @${b} ]
            ] @${ a}
          ]
        ]
      ]
    ]
  ]
]

@tabular[
  #:style full-width
  #:column-properties '(center)
  @list[
    @list[
      @tabular[
        @list[
          @list[
            @abs[
              @list{or = μ(@${a} @${b}).φ}
              [@False @${b}]
              [@${x}  @${x}]
            ] @${ a}
          ]
        ]
      ]
      @tabular[
        @list[
          @list[
            @abs[
              @list{xor = μ(@${a} @${b}).φ}
              [@False @${b}]
              [@${x}  @list{and (not @${b}) @${x}}]
            ] @${ a}
          ]
        ]
      ]
    ]
  ]
]

There isn't much to say about Booleans in the extended model.

@ext-example[
  (letrec ([not fun [False True] [_ False]]
           [and μ (a b) (fun [False False] [_ b]            ) a]
           [or  μ (a b) (fun [False     b] [x x]            ) a]
           [xor μ (a b) (fun [False     b] [x and (not b) x]) a])
    or (not True) and (xor True True) True)
]

The extended surface syntax is definitely easier to read, if only because the
definitions come first and aren't nested.

@; -----------------------------------------------------------------------------

@subsection[#:tag "tut:ext:example:lists"]{Lists}

@tabular[
  #:style full-width
  #:column-properties '(center)
  @list[
    @list[
      @abs[
        @list{list = μ}
        [@${x} "◊" @list{@Cons(@${x};@Nil)}]
        [@${x} @${xs} @list{@Cons(@${x};list @${xs})}]
      ]
    ]
  ]
]

The extended notation visually folds our ``list'' function definition on top of
itself. When we align patterns and terms vertically like this, the structural
similarities and differences across clauses stand out.

@ext-example[
  (letrec ([list mac [(x  ◊) Cons $ x       Nil]
                     [(x xs) Cons $ x (list xs)]])
    list (Succ Zero) (Succ Succ Zero) (Succ Succ Succ Zero) ◊)
]

If we recall the original result:

@core-example[
  ((φ fix
       ((φ list
          (list ((Succ Zero)
                 ((Succ (Succ Zero))
                  ((Succ (Succ (Succ Zero)))
                   ◊)))))
        (fix (φ list ($ (μ (x ◊) (Cons ($ x Nil)))
                        (μ (x xs) (Cons ($ x (list xs)))))))))
     (φ f
       ((φ x (f (φ y ((x x) y))))
        (φ x (f (φ y ((x x) y)))))))
]

We're 6 parentheses lighter. That's not bad, but not quite as dramatic as what
we squeezed out of our code. Can we do better with our data, too? The next
tutorial, @secref{tut:host}, will address this question.

@tabular[
  #:style full-width
  #:column-properties '(center)
  #:cell-properties '((vcenter))
  @list[
    @list[
      @tabular[
        @list[
          @list[
            @abs[
              @list{reverse = φ@${xs}.letrec @list[LP]@${rev} φ}
              [           @Nil            @${a} @${a}]
              [@list{@Cons(@${y};@${ys})} @${a} @list{@${rev} @${ys} @Cons(@${y};@${a})}]
            ]
            @list{@RP @${rev} @${xs} @Nil}
          ]
        ]
      ]
    ]
  ]
]

@ext-example[
  (letrec ([list mac [(x ◊) Cons $ x Nil] [(x xs) Cons $ x (list xs)]]
           [reverse φ xs
                    letrec ([rev fun
                                 [(Nil a) a]
                                 [((Cons ($ y ys)) a) rev ys Cons $ y a]])
                    rev xs Nil])
    reverse list (Succ Zero) (Succ Succ Zero) (Succ Succ Succ Zero) ◊)
]

Our model of the ``reverse'' function looks nice, but this time even the code is
resisting simplification. It should be clear by now that we have more work to
do.

@; =============================================================================

@section[#:tag "tut:ext:closing"]{Closing Remarks}

The nice thing about syntax extensions is they require less work. The extended
interpreter is half the size of the core and there's still plenty of room to
explore the minutiae of our design. In the @seclink["tut:host"]{next tutorial},
we'll start borrowing features from the host platform (i.e. Racket) to make the
code faster and the data easier to read.

@; @tabular[
@;   #:style full-width
@;   #:column-properties '(center)
@;   @list[
@;     @list[
@;       @abs[
@;         @list{append = φ}
@;         [       @Nil                @${ys} @${ys}]
@;         [@list{@Cons(@${x};@${xs})} @${ys} @list{@Cons(@${x};append @${xs} @${ys})}]
@;       ]
@;     ]
@;   ]
@; ]

@; @ext-example[
@;   (letrec ([list mac [(x ◊) Cons $ x Nil] [(x xs) Cons $ x (list xs)]]
@;            [append fun [(Nil ys) ys] [((Cons $ x xs) ys) Cons $ x (append xs ys)]])
@;     (append (list (Succ Zero) (Succ Succ Zero) ◊)
@;             list (Succ Succ Succ Zero) (Succ Succ Succ Succ Zero) ◊))
@; ]

@; @tabular[
@;   #:style full-width
@;   #:column-properties '(center)
@;   @list[
@;     @list[
@;       @abs[
@;         @list{map = φ}
@;         [ "_"             @Nil            @Nil]
@;         [@${f} @list{@Cons(@${x};@${xs})} @list{@Cons(@${f} @${x};map @${f} @${xs})}]
@;       ]
@;     ]
@;   ]
@; ]

@; @ext-example[
@;   (letrec ([list mac [(x ◊) Cons $ x Nil] [(x xs) Cons $ x (list xs)]]
@;            [map fun [(_ Nil) Nil] [(f Cons $ x xs) Cons $ (f x) (map f xs)]])
@;     map Succ list (Succ Succ Succ Zero) (Succ Succ Zero) (Succ Zero) ◊)
@; ]
