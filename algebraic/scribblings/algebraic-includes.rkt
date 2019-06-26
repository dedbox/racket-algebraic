#lang racket/base

(require (except-in pict table)
         racket/class
         racket/draw
         racket/sandbox
         scribble/core
         scribble/examples
         scribble/html-properties
         scribble/manual
         syntax/parse/define
         (for-syntax racket/base
                     racket/syntax))

(provide (all-defined-out)
         (for-syntax (all-defined-out)
                     (all-from-out racket/base)))

(define (rtech . args)
  (apply tech #:doc '(lib "scribblings/reference/reference.scrbl") args))

(define (gtech . args)
  (apply tech #:doc '(lib "scribblings/guide/guide.scrbl") args))

(define (stech . args)
  (apply tech #:doc '(lib "syntax/scribblings/syntax.scrbl") args))

(define-simple-macro (id x)
  (racketid x))

; -----------------------------------------------------------------------------
; Syntax

(define-simple-macro (define-ids x:id ...+)
  (begin (define x (racketid x)) ...))

(define-simple-macro (define-ids+ s x:id ...+)
  #:with (xs ...) (map (λ (x) (format-id x "~a~a" x (syntax->datum #'s)))
                       (attribute x))
  (begin (define xs (list (racketid x) s)) ...))

;;; Core AST

(define-ids Term  TApp  TSeq  TFun  TMac  TVar  TCon  TUni )
(define-ids Term? TApp? TSeq? TFun? TMac? TVar? TCon? TUni?)
(define-ids Patt  PApp  PSeq  PWil  PVar  PCon  PUni )
(define-ids Patt? PApp? PSeq? PWil? PVar? PCon? PUni?)

(define-ids+ "." Term TApp TSeq TFun TMac TVar TCon TUni)
(define-ids+ "s" Term TApp TSeq TFun TMac TVar TCon TUni)
(define-ids+ "s." Term TApp TSeq TFun TMac TVar TCon TUni)
(define-ids+ "." Patt PApp PSeq PWil PVar PCon PUni)
(define-ids+ "s" Patt PApp PSeq PWil PVar PCon PUni)
(define-ids+ "s." Patt PApp PSeq PWil PVar PCon PUni)

;;; Peano Arithmetic

(define-ids Peano Succ Zero)
(define-ids+ "s" Peano Succ Zero)
(define-ids+ "." Peano Succ Zero)

;;; Booleans

(define-ids Bool False True)
(define-ids+ "." Bool False True)

;;; Lists

(define-ids List Nil Cons)
(define-ids+ "." List Nil Cons)

;;; Rule Names (small caps)

(define-simple-macro (define-sc-ids name:id ...+)
  (begin
    (define name
      (let* ([str (symbol->string 'name)])
        (list (substring str 0 1)
              (smaller (string-upcase (substring str 1 3)))
              (substring str 3))))
    ...))

(define-sc-ids App1 App2 Seq1 Seq2 AppF AppM Fun1 Fun2 Mac1 Mac2)

;;; Relations

(define-simple-macro (relation (~seq (~or (~literal ~) L) op R) ...+)
  (tabular
   #:style full-width
   #:column-properties '(right center left)
   (list (list (~? L ~) (format "~a" 'op) R) ...)))

;;; ----------------------------------------------------------------------------
;;; Evaluators

(define-syntax-rule (module-language-evaluator mod-name)
  (call-with-trusted-sandbox-configuration
   (λ ()
     (parameterize ([sandbox-output       'string]
                    [sandbox-error-output 'string])
       (make-base-eval #:lang mod-name '(void))))))

(define-syntax-rule (module-language-evaluator* mod-name)
  (call-with-trusted-sandbox-configuration
   (λ ()
     (parameterize ([sandbox-output       'string]
                    [sandbox-error-output 'string])
       (make-base-eval #:lang mod-name)))))

(define-syntax-rule (algebraic-evaluator)
  (module-language-evaluator 'algebraic/racket/base))

(begin-for-syntax
  (define-syntax-rule (algebraic-example -eval)
    (...
     (λ (stx)
       (syntax-case stx ()
         [(_ expr ...) #'(examples #:eval -eval #:label #f expr ...)]))))

  (define-syntax-rule (algebraic-example/locations -eval)
    (...
     (λ (stx)
       (syntax-case stx ()
         [(_ expr ...) #'(examples #:eval -eval #:label #f
                                   #:preserve-source-locations expr ...)])))))

; -----------------------------------------------------------------------------
; algebraic eval

(define algebraic-eval (algebraic-evaluator))
(define-syntax example (algebraic-example algebraic-eval))

(void (example #:hidden (require algebraic/macro
                                 racket/function
                                 (for-syntax syntax/parse))))

(define-simple-macro (algebraic-code str ...)
  #:with stx (datum->syntax this-syntax 1)
  (typeset-code #:context #'stx
                #:keep-lang-line? #f
                "#lang algebraic/racket/base\n" str ...))

; core eval

(define-simple-macro (core-code str ...)
  #:with stx (datum->syntax #f 1)
  (typeset-code #:context #'stx
                #:keep-lang-line? #f
                "#lang algebraic/model/core\n" str ...))

;; (define-syntax-rule (core-code expr ...)
;;   (examples #:eval core-eval #:label #f #:no-prompt #:no-result expr ...))

; ext eval

(define-simple-macro (ext-code str ...)
  #:with stx (datum->syntax #f 1)
  (typeset-code #:context #'stx
                #:keep-lang-line? #f
                "#lang algebraic/model/ext\n" str ...))

; odds and ends

(define shlang
  (seclink "hash-lang" #:doc '(lib "scribblings/guide/guide.scrbl") "#lang"))

(define shlang. (list shlang "."))
(define shlangs (list shlang "s"))

(define-syntax-rule (hash-lang mod)
  (list shlang " " (racketmodname mod)))

(define algebraic-mod (racketmodlink algebraic/racket/base "algebraic"))

(define (subsection* #:tag [tag #f] . args)
  (apply subsection #:tag tag #:style 'unnumbered args))

(define (subsubsection* #:tag [tag #f] . args)
  (apply subsubsection #:tag tag #:style '(unnumbered toc-hidden) args))

(define (inset . args)
  (nested-flow (style 'inset null) (list (paragraph plain args))))

(define full-width
  (make-style "fullwidth"
              (list (make-css-addition "scribblings/css/fullwidth.css"))))

(define grammar-style
  (make-style "grammar"
              (list (make-css-addition "scribblings/css/grammar.css"))))

(define brackets
  (make-style "brackets"
              (list (make-css-addition "scribblings/css/brackets.css"))))

(define (grammar name . rules)
  (tabular
   #:sep (hspace 1)
   #:style grammar-style
   #:column-properties '(left center left right)
   (let loop ([fst (emph name)]
              [snd "⩴"]
              [rules rules])
     (if (null? rules)
         null
         (cons (list fst snd (caar rules) (list (hspace 4) (cadar rules)))
               (loop "" "|" (cdr rules)))))))

; oversized parentheses

(define rm-font (make-object font% 55 "CMU Serif" 'roman))

(define (center-descent p)
  (struct-copy pict p
               [ascent (+ (/ (pict-height p) 2) 5.25)]
               [descent (- (/ (pict-height p) 2) 5.25)]))

(define LP
  (center-descent
   (inset/clip (scale-to-fit (text "(" rm-font) (blank 80 55)) 0 -7 0 0)))

(define RP
  (center-descent
   (inset/clip (scale-to-fit (text ")" rm-font) (blank 80 55)) 0 -7 0 0)))

;;; Extended Syntax

(define-simple-macro (abs name [p ...+ t] ...)
  (tabular
   (list (list name (tabular
                     #:style brackets
                     #:column-properties '(center)
                     (list (list p ... t) ...))))))
