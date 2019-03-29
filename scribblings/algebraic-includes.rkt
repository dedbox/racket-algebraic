#lang racket/base

(require racket/sandbox
         scribble/core
         scribble/examples
         scribble/html-properties
         scribble/manual
         syntax/parse/define
         (for-syntax racket/base
                     racket/syntax))

(provide (all-defined-out))

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

(define-ids+ "s" Term Patt)
(define-ids+ "." Term TApp TSeq TFun TMac TVar TCon TUni)
(define-ids+ "." Patt PApp PSeq PWil PVar PCon PUni)

;;; Peano Arithmetic

(define-ids Peano Succ Zero)

(define-ids+ "s" Peano Succ Zero)
(define-ids+ "." Peano Succ Zero)

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
   (list (list (~? (tt (format "~a" 'L)) ~)
               (list ~ (format "~a" 'op) ~)
               (tt (format "~a" 'R)))
         ...)))

; -----------------------------------------------------------------------------
; algebraic eval

(define algebraic-eval
  (call-with-trusted-sandbox-configuration
   (λ ()
     (parameterize
         ([sandbox-output 'string]
          [sandbox-error-output 'string])
       (make-base-eval #:lang 'algebraic/racket/base
                       '(require (for-syntax syntax/parse)
                                 racket/function))))))

(define-syntax-rule (example expr ...)
  (examples #:eval algebraic-eval #:label #f expr ...))

(define-simple-macro (algebraic-code str ...)
  #:with stx (datum->syntax this-syntax 1)
  (typeset-code #:context #'stx
                #:keep-lang-line? #f
                "#lang algebraic/racket/base\n" str ...))

; core eval

(define core-eval
  (call-with-trusted-sandbox-configuration
   (λ ()
     (parameterize
         ([sandbox-output 'string]
          [sandbox-error-output 'string])
       (make-base-eval #:lang 'algebraic/model/core)))))

(define-simple-macro (core-code str ...)
  #:with stx (datum->syntax #f 1)
  (typeset-code #:context #'stx
                "#lang algebraic/model/core\n\n" str ...))

;; (define-syntax-rule (core-code expr ...)
;;   (examples #:eval core-eval #:label #f #:no-prompt #:no-result expr ...))

;; (define-syntax-rule (core-example expr ...)
;;   (examples #:eval core-eval #:label #f expr ...))

(define-simple-macro (core-example expr ...)
  (examples #:eval core-eval #:label #f expr ...))

; core-mod eval

(define core-mod-eval
  (call-with-trusted-sandbox-configuration
   (λ ()
     (parameterize
         ([sandbox-output 'string]
          [sandbox-error-output 'string])
       (make-base-eval #:lang 'algebraic/racket/base
                       '(require (except-in algebraic/model/core
                                            #%module-begin #%top-interaction
                                            #%datum #%app)
                                 racket/format
                                 racket/set))))))

(define-syntax-rule (core-mod-example expr ...)
  (examples #:eval core-mod-eval #:label #f expr ...))

; ext eval

(define ext-eval
  (call-with-trusted-sandbox-configuration
   (λ ()
     (parameterize
         ([sandbox-output 'string]
          [sandbox-error-output 'string])
       (make-base-eval #:lang 'algebraic/model/ext)))))

(define-syntax-rule (ext-example expr ...)
  (examples #:eval ext-eval #:label #f expr ...))

; hosted eval

(define hosted-eval
  (call-with-trusted-sandbox-configuration
   (λ ()
     (parameterize
         ([sandbox-output 'string]
          [sandbox-error-output 'string])
       (make-base-eval #:lang 'algebraic/model/hosted)))))

(define-syntax-rule (hosted-example expr ...)
  (examples #:eval hosted-eval #:label #f expr ...))

(define-syntax-rule (hosted-codeblock expr ...)
  (examples #:eval hosted-eval #:label #f #:no-prompt #:no-result expr ...))

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

(define full-width
  (make-style "fullwidth"
              (list (make-css-addition "scribblings/css/fullwidth.css"))))

(define grammar-style
  (make-style "grammar"
              (list (make-css-addition "scribblings/css/grammar.css"))))

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
