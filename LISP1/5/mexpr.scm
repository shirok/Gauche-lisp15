;; -*- coding:utf-8 -*-
;;;
;;; LISP1.5.mexpr - M-expression parser
;;;

;; NB: We make this module independent from memory and runtime, so that
;; it can be tested independently.

(define-module LISP1.5.mexpr
  (use parser.peg)
  (use parser.peg.deprecated)
  (use util.match)
  (use gauche.parameter)
  (use gauche.unicode)
  (export parse-mexpr parse-mexprs
          trace-mexpr-parser))
(select-module LISP1.5.mexpr)

;;
;; Tokenizer
;;

;; API: tokenize INPUT
;;   Returns an lseq of tokens. A token can be:
;;     (number <number>)
;;     (atom <name>)      ; atomic symbol; <name> is all uppercase
;;     (ident <name>)     ; identifier; <name> is all uppercase
;;     LAMBDA             ; λ is also recognized.
;;     LABEL
;;     ->                 ; → is also recognized.
;;     =>                 ; cond extension.  ⇒ is also recognized
;;     =                  ; definition
;;     :=                 ; used in PROG
;;     #\[
;;     #\]
;;     #\(
;;     #\)
;;     #\.
;;     #\;
;;
;;  Whitespaces and comments are consumed and discarded.

;; NB: LISP1.5 mexpr doesn't have comment syntax.  We don't want to use ';',
;; for it is used as an argument delimiter.  We use '#' instead.
(define %ws ($skip-many ($or ($. #[ \t\r\n])
                             ($seq ($. #\#) ($skip-many ($. #[^\n]))))))

(define (make-word chars)
  (let1 s (list->string chars)
    (if-let1 n (string->number s)
      `(number ,n)
      (cond [(#/^[A-Z][A-Z0-9]*$/ s) `(atom ,(string->symbol s))]
            [(#/^[a-z][a-z0-9]*$/ s) `(ident ,(string->symbol (string-upcase s)))]
            [else (error "Invalid word: " s)]))))

(define %word ($lift make-word ($many ($. #[0-9a-zA-Z]) 1)))

;; A few reserved word
(define %lambda ($seq ($or ($."lambda") ($. #\λ)) ($return 'LAMBDA)))
(define %label  ($seq ($."label") ($return 'LABEL)))
(define %-> ($seq ($or ($."->") ($. #\→)) ($return '->)))
(define %=> ($seq ($or ($."=>") ($. #\⇒)) ($return '=>)))
(define %:= ($seq ($.":=") ($return '|:=|)))
(define %=  ($seq ($. #\=) ($return '=)))

(define %token
  ($between %ws
            ($or %-> %=> %:= %= %lambda %label %word ($. #[\[\]\(\).\;]))
            %ws))

(define (tokenize input)
  (generator->lseq (peg-parser->generator %token input)))

;;
;; Parser
;;

;; API: parse-mexpr INPUT
;;   Parse single M-expr from INPUT and returns S-expr.
;; API: parse-mexprs INPUT
;;   Returns a lseq of parsed S-exprs from INPUT.
;; API: trace-mexpr-parser
;;   A parameter.  If true, parsers emits debug output.

(define (snd x y) y)
(define (tok-atom? x)       (match x [('atom x) x] [_ #f]))
(define (tok-number? x)     (match x [('number x) x] [_ #f]))
(define (tok-identifier? x) (match x [('ident x) x] [_ #f]))

(define %atom       ($satisfy tok-atom? 'atom snd))
(define %number     ($satisfy tok-number? 'number snd))
(define %identifier ($satisfy tok-identifier? 'identifier snd))

(define %datum ($lazy ($or %atom %number %list)))

(define %list-tail
  ($lazy ($or ($seq ($. #\)) ($return '()))
              ($between ($. #\.) %datum ($. #\)))
              ($lift cons %datum %list-tail))))

(define %list
  ($seq ($. #\()
        ($or ($seq ($. #\)) ($return '()))
             ($lift cons %datum %list-tail))))

(define %form
  ($lazy ($or ($lift (cut list 'QUOTE <>) %datum)
              %conditional
              %funcall-or-variable
              ($eos))))

(define %conditional-clause
  ($do [test %form]
       [arrow ($satisfy (cut memq <> '(-> =>)) '(-> =>))]
       [expr %form]
       ($return (if (eq? arrow '->)
                  (list test expr)
                  (list test '=> expr)))))

(define %conditional
  ($do [clauses ($between ($. #\[)
                          ($sep-by %conditional-clause ($. #\;))
                          ($. #\]))]
       ($return `(COND ,@clauses))))

(define %function ($lazy ($or %lambda-form %label-form %identifier)))

(define %lambda-form
  ($do [($satisfy (cut eq? 'LAMBDA <>) 'lambda)]
       [($. #\[)]
       [args ($between ($. #\[)
                       ($sep-by %identifier ($. #\;))
                       ($. #\]))]
       [($. #\;)]
       [body %form]
       [($. #\])]
       ($return `(LAMBDA ,args ,body))))

(define %label-form
  ($do [($satisfy (cut eq? 'LABEL <>) 'label)]
       [($. #\[)]
       [id %identifier]
       [($. #\;)]
       [f %function]
       [($. #\])]
       ($return `(LABEL ,id ,f))))

;; We parse the definition form
;;
;;   fn[arg;...] = expr
;;
;; as:
;;
;;   ($= (FN ARG ...) EXPR)
;;

(define %def ($satisfy (cut eq? '= <>) '=))

(define %funcall-or-variable
  ($do [head %function]
       [args ($optional ($between ($. #\[)
                                  ($sep-by %form ($. #\;))
                                  ($. #\])))]
       [follow ($optional ($seq %def %form))]
       ($return (let1 pre (if args (cons head args) head)
                  (if follow
                    `($= ,pre ,follow)
                    pre)))))

(define trace-mexpr-parser (make-parameter #f))

(define (%toplevel)
  (if (trace-mexpr-parser)
    ($debug "toplevel" %form)
    %form))

;; API
(define (parse-mexpr input)
  (values-ref (peg-run-parser (%toplevel) (tokenize input)) 0))

;; API
(define (parse-mexprs input)
  (generator->lseq (peg-parser->generator (%toplevel) (tokenize input))))

;; To embed M-expr within Scheme, use #,(m-expr "M-expr")
(define-reader-ctor 'm-expr (^s (parse-mexpr s)))

;;;
;;; The m-expr reader directive
;;;

;; This allows source file to be written in M-expression
;;
;;   (use LISP1.5.mexpr)
;;   #!m-expr
;;   ... code written in M-expression ...
;;
;; NB: This module does not defines any LISP1.5 primitive syntax.
;; To load m-expr that uses LISP1.5 syntax, you want to use other
;; modules such as LISP1.5.axiom.

;; LISP1.5 employs special treatment on toplevel forms, and it's not
;; convenient for us to deal with parsed result in the later stage.
;; (See README.adoc for more discussion).
;;
;; So, m-expr parser just collect all the toplevel forms
;; under (TOPLEVELS <expr> ...) form.  Interpretation of $TOPLEVELS form
;; depends on the later stage.

(define-reader-directive 'm-expr
  (^[sym port ctx]
    `($TOPLEVELS ,@(parse-mexprs port))))
