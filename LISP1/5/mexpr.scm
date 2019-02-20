;; -*- coding:utf-8 -*-
;;;
;;; LISP1.5.mexpr - M-expression parser
;;;

;; NB: We make this module independent from memory and runtime, so that
;; it can be tested independently.

(define-module LISP1.5.mexpr
  (use parser.peg)
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
;;     ::                 ; fexpr definition (our extension)
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

(define %word ($lift make-word ($many-chars #[0-9a-zA-Z] 1)))

;; A few reserved word
(define %lambda ($seq ($or ($."lambda") ($. #\λ)) ($return 'LAMBDA)))
(define %label  ($seq ($."label") ($return 'LABEL)))
(define %-> ($seq ($or ($."->") ($. #\→)) ($return '->)))
(define %=> ($seq ($or ($."=>") ($. #\⇒)) ($return '=>)))
(define %:= ($seq ($.":=") ($return '|:=|)))
(define %:: ($seq ($."::") ($return '|::|)))
(define %=  ($seq ($. #\=) ($return '=)))

(define %token
  ($between %ws
            ($or %-> %=> %:= %:: %= %lambda %label %word ($. #[\[\]\(\).\;]))
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
              ($do [x %datum]
                   [y %list-tail]
                   ($return (cons x y))))))
            
(define %list
  ($seq ($. #\()
        ($or ($seq ($. #\)) ($return '()))
             ($do [x %datum]
                  [y %list-tail]
                  ($return (cons x y))))))

(define %form
  ($lazy ($or ($do [x %datum] ($return `(QUOTE ,x)))
              %conditional
              %funcall-or-variable
              ($eos))))

(define %conditional-clause
  ($do [test %form]
       [($satisfy (cut eq? <> '->) '->)]
       [expr %form]
       ($return (list test expr))))

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
;;   (= (FN ARG ...) EXPR)
;;
;; Also, we extend M-expr to allow defining FEXPR in the following
;; syntax:
;;
;;   fn[args;env] : expr
;;
;; It is pased as:
;;
;;   (:: (FN ARGS ENV) EXPR)
;;

(define %def ($or ($satisfy (cut eq? '= <>) '=)
                  ($satisfy (cut eq? '|::| <>) '|::|)))

(define %funcall-or-variable
  ($do [head %function]
       [args ($optional ($between ($. #\[)
                                  ($sep-by %form ($. #\;))
                                  ($. #\])))]
       [follow ($optional ($lift cons %def %form))]
       ($return (let1 pre (if args (cons head args) head)
                  (if follow
                    `(,(car follow) ,pre ,(cdr follow))
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
;;; The m-expr reder directive
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

;; In LISP1.5, toplevel definitions are done with DEFINE form, as this:
;;
;;   (DEFINE ((VAR EXPR) ...))
;;
;; So we have to collect those toplevel definitions:
;;
;;   (= (FN1 ARG ...) EXPR1)
;;   (= (FN2 ARG ...) EXPR2)
;;   (= (FN3 ARG ...) EXPR3)
;;
;; and convert them into:
;;
;;   (DEFINE (QUOTE ((FN1 (LAMBDA (ARG ...) EXPR1)) 
;;                   (FN2 (LAMBDA (ARG ...) EXPR2)) 
;;                   (FN3 (LAMBDA (ARG ...) EXPR3)))))
;;
;; This transformation is better be done in higher-level construct
;; than in the parser.
;;
;; We also collect FEXPR definitions (our extension)
;;
;;   (: (FN1 ARGS ENV) EXPR1)
;;   ...
;;
;; into:
;;
;;   (DEFLIST (QUOTE ((FN1 (LAMBDA (ARGS ENV) EXPR1))) ...)
;;            (QUOTE FEXPR))
;;
;;

(define-reader-directive 'm-expr
  (^[sym port ctx]
    (define (xlate-1 f)
      (match f
        [('= (fn arg ...) expr) `(,fn (LAMBDA ,arg ,expr))]
        [expr (error "Invalid definition: " expr)]))
    `(DEFINE (QUOTE ,(map xlate-1 (parse-mexprs port))))))
