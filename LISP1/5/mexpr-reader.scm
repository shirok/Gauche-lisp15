;;;
;;; LISP1.5.mexpr-reader
;;;

;; This module allows you to load sources written in M-expression.
;;
;;   (use LISP1.5.mexpr-reader)
;;   #!m-expr
;;   ... code written in M-expression ...
;;
;; NB: This module does not defines any LISP1.5 primitive syntax, except
;; the definition using '='.  To load m-expr that uses LISP1.5 syntax,
;; you want to use other modules such as LISP1.5.interp.


(define-module LISP1.5.mexpr-reader
  (use LISP1.5.mexpr)
  (export := begin))
(select-module LISP1.5.mexpr-reader)

;; This definition is for loading m-expr source---the outermost 'begin'
;; and ':=' are dealt with 'load'.

(define-syntax := define)

(define-reader-directive 'm-expr
  (^[sym port ctx]
    `(begin ,@(parse-mexprs port))))

;; To embed M-expr within Scheme, use #,(m-expr "M-expr")

(define-reader-ctor 'm-expr (^s (parse-mexpr s)))