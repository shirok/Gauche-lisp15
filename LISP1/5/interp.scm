;;;
;;;  Interpret LISP1.5
;;;

;; Miminum interpreter to run "universal lisp function"

(define-module LISP1.5.interp
  (use LISP1.5.mexpr)
  (extend LISP1.5.mexpr-reader)  ;allow #!m-expr and #,(m-expr "...")
  (export CAR CDR CONS COND EQ ATOM QUOTE)
  )
(select-module LISP1.5.interp)

(define (CAR x) (if (null? (car x)) 'NIL (car x)))
(define (CDR x) (if (null? (cdr x)) 'NIL (cdr x)))
(define (CONS x y) (cons x (if (eq? y 'NIL) '() y)))
(define (ATOM x) (if (pair? x) 'F 'T))
(define (EQ a b) (if (eq? a b) 'T 'F))
(define-syntax COND
  (syntax-rules ()
    [(_) 'NIL]
    [(_ (test expr) . more)
     (let ([t test])
       (if (or (eq? t 'NIL) (eq? t 'F))
         (COND . more)
         expr))]))
(define-syntax QUOTE quote)
