;;;
;;;  Miminal primitives to run EVAL
;;;

(define-module LISP1.5.axioms
  (export CAR CDR CONS COND EQ ATOM QUOTE $TOPLEVELS))
(select-module LISP1.5.axioms)

(define (CAR x) (if (null? (car x)) 'NIL (car x)))
(define (CDR x) (if (null? (cdr x)) 'NIL (cdr x)))
(define (CONS x y) (cons x (if (eq? y 'NIL) '() y)))
(define (ATOM x) (if (pair? x) 'F 'T))
(define (EQ a b) (if (eq? a b) 'T 'F))

(define-syntax QUOTE quote)
(define-syntax COND
  (syntax-rules ()
    [(_) 'NIL]
    [(_ (test expr) . more)
     (let ([t test])
       (if (or (eq? t 'NIL) (eq? t 'F))
         (COND . more)
         expr))]))

;; $TOPLEVELS is not an axiom, but more like a directive to set up
;; the toplevel environment.  We translate 
(define-syntax $TOPLEVELS
  (syntax-rules ($=)
    [(_ ($= (name arg ...) expr) ...)
     (begin (define (name arg ...) expr) ...)]))

