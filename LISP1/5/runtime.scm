;;;
;;;  LISP1.5.runtime
;;;

;; This module adds enough support to run LISP1.5 evaluator shown in
;; Appendix B.

(define-module LISP1.5.runtime
  (export)
  )
(select-module LISP1.5.runtime)

;;
;; LISP symbols
;;
;;  LISP symbols are Gauche pairs whose car is 'ATOM.  Their cdr is a
;;  property list.

(define *PNAME* '#0=(ATOM #0# "PNAME"))
(define *APVAL* `(ATOM ,*PNAME* "APVAL"))
(define *NIL* (rlet1 nil (list 'ATOM *PNAME* "NIL" *APVAL*)
                (set! (cddddr nil) (list nil))))

;;
;; Axioms
;;

(define ($atom? obj) (and (pair? obj) (eq? (car obj) 'ATOM)))
(define ($cons? obj) (and (pair? obj) (not (eq? (car obj) 'ATOM))))

(define ($lisp->scheme obj)
  (define (rec obj)
    (cond [(eq? obj *NIL*) '()]
          [($atom? obj) (string->symbol (cadr (member *PNAME* (cdr obj))))]
          [else (cons (rec (car obj)) (rec (cdr obj)))]))
  (if (eq? obj *NIL*)
    'NIL
    (rec obj)))

