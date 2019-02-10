;;;
;;;  Interpret LISP1.5
;;;

;; Miminum interpreter to run "universal lisp function"

(define-module LISP1.5.interp
  (use LISP1.5.mexpr)
  (export (rename lisp:car car)
          (rename lisp:cdr cdr)
          (rename lisp:cons cons)
          (rename lisp:cond cond)
          eq atom
          := prog  ; these are inserted by mexpr reader
          )
  )
(select-module LISP1.5.interp)

(define (lisp:car x) (if (null? (car x)) 'NIL (car x)))
(define (lisp:cdr x) (if (null? (cdr x)) 'NIL (cdr x)))
(define (lisp:cons x y) (cons x (if (eq? y 'NIL) '() y)))
(define (atom x) (if (pair? x) 'F 'T))
(define (eq a b) (if (eq? a b) 'T 'F))
(define-syntax lisp:cond
  (syntax-rules ()
    [(_) 'NIL]
    [(_ (test expr) . more)
     (let ([t test])
       (if (or (eq? t 'NIL) (eq? t 'F))
         (lisp:cond . more)
         expr))]))
(define-syntax := define)
(define-syntax prog begin)
