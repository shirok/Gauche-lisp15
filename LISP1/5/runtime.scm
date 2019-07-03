;;;
;;;  LISP1.5.runtime
;;;

;; This module adds enough support to run LISP1.5 evaluator shown in
;; Appendix B.

(define-module LISP1.5.runtime
  (export $TOPLEVELS
          CAR CDR CONS ATOM EQ QUOTE COND CALLSUBR
          T F NIL ERROR
          $scheme->lisp $lisp->scheme)
  )
(select-module LISP1.5.runtime)

;;
;; LISP symbols
;;
;;  LISP symbols are Gauche pairs whose car is 'ATOM.  Their cdr is a
;;  property list.

(define *PNAME* '#0=(ATOM #0# "PNAME"))
(define *APVAL* `(ATOM ,*PNAME* "APVAL"))
(define *SUBR*  `(ATOM ,*PNAME* "SUBR"))
(define *NIL* (rlet1 nil (list 'ATOM *PNAME* "NIL" *APVAL*)
                (set! (cddddr nil) (list nil))))

(define *F* `(ATOM ,*PNAME* "F" ,*APVAL* *NIL*))
(define *T* (rlet1 t (list 'ATOM *PNAME* "T" *APVAL*)
              (set! (cddddr t) (list t))))

;;
;; Helper functions
;;

(define ($atom? obj) (and (pair? obj) (eq? (car obj) 'ATOM)))
(define ($cons? obj) (and (pair? obj) (not (eq? (car obj) 'ATOM))))

(define ($lisp->scheme obj)
  (define (rec obj)
    (cond [(eq? obj *NIL*) '()]
          [($atom? obj) (string->symbol (cadr (member *PNAME* (cdr obj))))]
          [(pair? obj) (cons (rec (car obj)) (rec (cdr obj)))]
          [else (format "#[~s]" obj)]))
  (if (eq? obj *NIL*)
    'NIL
    (rec obj)))

(define *obtable* (hash-table-r7 eq-comparator
                                 'NIL *NIL*
                                 'PNAME *PNAME*
                                 'APVAL *APVAL*
                                 'SUBR  *SUBR*
                                 'F *F*
                                 'T *T*))

(define ($scheme->lisp obj)
  (cond [(null? obj) *NIL*]
        [(symbol? obj) (or (hash-table-get *obtable* obj #f)
                           (rlet1 s (list 'ATOM *PNAME* (symbol->string obj))
                             (hash-table-put! *obtable* obj s)))]
        [(pair? obj) (cons ($scheme->lisp (car obj))
                           ($scheme->lisp (cdr obj)))]
        [else (errorf "Cannot convert ~s to LISP" obj)]))

;;;
;;; The "basement"---primitives that are used to run eval in the ground floor
;;;

;; We don't check whether the argument is an atom--thus we allow them
;; to go through symbol's property list.
(define (CAR x) (if (null? (car x)) *NIL* (car x)))
(define (CDR x) (if (null? (cdr x)) *NIL* (cdr x)))
(define (CONS x y) (cons x (if (eq? y *NIL*) '() y)))
(define (ATOM x) (if ($atom? x) *T* *F*))
(define (EQ x y) (if (eq? x y) *T* *F*))

(define-syntax QUOTE
  (syntax-rules ()
    [(_ x) ($scheme->lisp 'x)]))
(define-syntax COND
  (syntax-rules (=> LAMBDA)
    [(_) *NIL*]
    [(_ (test expr) . more)
     (let ([t test])
       (if (or (eq? t *NIL*) (eq? t *F*))
         (COND . more)
         expr))]
    [(_ (test => (LAMBDA (var) expr)) . more)          ; extension
     (let ([t test])
       (if (or (eq? t *NIL*) (eq? t *F*))
         (COND . more)
         (let ([var t])
           expr)))]))

(define (CALLSUBR subr args) (apply subr args))
(define T *T*)
(define F *T*)
(define NIL *NIL*)


(define (ERROR obj) (error "Meta*LISP Error:" ($lisp->scheme obj)))

(define-syntax $TOPLEVELS
  (syntax-rules ($=)
    [(_ ($= (name args ...) expr) ...)
     (begin (define name
              (let ([lsym ($scheme->lisp 'name)]
                    [lfn ($scheme->lisp '(LAMBDA (args ...) expr))])
                (set! (cdr lsym) `(,($scheme->lisp 'EXPR) ,lfn ,@(cdr lsym)))
                (lambda (args ...) expr)))
            ...)]))

(define $lisp-eval
  (let1 promis (delay (get-keyword *SUBR* ($scheme->lisp 'EVAL)))
    (lambda () (force promis))))

(define ($cond args env)
  (if (null? args)
    *NIL*
    (let ([test (caar args)]
          [expr (cadar args)])
      (let1 val (($lisp-eval) test env)
        (if (or (eq? val *F*) (eq? val *NIL*))
          ($cond (cdr args) env)
          (($lisp-eval) expr env))))))

;;;
;;; The "ground floor"---these are used 
;;;

(define-syntax defattr
  (syntax-rules ()
    [(_ var key val)
     (let1 lsym ($scheme->lisp 'var)
       (set! (cdr lsym) `(,($scheme->lisp key) ,val ,@(cdr lsym))))]))

(defattr CAR 'SUBR CAR)
(defattr CDR 'SUBR CDR)
(defattr CONS 'SUBR CONS)
(defattr ATOM 'SUBR ATOM)
(defattr EQ 'SUBR EQ)
(defattr QUOTE 'FSUBR (lambda (args env) (car args)))
(defattr COND 'FSUBR $cond)
(defattr ERROR 'SUBR ERROR)
(defattr CALLSUBR 'SUBR CALLSUBR)
