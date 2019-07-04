;;;
;;;  LISP1.5.runtime
;;;

;; This module adds enough support to run LISP1.5 evaluator shown in
;; Appendix B.

(define-module LISP1.5.runtime
  (use util.match)
  (export $TOPLEVELS
          CAR CDR CONS ATOM EQ QUOTE COND CALLSUBR
          T F NIL ERROR LAMBDA
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
                (set! (cdr (last-pair nil)) `((,nil)))))

(define *F* `(ATOM ,*PNAME* "F" ,*APVAL* (,*NIL*)))
(define *T* (rlet1 t (list 'ATOM *PNAME* "T" *APVAL*)
              (set! (cdr (last-pair t)) `((,t)))))

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
          [(null? obj) '()]
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
(define (CALLSUBR subr args) (apply subr args))
(define (ERROR obj) (error "Meta*LISP Error:" ($lisp->scheme obj)))
(define T *T*)
(define F *NIL*)
(define NIL *NIL*)

(define-syntax LAMBDA lambda)
(define-syntax QUOTE
  (syntax-rules ()
    [(_ x) ($scheme->lisp 'x)]))
(define-syntax COND
  (syntax-rules (=>)
    [(_) *NIL*]
    [(_ (test expr) . more)
     (let ([t test])
       (if (or (eq? t *NIL*) (eq? t *F*))
         (COND . more)
         expr))]
    [(_ (test => proc) . more)          ; extension
     (let ([t test])
       (if (or (eq? t *NIL*) (eq? t *F*))
         (COND . more)
         (proc t)))]))

(define-syntax $TOPLEVELS
  (syntax-rules ($=)
    [(_ ($= (name args ...) expr) ...)
     (begin (define name
              (let ([lsym ($scheme->lisp 'name)]
                    [lfn ($scheme->lisp '(LAMBDA (args ...) expr))])
                (set! (cdr lsym) `(,($scheme->lisp 'EXPR) ,lfn ,@(cdr lsym)))
                (lambda (args ...) expr)))
            ...)]))

;;;
;;; The "ground floor"---these are used to evaluate the second-level code
;;;

(define-syntax defglobal
  (syntax-rules ()
    [(_ var key val)
     (let1 lsym ($scheme->lisp 'var)
       (set! (cdr lsym) `(,($scheme->lisp key) ,val ,@(cdr lsym))))]))

(defglobal CAR 'SUBR CAR)
(defglobal CDR 'SUBR CDR)
(defglobal CONS 'SUBR CONS)
(defglobal ATOM 'SUBR ATOM)
(defglobal EQ 'SUBR EQ)
(defglobal ERROR 'SUBR ERROR)
(defglobal CALLSUBR 'SUBR CALLSUBR)
