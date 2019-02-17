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

(define *F* `(ATOM ,*PNAME* "F" ,*APVAL* *NIL*))
(define *T* (rlet1 t (list 'ATOM *PNAME* "T" *APVAL*)
              (set! (cddddr nil) (list t))))

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
                                 'APVAL *APVAL*))

(define ($scheme->lisp obj)
  (cond [(null? obj) *NIL*]
        [(symbol? obj) (or (hash-table-get *obtable* obj #f)
                           (rlet1 s (list 'ATOM *PNAME* (symbol->string obj))
                             (hash-table-put! *obtable* obj s)))]
        [(pair? obj) (cons ($scheme->lisp (car obj))
                           ($scheme->lisp (cdr obj)))]
        [else (errorf "Cannot convert ~s to LISP" obj)]))

(define ($error msg . objs)
  (apply error msg (map $lisp->scheme objs)))

(define-syntax $define
  (syntax-rules ()
    [(_ symbol type value)
     (let1 lsym ($scheme->lisp 'symbol)
       (set! (cdr lsym)
             (append! (cdr lsym) (list ($scheme->lisp 'type) value))))]))


($define CAR SUBR
         (^x (cond [(eq? x *NIL*) *NIL*]
                   [($cons? x) (car x)]
                   [else ($error "Cannot take CAR of an atom:" x)])))
($define CDR SUBR
         (^x (cond [(eq? x *NIL*) *NIL*]
                   [($cons? x) (cdr x)]
                   [else ($error "Cannot take CDR of an atom:" x)])))
($define CONS SUBR (^[x y] (cons x y)))
($define ATOM SUBR (^x (if ($atom? x) *T* *F*)))
($define EQ SUBR (^[x y] (if (eq? x y) *T* *F*)))

;; These two are not LISP1.5 public functions, but the minimal native
;; functions, on top of which we can build other primitives in LISP.
($define $GETPLIST SUBR
         (^x (if ($atom? x)
               (cdr x)
               ($error "Atom required, but got:" x))))
($define $PUTPLIST SUBR
         (^[x p] (if ($atom? x)
                   (begin (set-cdr! x p) *NIL*)
                   ($error "Atom required, but got:" x))))

($define ERROR SUBR
         (^[msg obj]
           (errorf "LISP Error: ~a:" ($lisp->scheme msg) ($lisp->scheme obj))))

