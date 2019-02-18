;;;
;;;  LISP1.5.runtime
;;;

;; This module adds enough support to run LISP1.5 evaluator shown in
;; Appendix B.

(define-module LISP1.5.runtime
  (export CAR CDR CONS ATOM EQ ERROR DEFINE DEFLIST COND QUOTE
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

(define ($error msg . objs)
  (apply error msg (map $lisp->scheme objs)))

;;;
;;; Primitives exposed to eval-globalenv.mx
;;;

(define (CAR x)
  (cond ;[(eq? x *NIL*) *NIL*]
        [($cons? x) (car x)]
        [else ($error "Cannot take CAR of an atom:" x)]))
(define (CDR x)
  (cond ;[(eq? x *NIL*) *NIL*]
        [($cons? x) (cdr x)]
        [else ($error "Cannot take CDR of an atom:" x)]))
(define (CONS x y) (cons x y))
(define (ATOM x) (if ($atom? x) *T* *F*))
(define (EQ x y) (if (eq? x y) *T* *F*))

(define (ERROR msg obj)
  (errorf "LISP Error: ~a:" ($lisp->scheme msg) ($lisp->scheme obj)))

;; These two are not LISP1.5 public functions, but the minimal native
;; functions, on top of which we can build other primitives in LISP.
(define ($GETPLIST x)
  (if ($atom? x)
    (cdr x)
    ($error "Atom required, but got:" x)))
(define ($PUTPLIST x)
  (if ($atom? x)
    (begin (set-cdr! x p) *NIL*)
    ($error "Atom required, but got:" x)))

(define (DEFLIST bindings key)
  (unless ($atom? key)
    ($error "Atom required, but got:" key))
  (dolist [binding bindings]
    (let ([var (car binding)]
          [val (cadr binding)])
      (unless ($atom? var)
        ($error "Atom required, but got:" var))
      (set! (cdr var) (list* key val (cdr var))))))

;; DEFINE does different things in the Basement and Ground Floor.
;; In the Basement, it translates LISP1.5 expressions into
;; Scheme definitions, as well as inserts LISP global bindings.
;; In the Ground Floor, it only deals with LISP bindings.

(define-syntax DEFINE
  (syntax-rules ()
    [(_ ((var (LAMBDA args expr)) ...))
     (begin (define var
              (let ([lsym ($scheme->lisp 'var)]
                    [lfn `(,($scheme->lisp 'LAMBDA) args expr)])
                (set! (cdr lsym) `(,($scheme->lisp 'EXPR) ,lfn ,@(cdr lsym)))
                (lambda args expr)))
            ...)]))

(define-syntax QUOTE quote)
(define-syntax COND
  (syntax-rules ()
    [(_) *NIL*]
    [(_ (test expr) . more)
     (let ([t test])
       (if (or (eq? t *NIL*) (eq? t *F*))
         (COND . more)
         expr))]))
;;;
;;; Make primitives visible from the First Floor
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
(defattr ERROR 'SUBR ERROR)


