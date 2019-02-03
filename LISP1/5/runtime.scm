;;;
;;;  LISP1.5 Runtime
;;;

(define-module LISP1.5.runtime
  (export lisp-cons? lisp-symbol?
          symbol-name symbol-intern symbol-plist)
  )
(select-module LISP1.5.runtime)

;; LISP 1.5 uses a specially marked pair as a symbol, whose car is a list
;; beginning with -1, followed by the symbol property list.  That worked
;; -1 can't be a valid address pointing to a lisp object.  For us, we put
;; #<undef> as a marker.  Cdr of the lisp symbol is Scheme symbol, for
;; the simplicity.

(define (lisp-cons? obj) (and (pair? obj) (not (undefined? (car obj)))))
(define (lisp-symbol? obj) (and (pair? obj) (undefined? (car obj))))

;; NIL is an an end-of-list marker.  We need it before constructing Lisp list,
;; but its contentes will be filled after we set up symbol stuff.
(define NIL (list (undefined)))  ; placeholder for now.

(define (lisp-null? obj) (eq? obj NIL))

;; Scheme symbol -> lisp symbol
(define *obtable* (make-hash-table 'eq?))

;; predefined symbol--bootstrap.  Symbol PNAME has circular reference.
(define PNAME
  (rlet1 pname (list* (undefined) #f "PNAME" NIL)
    (set! (cadr pname) pname)
    (hash-table-put! *obtable* 'PNAME pname)))

(define (symbol-intern name)            ;NAME being Scheme symbol
  (or (hash-table-get *obtable* name #f)
      (rlet1 s (list* (undefined) PNAME (symbol->string name) NIL)
        (hash-table-put! *obtable* name s))))

(define (symbol-name obj)
  (assume (lisp-symbol? obj))
  (cdr obj))

(define symbol-plist
  (getter-with-setter
   (^[obj]
     (assume (lisp-symbol? obj))
     (cdr obj))
   (^[obj val]
     (assume (lisp-symbol? obj))
     (set! (cdr obj) val))))

(define (symbol-plist-push! sym kind val)
  (update! (symbol-plist sym) (^[v] (list* kind val v))))

;; Some predefined marker symbols
(define SUBR  (symbol-intern 'SUBR))
(define FSUBR (symbol-intern 'FSUBR))
(define EXPR  (symbol-intern 'EXPR))
(define FEXPR (symbol-intern 'FEXPR))
(define APVAL (symbol-intern 'APVAL))

;; now we can make NIL a proper symbol
(let ()
  (set! (cdr NIL) (list* PNAME "NIL" APVAL NIL NIL))
  (hash-table-put! *obtable* 'NIL NIL))

;; Global values of symbols are stored in its plist.
(define (csetq sym val) (symbol-plist-push! sym APVAL val))

;; Predefined constants
(define T
  (rlet1 T (symbol-intern 'T)
    (csetq T T)))

(define F
  (rlet1 F (symbol-intern 'F)
    (csetq F F)))




