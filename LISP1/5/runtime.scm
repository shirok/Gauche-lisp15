;;;
;;;  LISP1.5 Runtime
;;;

(define-module LISP1.5.runtime
  (use gauche.uvector)
  (use gauche.parameter)
  (use srfi-42)
  (export lisp-cons? lisp-symbol?
          symbol-name symbol-intern symbol-plist)
  )
(select-module LISP1.5.runtime)

;; We don't really emulate IBM70x, but define a similar memory structure
;; so that we can get the feeling of those past days.
;;
;; Main memory conists of three areas: Cells, bytes, and native objects.
;;
;; Cells are an array of 64bit word.  Each word is devided to two 32bit 
;; word, the upper portion as "Address" field and the lower portion is
;; "Decrement" field (these terms only have historical significance).
;; Each field contains index to other objects.
;;
;; The index space is divided by its higher bits.
;;
;;    00xxxx....    Index to cell array
;;    01xxxx....    Index to byte array
;;    10xxxx....    Immediate fixnum value (30bit signed int)
;;    11xxxx....    Index to native object array.
;;                  (#xffffffff is reserved)
;;
;; Native object array has Gauche objects, and we store built-in primitives
;; and flonums in it.

(define-class <memory> ()
  ((cells :init-keyword :cells)         ; u64vector
   (num-cells :init-keyword :num-cells)

   (bytes :init-keyword :bytes)         ; u8vector
   (num-bytes :init-keyword :num-bytes)

   (natives :init-keyword :natives)     ; vector, auto extended

   (freecell :init-value 0)             ; chain of free cells
   (freebyte :init-value 0)             ; beginning of free bytes

   (obtable :init-value (make-hash-table 'string=?)) ; name -> index
   ))

(define-constant *ATOM* #xffffffff)     ; marker of atomic symbol
(define-constant *NIL* 0)               ; reserve cell #0 for NIL

(define (make-memory num-cells num-bytes)
  (define cells (make-u64vector num-cells))
  (define bytes (make-u8vector num-bytes))
  (define natives (make-vector 10000 (undefined)))

  ;; Build freelist
  (dotimes [i (- num-cells 1)]
    (set! (~ cells i) (+ i 1)))
  (set! (~ cells (- num-cells 1)) *NIL*)

  (make <memory>
    :num-cells num-cells
    :cells cells
    :num-bytes num-bytes
    :bytes bytes
    :natives natives))

;; Our memory
(define the-mem (make-parameter #f))

;; Procedures with '$' takes and/or returns index to the memory

(define-constant *CELL-PAGE* #x0000_0000)
(define-constant *BYTE-PAGE* #x4000_0000)
(define-constant *FIXNUM-PAGE* #x8000_0000)
(define-constant *NATIVE-PAGE* #xc000_0000)
(define-constant *MASK* (lognot #xc000_0000))

;; index classification
(define ($index-type ind)
  (cond [(< ind *BYTE-PAGE*) 'cell]
        [(< ind *FIXNUM-PAGE*) 'bytes]
        [(< ind *NATIVE-PAGE*) 'fixnum]
        [else 'native]))

(define ($new-fixnum n)                 ;n is Scheme integer
  (logior n *FIXNUM-PAGE*))
(define ($fixnum-value ind)             ;returns Scheme integer
  (let1 v (logand ind *MASK*)
    (if (> v #x2000_0000)
      (- v #x4000_0000)
      v)))
(define ($fixnum? ind)
  (= (logand ind *MASK*) *FIXNUM-PAGE*))

;; cell handling.  cell-ind must be an index to the cell array.
(define ($cell-car cell-ind) (ash (~ (the-mem)'cells cell-ind) -32))
(define ($cell-cdr cell-ind) (logand (~ (the-mem)'cells cell-ind) #xffffffff))

(define ($cell-set-car! cell-ind val)
  (update! (~ (the-mem)'cells cell-ind) (^c (copy-bit-field c val 32 64))))
(define ($cell-set-cdr! cell-ind val)
  (update! (~ (the-mem)'cells cell-ind) (^c (copy-bit-field c val 0 32))))

(define ($new-cell ca cd)
  (rlet1 ind (~ (the-mem) 'freecell)
    (when (= ($cell-cdr ind) *ATOM*)
      (error "Cell exhausted"))
    (set! (~ (the-mem)'freecell) ($cell-cdr ind))
    (set! (~ (the-mem)'cells ind) (logior (ash ca 32) cd))))

(define ($new-atom plist)
  ($new-cell *ATOM* plist))

(define ($cell? ind)
  (= (logand ind *MASK*) *CELL-PAGE*))

;; A cell can be a pair or an atom
(define ($atom? ind)
  (and ($cell? ind)
       (= ($cell-car ind) *ATOM*)))
(define ($pair? ind)
  (and ($cell? ind)
       (not ($atom? ind))))

(define ($new-list . elts)     ;elts must be a Scheme list of LISP indexes
  (if (null? elts)
    *NIL*
    ($new-cell (car elts) (apply $new-list (cdr elts)))))

;; Bytes area is used to store variable-length strings.  In LISP1.5, strings
;; are stored as a linked list of "full word"s, where each full word is 36bit
;; work that can hold up to 6 characters.   We use byte-addressable memory
;; for the characters, and use one cell for a string header, whose car points
;; to the byte area (character array) and whose cdr holds fixnum of character
;; count.

;; Allocate num-bytes from bytes area and returns its tagged index.
(define ($new-bytes num-bytes) 
  (unless (< (+ (~ (the-mem)'freebyte) num-bytes) (~ (the-mem)'num-bytes))
    (error "Bytes exhausted."))
  (rlet1 ind (logior (~ (the-mem)'freebyte) *BYTE-PAGE*)
    (inc! (~ (the-mem)'freebyte) num-bytes)))

(define ($bytes? ind)
  (= (logand ind *MASK*) *BYTE-PAGE*))

(define ($put-bytes! ind str)       ;str is Scheme string
  (let1 start (logand ind *MASK*)
    (u8vector-copy! (~ (the-mem)'bytes) start (string->u8vector str))))

(define ($get-bytes ind len)        ;returns Scheme string
  (let1 start (logand ind *MASK*)
    (u8vector->string (u8vector-copy (~ (the-mem)'bytes) start len))))

(define ($new-string str)           ;str is Scheme string
  (let* ([len (string-size str)]
         [ind ($new-bytes len)])
    ($put-bytes! ind str)
    ($new-cell ind ($new-fixnum len))))

(define ($string? ind)
  (and ($pair? ind)
       ($bytes? ($cell-car ind))))

;; Symbol construction
(define ($init-symbol sym plist) ;plist is a Scheme list of LISP indexes
  ($cell-set-car! sym *ATOM*)
  ($cell-set-cdr! sym (apply $new-list plist)))


  

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




