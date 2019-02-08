;;;
;;;  LISP1.5 Runtime
;;;

(define-module LISP1.5.runtime
  (use gauche.uvector)
  (use gauche.parameter)
  (use srfi-42)
  (export-all)                          ;for now
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

   (freecell :init-keyword :freecell)   ; chain of free cells
   (freebyte :init-value 0)             ; beginning of free bytes

   (obtable :init-value (make-hash-table 'string=?)) ; name -> index
   ))

(define-constant *ATOM* #xffffffff)     ; marker of atomic symbol

;; First several cells are reserved for important symbols.
(define-constant *NIL* 0)
(define-constant *PNAME* 1)
(define-constant *APVAL* 2)
(define-constant *EXPR* 3)
(define-constant *FEXPR* 4)
(define-constant *SUBR* 5)
(define-constant *FSUBR* 6)
(define-constant *NUM-RESERVERD-CELLS* 7)

(define (make-memory num-cells num-bytes)
  (define cells (make-u64vector num-cells))
  (define bytes (make-u8vector num-bytes))
  (define natives (make-vector 10000 (undefined)))

  ;; Build freelist
  (dotimes [i (- num-cells 1)]
    (set! (~ cells i) (+ i 1)))
  (set! (~ cells (- num-cells 1)) *NIL*)

  (rlet1 mem (make <memory>
               :num-cells num-cells
               :cells cells
               :num-bytes num-bytes
               :bytes bytes
               :freecell *NUM-RESERVERD-CELLS*
               :natives natives)
    (init-predefined-symbols mem)))

;; Our memory
(define the-mem (make-parameter #f))

;; Procedures with '$' takes and/or returns index to the memory

(define-constant *CELL-PAGE* #x0000_0000)
(define-constant *BYTE-PAGE* #x4000_0000)
(define-constant *FIXNUM-PAGE* #x8000_0000)
(define-constant *NATIVE-PAGE* #xc000_0000)
(define-constant *PAGE-MASK* #xc000_0000)
(define-constant *VALUE-MASK* (lognot *PAGE-MASK*))

;; index classification
(define ($index-type ind)
  (cond [(< ind *BYTE-PAGE*) 'cell]
        [(< ind *FIXNUM-PAGE*) 'bytes]
        [(< ind *NATIVE-PAGE*) 'fixnum]
        [else 'native]))

(define ($new-fixnum n)                 ;n is Scheme integer
  (logior n *FIXNUM-PAGE*))
(define ($fixnum-value ind)             ;returns Scheme integer
  (let1 v (logand ind *VALUE-MASK*)
    (if (> v #x2000_0000)
      (- v #x4000_0000)
      v)))
(define ($fixnum? ind)
  (= (logand ind *PAGE-MASK*) *FIXNUM-PAGE*))

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

(define ($cell? ind)
  (= (logand ind *PAGE-MASK*) *CELL-PAGE*))

;; A cell can be a pair or an atom
(define ($atom? ind)
  (and ($cell? ind)
       (= ($cell-car ind) *ATOM*)))
(define ($pair? ind)
  (and ($cell? ind)
       (not ($atom? ind))))

;; let's define this here for we'll define list functions.
(define ($null? ind)
  (eqv? ind *NIL*))

(define ($new-list . elts)     ;elts must be a Scheme list of LISP indexes
  (if (null? elts)
    *NIL*
    ($new-cell (car elts) (apply $new-list (cdr elts)))))

(define ($get-prop plist key)
  (cond [($null? plist) *NIL*]
        [(eqv? ($cell-car plist) key) ($cell-car ($cell-cdr plist))]
        [else ($get-prop ($cell-cdr ($cell-cdr plist)) key)]))
       
    

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
  (= (logand ind *PAGE-MASK*) *BYTE-PAGE*))

(define ($put-bytes! ind str)       ;str is Scheme string
  (let1 start (logand ind *VALUE-MASK*)
    (u8vector-copy! (~ (the-mem)'bytes) start (string->u8vector str))))

(define ($get-bytes ind len)        ;returns Scheme string
  (let1 start (logand ind *VALUE-MASK*)
    (u8vector->string (u8vector-copy (~ (the-mem)'bytes) start (+ start len)))))

(define ($new-string str)           ;str is Scheme string
  (let* ([len (string-size str)]
         [ind ($new-bytes len)])
    ($put-bytes! ind str)
    ($new-cell ind ($new-fixnum len))))

(define ($string? ind)
  (and ($pair? ind)
       ($bytes? ($cell-car ind))))

(define ($get-string ind)
  (assume ($string? ind))
  ($get-bytes ($cell-car ind) ($fixnum-value ($cell-cdr ind))))

;; Symbol construction

;; Initialize a symbol of index sym with NAME (Scheme string)
;; and plist (Scheme list)
(define ($init-symbol sym name plist)
  (let1 name-str ($new-string name)
    ($cell-set-car! sym *ATOM*)
    ($cell-set-cdr! sym (apply $new-list *PNAME* name-str plist))
    (hash-table-put! (~ (the-mem)'obtable) name sym)))

;; bootstrap - called from make-memory
(define (init-predefined-symbols mem)
  (parameterize ((the-mem mem))
    ($init-symbol *NIL* "NIL" `(,*APVAL* ,*NIL*))
    ($init-symbol *PNAME* "PNAME" '())
    ($init-symbol *APVAL* "APVAL" '())
    ($init-symbol *EXPR*  "EXPR" '())
    ($init-symbol *FEXPR* "FEXPR" '())
    ($init-symbol *SUBR*  "SUBR" '())
    ($init-symbol *FSUBR* "FSUBR" '())))

(define ($intern name)
  (or (hash-table-get (~(the-mem)'obtable) name #f)
      (rlet1 sym ($new-cell *ATOM* *NIL*)
        ($init-symbol sym name '()))))

(define ($symbol-plist sym)
  ($cell-cdr sym))

(define ($symbol-pname sym)             ; returns Scheme string
  ($get-string ($get-prop ($symbol-plist sym) *PNAME*)))

(define ($symbol-apval sym)
  ($get-prop ($symbol-plist sym) *APVAL*))

;;
;; For ease of debugging
;;

(define ($lisp->scheme ind)
  (cond [($fixnum? ind) ($fixnum-value ind)]
        [($atom? ind) (string->symbol ($symbol-pname ind))]
        [($cell? ind)
         (if ($null? ($cell-cdr ind))
           (list ($lisp->scheme ($cell-car ind)))
           (cons ($lisp->scheme ($cell-car ind))
                 ($lisp->scheme ($cell-cdr ind))))]
        [else "#<internal>"]))
