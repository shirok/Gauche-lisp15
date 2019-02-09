;;;
;;;  LISP1.5 Memory model
;;;

(define-module LISP1.5.memory
  (use gauche.uvector)
  (use gauche.parameter)
  (use srfi-42)
  (export make-memory the-mem
          *NIL* *PNAME* *APVAL* *EXPR* *FEXPR* *SUBR* *FSUBR* *T* *F*

          $fixnum $fixnum-value $fixnum?
          $cons $car $cdr $set-car! $set-cdr! $cell? $atom? $pair?
          $null? $list $get-prop

          $symbol $symbol-plist $symbol-pname $symbol-apval

          $lisp->scheme $scheme->lisp)
  )
(select-module LISP1.5.memory)

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
(define-constant *T* 7)
(define-constant *F* 8)
(define-constant *NUM-RESERVERD-CELLS* 9)

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
(define-constant *VALUE-MASK* (logand (lognot *PAGE-MASK*) #xffff_ffff))

;;
;; Fixnum
;;

(define ($fixnum n)                 ;n is Scheme integer
  (logior (logand n *VALUE-MASK*) *FIXNUM-PAGE*))
(define ($fixnum-value ind)             ;returns Scheme integer
  (let1 v (logand ind *VALUE-MASK*)
    (if (> v #x2000_0000)
      (- v #x4000_0000)
      v)))
(define ($fixnum? ind)
  (= (logand ind *PAGE-MASK*) *FIXNUM-PAGE*))

;; cell handling.  cell-ind must be an index to the cell array.
(define ($car cell-ind) (ash (~ (the-mem)'cells cell-ind) -32))
(define ($cdr cell-ind) (logand (~ (the-mem)'cells cell-ind) #xffffffff))

(define ($set-car! cell-ind val)
  (update! (~ (the-mem)'cells cell-ind) (^c (copy-bit-field c val 32 64))))
(define ($set-cdr! cell-ind val)
  (update! (~ (the-mem)'cells cell-ind) (^c (copy-bit-field c val 0 32))))

(define ($cons ca cd)
  (rlet1 ind (~ (the-mem) 'freecell)
    (when (= ($cdr ind) *ATOM*)
      (error "Cell exhausted"))
    (set! (~ (the-mem)'freecell) ($cdr ind))
    (set! (~ (the-mem)'cells ind) (logior (ash ca 32) cd))))

(define ($cell? ind)
  (= (logand ind *PAGE-MASK*) *CELL-PAGE*))

;; A cell can be a pair or an atom
(define ($atom? ind)
  (and ($cell? ind)
       (= ($car ind) *ATOM*)))
(define ($pair? ind)
  (and ($cell? ind)
       (not ($atom? ind))))

;; let's define this here for we'll define list functions.
(define ($null? ind)
  (eqv? ind *NIL*))

(define ($list . elts)
  (if (null? elts)
    *NIL*
    ($cons (car elts) (apply $list (cdr elts)))))

(define ($get-prop plist key)
  (cond [($null? plist) *NIL*]
        [(eqv? ($car plist) key) ($car ($cdr plist))]
        [else ($get-prop ($cdr ($cdr plist)) key)]))
       
;; Bytes area is used to store variable-length strings.  In LISP1.5, strings
;; are stored as a linked list of "full word"s, where each full word is 36bit
;; work that can hold up to 6 characters.   We use byte-addressable memory
;; for the characters, and use one cell for a string header, whose car points
;; to the byte area (character array) and whose cdr holds fixnum of character
;; count.

;; Allocate num-bytes from bytes area and returns its tagged index.
;; NB: bytes and strings are "under the hood" in LISP1.5---they can't be
;; manipulated directly from LISP code.
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
    ($cons ind ($fixnum len))))

(define ($string? ind)
  (and ($pair? ind)
       ($bytes? ($car ind))))

(define ($get-string ind)
  (assume ($string? ind))
  ($get-bytes ($car ind) ($fixnum-value ($cdr ind))))

;; Symbol construction

;; Initialize a symbol of index sym with NAME (Scheme string)
;; and plist (Scheme list)
(define ($init-symbol sym name plist)
  (let1 name-str ($new-string name)
    ($set-car! sym *ATOM*)
    ($set-cdr! sym (apply $list *PNAME* name-str plist))
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
    ($init-symbol *FSUBR* "FSUBR" '())
    ($init-symbol *T*     "T"  `(,*APVAL* ,*T*))
    ($init-symbol *F*     "F"  `(,*APVAL* ,*NIL*))))

(define ($symbol name)
  (or (hash-table-get (~(the-mem)'obtable) name #f)
      (rlet1 sym ($cons *ATOM* *NIL*)
        ($init-symbol sym name '()))))

(define ($symbol-plist sym)
  ($cdr sym))

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
         (if ($null? ($cdr ind))
           (list ($lisp->scheme ($car ind)))
           (cons ($lisp->scheme ($car ind))
                 ($lisp->scheme ($cdr ind))))]
        [else "#<internal>"]))

(define ($scheme->lisp obj)
  (cond [(fixnum? obj) ($fixnum obj)]
        [(symbol? obj) ($symbol (symbol->string obj))]
        [(null? obj) *NIL*]
        [(eq? obj #t) *T*]
        [(eq? obj #f) *F*]
        [(pair? obj) ($cons ($scheme->lisp (car obj))
                            ($scheme->lisp (cdr obj)))]
        [else (error "Can't convert ~s to LISP object" obj)]))

