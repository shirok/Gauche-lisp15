;;;
;;;  LISP1.5 Memory model
;;;

(define-module LISP1.5.memory
  (use gauche.uvector)
  (use gauche.parameter)
  (use srfi-42)
  (export make-memory the-mem
          *NIL* *PNAME* *APVAL* *EXPR* *FEXPR* *SUBR* *FSUBR* *T* *F*
          *OBLIST* *OBLIST-SIZE*
          
          ;;$full-word?
          ;;$cons? $symbol? $fixnum? $flonum?
          ))
(select-module LISP1.5.memory)

;; We don't really emulate IBM70x, but define a similar memory structure
;; so that we can get the feeling of those days.

(define-constant *MEMORY-SIZE* 65536)   ;Number of words.  Data layout heavily
                                        ;depends on this, so can't be change 
                                        ;casually.
(define-constant *NATIVE-SIZE* 2048)    ;Native vector size.

(define-class <memory> ()
  (;; Main memory is 65536 32-bit words.   Lower half is used for
   ;; cons cells, upper half for full words.
   (cells :init-value (make-u32vector *MEMORY-SIZE*))

   ;; Mark bits
   (mark-bits :init-value (make-u8vector (/ *MEMORY-SIZE* 8)))

   ;; Native object vector
   (natives :init-value (make-vector *NATIVE-SIZE*))
   ))

;; Atoms are represented as cells with special tag in its CAR.
(define-constant *TAG-SYMBOL* #xffff)
(define-constant *TAG-NATIVE* #xfffe)
(define-constant *TAG-FIXNUM* #xfffd)
(define-constant *TAG-FLONUM* #xfffc)

;; The last part of full words area is reserved by the system, so that 
;; above tags would never be a valid pointer.  We use two of reserved words
;; for the anchors of freelists:
(define-constant *FREELIST-CONS* #xffff)
(define-constant *FREELIST-FULL* #xfffe)

(define-constant *FULL-WORD-BASE* #x8000)
(define-constant *FULL-WORD-CEIL* #xfffc)

;; First several cells are pre-allocated for important symbols.
(define-constant *NIL* 0)
(define-constant *PNAME* 1)
(define-constant *APVAL* 2)
(define-constant *EXPR* 3)
(define-constant *FEXPR* 4)
(define-constant *SUBR* 5)
(define-constant *FSUBR* 6)
(define-constant *T* 7)
(define-constant *F* 8)
(define-constant *NUM-RESERVERD-SYMBOLS* 9)

;; Followed by pre-allocated cells are a list of symbols, customarily
;; called oblist.  It is actually a list of lists; we hash the symbol
;; name and chains the symbol in the corresponding sublist (bucket).
;; We guarantee that the spine of oblist is allocated contiguously,
;; thus we can directly pick the bucket from the hash value.
(define-constant *OBLIST-SIZE* 211)
(define-constant *OBLIST* *NUM-RESERVERD-SYMBOLS*)
(define-constant *NUM-RESERVED-CELLS* (+ *OBLIST* *OBLIST-SIZE*))

;; Create a new memory.  Only freelists and OBLIST are initialized; the 
;; predefined symbols must be initialized in symbol.scm
(define (make-memory)
  (rlet1 mem (make <memory>)
    (do-ec (:range i *NUM-RESERVED-CELLS* *FULL-WORD-BASE*)
           (set! (~ mem'cells i) (+ i 1)))
    (set! (~ mem'cells (- *FULL-WORD-BASE* 1)) *NIL*)
    (set! (~ mem'cells *FREELIST-CONS*) *NUM-RESERVED-CELLS*)

    (do-ec (:range i *FULL-WORD-BASE* *FULL-WORD-CEIL*)
           (set! (~ mem'cells i) (+ i 1)))
    (set! (~ mem'cells (- *FULL-WORD-CEIL* 1)) *NIL*)
    (set! (~ mem'cells *FREELIST-FULL*) *FULL-WORD-BASE*)))


;; Our memory
(define the-mem (make-parameter (make-memory)))

;; Basic accessors
(define ($get-word ptr) (~ (the-mem)'cells ptr))
(define ($put-word! ptr w) (set! (~ (the-mem)'cells ptr) w))

(define ($car ptr) (logand (~ (the-mem)'cells ptr) #xffff))
(define ($cdr ptr) (ash (~ (the-mem)'cells ptr) -16))
(define ($set-car! ptr val)
  (update! (~ (the-mem)'cells ptr) (^c (copy-bit-field c val 16 32))))
(define ($set-cdr! ptr val)
  (update! (~ (the-mem)'cells ptr) (^c (copy-bit-field c val 0 16))))

;; A basic type predicates
(define ($full-word? ptr) (>= ptr *FULL-WORD-BASE*))

(define ($cell? ptr) (and (not ($full-word? ptr))
                          (not ($full-word? ($car ptr)))))
(define ($atom? ptr) (and (not ($full-word? ptr))
                          ($full-word? ($car ptr))))
  
(define ($symbol? ptr) (and (not ($full-word? ptr))
                            (eqv? ($car ptr) *TAG-SYMBOL*)))
(define ($native? ptr) (and (not ($full-word? ptr))
                            (eqv? ($car ptr) *TAG-NATIVE*)))
(define ($fixnum? ptr) (and (not ($full-word? ptr))
                            (eqv? ($car ptr) *TAG-FIXNUM*)))
(define ($flonum? ptr) (and (not ($full-word? ptr))
                            (eqv? ($car ptr) *TAG-FLONUM*)))

#|
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



(define ($cons ca cd)
  (rlet1 ind (~ (the-mem) 'freecell)
    (when (= ($cdr ind) *ATOM*)
      (error "Cell exhausted"))
    (set! (~ (the-mem)'freecell) ($cdr ind))
    (set! (~ (the-mem)'cells ind) (logior (ash ca 32) cd))))

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

|#
