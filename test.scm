;;;
;;; Test LISP1.5
;;;

(use gauche.test)
(use gauche.parameter)
(test-start "LISP1.5")

(test-section "Basic modules")

(use LISP1.5.axiom)
(test-module 'LISP1.5.axiom)

(use LISP1.5.memory)
(test-module 'LISP1.5.memory)

(parameterize ((the-mem (make-memory 1000 1000)))
  (test* "memory" '(NIL (1 2 -3) PNAME T)
         ($lisp->scheme ($list *NIL* 
                               ($list ($fixnum 1)
                                      ($fixnum 2)
                                      ($fixnum -3))
                               *PNAME* *T*)))
  (test* "round-trip" '(A B (1 . -1) . C)
         ($lisp->scheme ($scheme->lisp '(A B (1 . -1) . C))))
  )

(use LISP1.5.mexpr)
(test-module 'LISP1.5.mexpr)

(define (test-m mexpr expected)
  (test* mexpr expected (parse-mexpr mexpr)))

(test-m "123" '(QUOTE 123))
(test-m "ABC123" '(QUOTE ABC123))
(test-m "blurb" 'BLURB)
(test-m "list[]" '(LIST))
(test-m "cons[A;B]" '(CONS (QUOTE A) (QUOTE B)))
(test-m "cons[(A . B);C]" '(CONS (QUOTE (A . B)) (QUOTE C)))
(test-m "cons[cons[A;B]; C]" '(CONS (CONS (QUOTE A) (QUOTE B)) (QUOTE C)))
(test-m "car[(A . (B1 . B2))]" '(CAR (QUOTE (A . (B1 . B2)))))

(test-m "#comment\ncons[A;\nB #comment\n]" '(CONS (QUOTE A) (QUOTE B)))

(test-m "[eq[car[x];A] -> cons[B;cdr[x]]; T -> x]"
        '(COND ((EQ (CAR X) (QUOTE A)) (CONS (QUOTE B) (CDR X)))
               ((QUOTE T) X)))

(test-m "label[ff;lambda[[x];[atom[x]->x; T->ff[car[x]]]]]"
        '(LABEL FF (LAMBDA (X)
                     (COND ((ATOM X) X)
                           ((QUOTE T) (FF (CAR X)))))))

(test-m "equal[x;y] = [atom[x] -> [atom[y] -> eq[x;y]; T -> F];\
                      equal[car[x]; car[y]] -> equal[cdr[x]; cdr[y]];\
                      T -> F]"
        '(:= (EQUAL X Y)
             (COND ((ATOM X) (COND ((ATOM Y) (EQ X Y))
                                   ((QUOTE T) (QUOTE F))))
                   ((EQUAL (CAR X) (CAR Y)) (EQUAL (CDR X) (CDR Y)))
                   ((QUOTE T) (QUOTE F)))))

(use LISP1.5)
(test-module 'LISP1.5)

(test-section "Eval")

(test* "Loading eval.mx" #t
       (load "examples/eval.mx"))
(test* "Calling APPLY" '(A B C X Y Z)
       (APPLY '#,(m-expr "label[apnd;lambda[[xs;r];\n\
                                 [eq[xs;NIL] -> r;\n\
                                  T -> cons[car[xs];apnd[cdr[xs];r]]]]]")
              '((A B C) (X Y Z))
              'NIL))

;; If you don't want `gosh' to exit with nonzero status even if
;; the test fails, pass #f to :exit-on-failure.
(test-end :exit-on-failure #t)




