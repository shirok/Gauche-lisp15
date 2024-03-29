;;;
;;; Test LISP1.5
;;;

(use gauche.test)
(use gauche.parameter)
(use file.util)
(test-start "LISP1.5")

(test-section "Basic modules")

(use LISP1.5.axioms)
(test-module 'LISP1.5.axioms)

(use LISP1.5.memory)
(test-module 'LISP1.5.memory)

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
        '($= (EQUAL X Y)
             (COND ((ATOM X) (COND ((ATOM Y) (EQ X Y))
                                   ((QUOTE T) (QUOTE F))))
                   ((EQUAL (CAR X) (CAR Y)) (EQUAL (CDR X) (CDR Y)))
                   ((QUOTE T) (QUOTE F)))))

(use LISP1.5)
(test-module 'LISP1.5)

(test-section "Eval")

(test* "Loading eval.mx" #t
       (load "mx/eval.mx"))
(test* "Calling APPLY" '(A B C X Y Z)
       (APPLY '#,(m-expr "label[apnd;lambda[[xs;r];\
                                 [eq[xs;NIL] -> r;\
                                  T -> cons[car[xs];apnd[cdr[xs];r]]]]]")
              '((A B C) (X Y Z))
              'NIL))
(test* "Calling EVAL" '(G F E D C B A)
       (EVAL '#,(m-expr "reverse[(A B C D E F G)]")
             '((NULL . #,(m-expr "lambda[[x];[eq[x;NIL] -> T; T -> F]]"))
               (APPEND . #,(m-expr "lambda[[xs;r];\
                                      [null[xs] -> r;\
                                       T -> cons[car[xs];append[cdr[xs];r]]]]"))
               (REVERSE . #,(m-expr "lambda[[xs];\
                                      [null[xs] -> NIL;\
                                       T -> append[reverse[cdr[xs]];cons[car[xs];NIL]]]]"))
               )))

(define-module metacircular-test1
  (use LISP1.5.axioms)
  (load "mx/eval.mx" :environment (current-module))
  (load "lisp/eval.lisp" :environment (current-module)))
(test* "Metacircular (axioms)" '(X . Y)
       (with-module metacircular-test1
         (EVAL* '#,(m-expr"eval[(CONS (QUOTE X) (QUOTE Y));NIL]"))))

(define-module metacircular-test2
  (use LISP1.5.axioms)
  (load "mx/eval.mx" :environment (current-module))
  (load "lisp/mapcar.lisp" :environment (current-module)))
(test* "Metacircular (mapcar, no function)" '((A . Y) (B . Y) (C . Y))
       (with-module metacircular-test2
         (EVAL* '(MAPCAR (QUOTE (LAMBDA (X) (CONS X (QUOTE Y))))
                         (QUOTE (A B C))))))
(test* "Metacircular (mapcar, no function, nested)"
       '((((P Q R) . P) ((Q R) . Q) ((R) . R))
         (((P Q R) . P) ((Q R) . Q) ((R) . R))
         (((P Q R) . P) ((Q R) . Q) ((R) . R)))
       (with-module metacircular-test2
         (EVAL* '(MAPCAR (QUOTE (LAMBDA (X)
                                        (MAPCAR (QUOTE (LAMBDA (Y) (CONS X Y)))
                                                (QUOTE (P Q R)))))
                         (QUOTE (A B C))))))

(define-module metacircular-test3
  (use LISP1.5.axioms)
  (load "mx/funarg.mx" :environment (current-module))
  (load "lisp/mapcar.lisp" :environment (current-module)))
(test* "Metacircular (mapcar, with function)"
       '(((A . P) (A . Q) (A . R))
         ((B . P) (B . Q) (B . R))
         ((C . P) (C . Q) (C . R)))
       (with-module metacircular-test3
         (EVAL* '(MAPCAR (FUNCTION
                          (LAMBDA (X)
                                  (MAPCAR (FUNCTION (LAMBDA (Y) (CONS X Y)))
                                          (QUOTE (P Q R)))))
                         (QUOTE (A B C))))))

;; If you don't want `gosh' to exit with nonzero status even if
;; the test fails, pass #f to :exit-on-failure.
(test-end :exit-on-failure #t)
