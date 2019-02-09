;;;
;;; Test LISP1.5
;;;

(use gauche.test)
(use gauche.parameter)
(test-start "LISP1.5")

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

(test-m "123" ''123)
(test-m "ABC123" ''ABC123)
(test-m "blurb" 'blurb)
(test-m "list[]" '(list))
(test-m "cons[A;B]" '(cons 'A 'B))
(test-m "cons[(A . B);C]" '(cons '(A . B) 'C))
(test-m "cons[cons[A;B]; C]" '(cons (cons 'A 'B) 'C))
(test-m "car[(A . (B1 . B2))]" '(car '(A . (B1 . B2))))

(test-m "#comment\ncons[A;\nB #comment\n]" '(cons 'A 'B))

(test-m "[eq[car[x];A] -> cons[B;cdr[x]]; T -> x]"
        '(cond ((eq (car x) 'A) (cons 'B (cdr x)))
               ('T x)))

(test-m "label[ff;lambda[[x];[atom[x]->x; T->ff[car[x]]]]]"
        '(label ff (lambda (x)
                     (cond ((atom x) x)
                           ('T (ff (car x)))))))

(test-m "equal[x;y] = [atom[x] -> [atom[y] -> eq[x;y]; T -> F];\
                      equal[car[x]; car[y]] -> equal[cdr[x]; cdr[y]];\
                      T -> F]"
        '(define (equal x y)
           (cond ((atom x) (cond ((atom y) (eq x y))
                                 ('T 'F)))
                 ((equal (car x) (car y)) (equal (cdr x) (cdr y)))
                 ('T 'F))))

(use LISP1.5)
(test-module 'LISP1.5)

;; If you don't want `gosh' to exit with nonzero status even if
;; the test fails, pass #f to :exit-on-failure.
(test-end :exit-on-failure #t)




