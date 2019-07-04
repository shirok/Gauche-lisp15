(use gauche.test)
(use gauche.parameter)
(use file.util)
(test-start "genv")

(test-section "LISP1.5.runtime")

(use LISP1.5.runtime)
(test-module 'LISP1.5.runtime)

(test* "Loading genv" #t (load "mx/genv.mx"))

(define-syntax evaltest
  (syntax-rules ()
    [(evaltest output input env)
     (test* (x->string input) output
            ($lisp->scheme (EVAL ($scheme->lisp input)
                                 ($scheme->lisp env))))]))

(evaltest 'A '(QUOTE A) '())
(evaltest '(X . Y) '(CONS (QUOTE X) (QUOTE Y)) '())
(evaltest 'NIL 'NIL '())
(evaltest 'NIL 'F '())
(evaltest 'T 'T '())

(evaltest 'ORANGE 'APPLE '((APPLE . ORANGE)))
(evaltest '(G F E D C B A)
          '(REVERSE (QUOTE (A B C D E F G)))
          '((REVERSE . (LAMBDA (XS)
                               (COND ((NULL XS) NIL)
                                     (T (APPEND (REVERSE (CDR XS))
                                                (CONS (CAR XS) NIL))))))))

(test-end)

