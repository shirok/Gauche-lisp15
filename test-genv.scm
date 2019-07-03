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

(test-end)

