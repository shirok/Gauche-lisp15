;;;
;;; Test LISP1.5
;;;

(use gauche.test)
(test-start "LISP1.5")

(use LISP1.5.mexpr)
(test-module 'LISP1.5.mexpr)


(use LISP1.5)
(test-module 'LISP1.5)

;; If you don't want `gosh' to exit with nonzero status even if
;; the test fails, pass #f to :exit-on-failure.
(test-end :exit-on-failure #t)




