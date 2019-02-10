;;;
;;; LISP1.5
;;;

(define-module LISP1.5
  (use LISP1.5.runtime)
  (export prog :=)
  )
(select-module LISP1.5)

;; temporary - minimal emulation so that we can load m-expr source
(define-syntax prog begin)
(define-syntax := define)

