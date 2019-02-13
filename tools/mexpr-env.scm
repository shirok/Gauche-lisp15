;;
;; A helper tool to convert definitions M-expr source into an assoc list.
;;

;; Usage: gosh tools/mexpr-env examples/eval.mx

(add-load-path ".." :relative)

(define-syntax DEFINE
  (syntax-rules ()
    [(_ ((var expr) ...))
     (begin (pprint '((var . expr) ...))
            (undefined))]))

(define (usage)
  (print "Usage: gosh tools/axiom-env [-e] MEXPR-SOURCE ...")
  (print "  Read MEXPR-SOURCE and writes out the definitions in an assoc list")
  (print "  that can be passed to EVAL as an environment.")
  (exit 1))

(define (main args)
  (if (null? (cdr args))
    (usage)
    (dolist [file (cdr args)]
      (load file :paths '("."))))
  0)
