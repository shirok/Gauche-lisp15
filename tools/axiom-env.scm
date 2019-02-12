;;
;; A helper tool to convert definitions in eval.mx into an assoc list.
;;

;; Usage: gosh tools/axiom-env examples/eval.mx

(add-load-path ".." :relative)

(define-syntax DEFINE
  (syntax-rules ()
    [(_ ((var expr) ...))
     (begin (pprint '((var . expr) ...))
            (undefined))]))

(define (usage)
  (print "Usage: gosh tools/axiom-env MEXPR-SOURCE")
  (print "  Read MEXPR-SOURCE and writes out the definitions in an assoc list")
  (print "  that can be passed to EVAL as an environment."))

(define (main args)
  (apply (case-lambda
           [(file) (load file :paths '(".")) 0]
           [_ (usage) 1])
         (cdr args)))
