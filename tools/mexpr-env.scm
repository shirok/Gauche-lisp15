;;
;; A helper tool to convert definitions M-expr source into an assoc list.
;;

(use gauche.parseopt)
(add-load-path ".." :relative)

(define (usage)
  (print "Usage: gosh tools/axiom-env [-e] MEXPR-SOURCE ...")
  (print "  Read MEXPR-SOURCE and writes out the definitions in an assoc list")
  (print "  that can be passed to EVAL as an environment.")
  (print "    With -e option, generate a definition of EVAL*, which calls EVAL")
  (print "  with the given environment.")
  (print "    If more than one MEXPR-SOURCE is given, definitions are concatenated")
  (print "  in reverse order, so if there're definitions of the same name, the latter")
  (print "  one takes precedence.")
  (exit 1))

(define *defs* '())

(define-syntax $TOPLEVELS
  (syntax-rules ($=)
    [(_ ($= (name arg ...) expr) ...)
     (begin (push! *defs* '((name . (LAMBDA (arg ...) expr)) ...))
            (undefined))]))

(define (main args)
  (let-args (cdr args) ([emit-eval* "e"]
                        [else => (^ _ (usage))]
                        . files)
    (when (null? files) (usage))
    (dolist [file files]
      (load file :paths '(".")))
    (when emit-eval* (display "($TOPLEVELS ($= (EVAL* X) (EVAL X (QUOTE "))
    (pprint (concatenate *defs*))
    (when emit-eval* (print "))))"))
    0))
