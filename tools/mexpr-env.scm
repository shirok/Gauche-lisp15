;;
;; A helper tool to convert definitions M-expr source into an assoc list.
;;

(use gauche.parseopt)
(add-load-path ".." :relative)

(define (usage)
  (print "Usage: gosh tools/axiom-env [-e] MEXPR-SOURCE ...")
  (print "  Read MEXPR-SOURCE and writes out the definitions in an assoc list")
  (print "  that can be passed to EVAL as an environment.")
  (print "  With -e option, generate a definition of EVAL*, which calls EVAL")
  (print "  with the given environment.")
  (exit 1))

(define *preamble* "")
(define *postamble* "")

(define (setup-evalfn)
  (set! *preamble* "(DEFINE ((EVAL* (LAMBDA (X) (EVAL X '")
  (set! *postamble* ")))))\n"))

(define-syntax DEFINE
  (syntax-rules ()
    [(_ ((var expr) ...))
     (begin 
       (display *preamble*)
       (pprint '((var . expr) ...))
       (display *postamble*)
       (undefined))]))

(define (main args)
  (let-args (cdr args) ([#f "e" => setup-evalfn]
                        [else => (^ _ (usage))]
                        . files)
    (if (null? files)
      (usage)
      (dolist [file files]
        (load file :paths '("."))))
    0))
