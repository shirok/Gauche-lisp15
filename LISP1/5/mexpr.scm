;;;
;;; LISP1.5.mexpr - M-expression parser
;;;

;; NB: We make this module independent from memory and runtime, so that
;; it can be tested independently.

(define-module LISP1.5.mexpr
  (use parser.peg)
  (use util.match)
  (export parse-mexpr parse-mexprs))
(select-module LISP1.5.mexpr)

;;
;; Tokenizer
;;

;; NB: LISP1.5 mexpr doesn't have comment syntax.  We don't want to use ';',
;; for it is used as an argument delimiter.  We use '#' instead.
(define %ws ($skip-many ($or ($. #[ \t\r\n])
                             ($seq ($. #\#) ($skip-many ($. #[^\n]))))))

(define (make-word chars)
  (let1 s (list->string chars)
    (if-let1 n (string->number s)
      `(number ,n)
      (cond [(#/^[A-Z][A-Z0-9]*$/ s) `(atom ,(string->symbol s))]
            [(#/^[a-z][a-z0-9]*$/ s) `(identifier ,(string->symbol s))]
            [else (error "Invalid word: " s)]))))

(define %word ($lift make-word ($many-chars #[0-9a-zA-Z] 1)))

;; A few reserved word
(define %lambda ($seq ($."lambda") ($return 'lambda)))
(define %label  ($seq ($."label") ($return 'label)))
(define %-> ($seq ($or ($."->") ($. #\u2192)) ($return '->))) ; right arrow

(define %token
  ($between %ws
            ($or %-> %lambda %label %word ($. #[\[\]\(\).\;=]))
            %ws))

(define (tokenize input)
  (generator->lseq (peg-parser->generator %token input)))

;;
;; Parser
;;

(define (snd x y) y)
(define (tok-atom? x)       (match x [('atom x) x] [_ #f]))
(define (tok-number? x)     (match x [('number x) x] [_ #f]))
(define (tok-identifier? x) (match x [('identifier x) x] [_ #f]))

(define %atom       ($satisfy tok-atom? 'atom snd))
(define %number     ($satisfy tok-number? 'number snd))
(define %identifier ($satisfy tok-identifier? 'identifier snd))

(define %datum ($lazy ($or %atom %number %list)))

(define %list-tail
  ($lazy ($or ($seq ($. #\)) ($return '()))
              ($between ($. #\.) %datum ($. #\)))
              ($do [x %datum]
                   [y %list-tail]
                   ($return (cons x y))))))
            
(define %list
  ($seq ($. #\()
        ($or ($seq ($. #\)) ($return '()))
             ($do [x %datum]
                  [y %list-tail]
                  ($return (cons x y))))))

(define %form
  ($lazy ($or ($do [x %datum] ($return `(quote ,x)))
              %conditional
              %funcall-or-variable
              ($eos))))

(define %conditional-clause
  ($do [test %form]
       [($satisfy (cut eq? <> '->) '->)]
       [expr %form]
       ($return (list test expr))))

(define %conditional
  ($do [clauses ($between ($. #\[)
                          ($sep-by %conditional-clause ($. #\;))
                          ($. #\]))]
       ($return `(cond ,@clauses))))

(define %function ($lazy ($or %lambda-form %label-form %identifier)))

(define %lambda-form
  ($do [($satisfy (cut eq? 'lambda <>) 'lambda)]
       [($. #\[)]
       [args ($between ($. #\[)
                       ($sep-by %identifier ($. #\;))
                       ($. #\]))]
       [($. #\;)]
       [body %form]
       [($. #\])]
       ($return `(lambda ,args ,body))))

(define %label-form
  ($do [($satisfy (cut eq? 'label <>) 'label)]
       [($. #\[)]
       [id %identifier]
       [($. #\;)]
       [f %function]
       [($. #\])]
       ($return `(label ,id ,f))))

;; NB: In the era of LISP1.5, M-expr is hand-converted into S-expr,
;; and a bunch of definitions are translated into one DEFINE form; e.g.
;;
;;   <a> = <b>
;;   <c> = <d>
;;
;;  becomes:
;;
;;   (DEFINE ((<a> <b>) (<c> <d>))
;;
;; Since it's more convenient for us to process one definition at a time,
;; we translate <a> = <b> to (:= <a> <b>).

(define %funcall-or-variable
  ($do [head %function]
       [args ($optional ($between ($. #\[)
                                  ($sep-by %form ($. #\;))
                                  ($. #\])))]
       [follow ($optional ($seq ($. #\=) %form))]
       ($return (let1 pre (if args (cons head args) head)
                  (if follow
                    `(:= ,pre ,follow)
                    pre)))))

(define (parse-mexpr input)
  (values-ref (peg-run-parser %form (tokenize input)) 0))

(define (parse-mexprs input)
  (generator->lseq (peg-parser->generator %form (tokenize input))))

(define-reader-directive 'm-expr
  (^[sym port ctx] `(prog ,@(parse-mexprs port))))


