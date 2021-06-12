(use srfi-42)
(use gauche.parameter)

(define-class <traced> ()
  ((orig-proc :init-keyword :orig-proc)
   (name :init-keyword :name)))

(define nesting (make-parameter 0))

(define (indent) (make-string (* (nesting) 2)))

(define-method object-apply ((t <traced>) . args)
  (print #"~(indent)Calling ~(~ t'name) with args:")
  (do-ec [: arg (index i) args]
         (begin
           (format (current-output-port) "~a~2d: " (indent) i)
           (pprint ($lisp->scheme arg) :length 6 :level 4)))
  (rlet1 r (parameterize ((nesting (+ (nesting) 1)))
             (apply (~ t'orig-proc) args))
    (display #"~(indent)Result of ~(~ t'name): ")
    (pprint ($lisp->scheme r) :length 6 :level 4)))

(define-syntax trace
  (syntax-rules ()
    [(_ X) (set! X (%make-trace X 'X))]))

(define (%make-trace proc name)
  (when (is-a? proc <traced>)
    (error "Already traced:" name))
  (make <traced> :orig-proc proc :name name))
