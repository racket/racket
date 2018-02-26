#lang racket/base
(require "../common/check.rkt"
         "input-port.rkt"
         "output-port.rkt"
         "flush-output.rkt"
         (submod "../print/main.rkt" internal))

(provide port-read-handler
         port-write-handler
         port-display-handler
         port-print-handler

         global-port-print-handler
         default-global-port-print-handler

         install-reader!
         installed-read-syntax
         installed-read-accept-reader
         installed-read-accept-lang)

(define/who port-read-handler
  (case-lambda
    [(i)
     (check who input-port? i)
     (let ([i (->core-input-port i)])
       (or (core-input-port-read-handler i)
           default-port-read-handler))]
    [(i h)
     (check who input-port? i)
     (check who (lambda (p)
                  (and (procedure? p)
                       (procedure-arity-includes? p 1)
                       (procedure-arity-includes? p 2)))
            #:contract "(and/c (procedure-arity-includes/c 1) (procedure-arity-includes/c 2))"
            h)
     (let ([i (->core-input-port i)])
       (set-core-input-port-read-handler! i h))]))

(define/who default-port-read-handler
  (case-lambda
    [(i)
     (check who input-port? i)
     (maybe-flush-stdout i)
     (installed-read i)]
    [(i src)
     (check who input-port? i)
     (maybe-flush-stdout i)
     (installed-read-syntax src i)]))

(define installed-read #f)
(define installed-read-syntax #f)
(define installed-read-accept-reader #f)
(define installed-read-accept-lang #f)

(define (install-reader! read read-syntax read-accept-reader read-accept-lang)
  (set! installed-read read)
  (set! installed-read-syntax read-syntax)
  (set! installed-read-accept-reader installed-read-accept-reader)
  (set! installed-read-accept-lang read-accept-lang))

;; ----------------------------------------

(define/who port-write-handler
  (case-lambda
    [(o)
     (check who output-port? o)
     (let ([o (->core-output-port o)])
       (or (core-output-port-write-handler o)
           default-port-write-handler))]
    [(o h)
     (check who output-port? o)
     (check who (procedure-arity-includes/c 2) h)
     (let ([o (->core-output-port o)])
       (set-core-output-port-write-handler! o (if (eq? h default-port-write-handler)
                                                  #f
                                                  h)))]))

(define/who (default-port-write-handler v o)
  (check who output-port? o)
  (do-write 'write v o))

(define/who port-display-handler
  (case-lambda
    [(o)
     (check who output-port? o)
     (let ([o (->core-output-port o)])
       (or (core-output-port-display-handler o)
           default-port-display-handler))]
    [(o h)
     (check who output-port? o)
     (check who (procedure-arity-includes/c 2) h)
     (let ([o (->core-output-port o)])
       (set-core-output-port-display-handler! o (if (eq? h default-port-display-handler)
                                                    #f
                                                    h)))]))

(define/who (default-port-display-handler v o)
  (check who output-port? o)
  (do-display 'display v o))

(define/who port-print-handler
  (case-lambda
    [(o)
     (check who output-port? o)
     (let ([o (->core-output-port o)])
       (or (core-output-port-print-handler o)
           default-port-print-handler))]
    [(o h)
     (check who output-port? o)
     (check who (procedure-arity-includes/c 2) h)
     (let ([o (->core-output-port o)])
       (set-core-output-port-print-handler! o (cond
                                                [(eq? h default-port-print-handler)
                                                 #f]
                                                [(procedure-arity-includes? h 3)
                                                 h]
                                                [else
                                                 (lambda (v o [w #f]) (h v o))])))]))

(define/who (default-port-print-handler v o [quote-depth 0])
  (check who output-port? o)
  (check who (lambda (d) (or (eq? d 0) (eq? d 1)))
         #:contract "(or/c 0 1)"
         quote-depth)
  ((global-port-print-handler) v o quote-depth))

(define/who (default-global-port-print-handler v o [quote-depth 0])
  (check who output-port? o)
  (check who (lambda (d) (or (eq? d 0) (eq? d 1)))
         #:contract "(or/c 0 1)"
         quote-depth)
  (do-print 'print v o quote-depth))

(define/who global-port-print-handler
  (make-parameter default-global-port-print-handler
                  (lambda (p)
                    (check who
                           (procedure-arity-includes/c 2)
                           #:contract (string-append
                                       "(or/c (->* (any/c output-port?) ((or/c 0 1)) any)\n"
                                       "      (any/c output-port? . -> . any))")
                           p)
                    (if (procedure-arity-includes? p 3)
                        p
                        (lambda (v o [quote-depth 0]) (p v o))))))

(void (install-do-global-print! global-port-print-handler
                                default-global-port-print-handler))
