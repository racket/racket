#lang racket/base

;; Playing with input-port-append and transplant-input-port,
;; with the idea of adding the #lang line for arbitrary text
;; without screwing up the original input port's locations.
(provide prepend-lang-line)

(require racket/port)

;; prepend-lang-line: string input-port -> input-port
;; Prepends the lang line to the input port.
(define (prepend-lang-line lang-line ip)
  (define lang-ip (open-input-string lang-line))
  (port-count-lines! ip)
  (port-count-lines! lang-ip)
  (define concatenated-port (input-port-append #f lang-ip ip))
  (port-count-lines! concatenated-port)
  
  (define (get-location)
    (define-values (line column position) (port-next-location concatenated-port))
    (cond [(<= position (string-length lang-line))
           (values #f #f #f)]
          [else
           (values (and line (sub1 line))
                   column
                   (and position (- position (string-length lang-line))))]))
  
  (define transplanted-port
    (transplant-input-port concatenated-port get-location 1 #f))
  (port-count-lines! transplanted-port)
  transplanted-port)

(module* test racket/base
  (require (submod "..")
           rackunit
           syntax/parse
           (for-syntax racket/base syntax/parse))
  
  (define-syntax (check-position stx)
    (syntax-parse stx
      [(_ source-stx
          #:source source
          #:line line
          #:column column
          #:position position
          #:span span)
       (let ([syntax-loc-info (lambda (s)
                                (quasisyntax (list
                                              (make-check-location
                                               (list #,(syntax-source s)
                                                     #,(syntax-line s)
                                                     #,(syntax-column s)
                                                     #,(syntax-position s)
                                                     #,(syntax-span s))))))])
         (quasisyntax/loc stx
           (let ([stx-v source-stx])
             (with-check-info* #,(syntax-loc-info #'source)
                               (lambda ()
                                 (check-equal? (syntax-source stx-v) source)))
             (with-check-info* #,(syntax-loc-info #'line)
                               (lambda () (check-equal? (syntax-line stx-v) line)))
             (with-check-info* #,(syntax-loc-info #'column)
                               (lambda () (check-equal? (syntax-column stx-v) column)))
             (with-check-info* #,(syntax-loc-info #'position)
                               (lambda () (check-equal? (syntax-position stx-v) position)))
             (with-check-info* #,(syntax-loc-info #'span)
                               (lambda () (check-equal? (syntax-span stx-v) span))))))]))
  
  (define an-input-port
    (prepend-lang-line "#lang racket\n"
                       (open-input-string "(+ 1\n 2 three)\n")))
  
  (define the-stx
    (parameterize ([read-accept-reader #t])
      (read-syntax 'my-source an-input-port)))
  
  (syntax-parse the-stx
    [(m n l (#%mb (~and (plus ONE TWO THREE)
                        papp)))
     (check-position #'papp
                     #:source 'my-source
                     #:line 1
                     #:column 0
                     #:position 1
                     #:span 14)
     (check-position #'plus
                     #:source 'my-source
                     #:line 1
                     #:column 1
                     #:position 2
                     #:span 1)
     (check-position #'ONE
                     #:source 'my-source
                     #:line 1
                     #:column 3
                     #:position 4
                     #:span 1)
     (check-position #'TWO
                     #:source 'my-source
                     #:line 2
                     #:column 1
                     #:position 7
                     #:span 1)
     (check-position #'THREE
                     #:source 'my-source
                     #:line 2
                     #:column 3
                     #:position 9
                     #:span 5)]))
