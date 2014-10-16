#lang racket/base
(require racket/contract/base)

(define-struct event () #:prefab)
(define-struct (stdout event) (bytes) #:prefab)
(define-struct (stderr event) (bytes) #:prefab)

(define-struct status (start end command-line output-log) #:prefab)
(define-struct (timeout status) () #:prefab)
(define-struct (exit status) (code) #:prefab)

(define (status-duration s)
  (- (status-end s) (status-start s)))

(provide/contract
 ;; Notice the event? is basically (or/c stdout? stderr?) because
 ;; event is not exposed, so the only event?s that can exist are these
 ;; two.
 [event?
  (-> any/c boolean?)]
 [struct (stdout event) ([bytes bytes?])]
 [struct (stderr event) ([bytes bytes?])]
 [struct status ([start number?]
                 [end number?]
                 [command-line (listof string?)]
                 [output-log (listof event?)])]
 [struct (exit status)
         ([start number?]
          [end number?]
          [command-line (listof string?)]
          [output-log (listof event?)]
          [code exact-integer?])]
 [struct (timeout status) 
         ([start number?]
          [end number?]
          [command-line (listof string?)]
          [output-log (listof event?)])]
 [status-duration (status? . -> . number?)])
