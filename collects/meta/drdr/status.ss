#lang scheme
(define-struct event () #:prefab)
(define-struct (stdout event) (bytes) #:prefab)
(define-struct (stderr event) (bytes) #:prefab)

(define-struct status (start end command-line output-log) #:prefab)
(define-struct (timeout status) () #:prefab)
(define-struct (exit status) (code) #:prefab)

(define (status-duration s)
  (- (status-end s) (status-start s)))

(provide/contract
 [struct event ()]
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