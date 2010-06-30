#lang racket

(define (make-byte-counting-port [name 'byte-counting-port])
  (define location 0)
  (define (write-out bs starting ending opt1 opt2)
    (define how-many-written (- ending starting))
    (set! location (+ location how-many-written))
    how-many-written)
  (define close void)
  (define (get-location)
    (values #f #f location))
  (make-output-port name always-evt write-out close 
                    #f #f #f
                    get-location))
;; Ryan: Isn't this just a reimplementation of 'open-output-nowhere'?
;;   Actually, the 'get-location' method isn't called unless 'port-count-lines!'
;;   is called first, and on a fresh port (no writes), it errors because it returns
;;   0 and a positive number is required.

(provide/contract
 [make-byte-counting-port (() (any/c) . ->* . output-port?)])
