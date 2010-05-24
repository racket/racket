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

(provide/contract
 [make-byte-counting-port (any/c . -> . output-port?)])