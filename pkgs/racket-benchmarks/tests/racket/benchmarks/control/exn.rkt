#lang racket/base
(require racket/include)

(include "config.rktl")

'----------------------------------------

'exn
(time
 (for/fold ([v #f]) ([i (in-range Q)])
   (with-handlers ([(lambda (x) #t) (lambda (x) 'caught)])
     (raise 'exn))))

'exn-deep
(time
 (let loop ([n 1000])
   (if (zero? n)
       (for/fold ([v #f]) ([i (in-range Q)])
         (with-handlers ([(lambda (x) #t) (lambda (x) 'caught)])
           (raise 'exn)))
       (with-continuation-mark
        'key 'val
        (values (loop (sub1 n)))))))

;; Runs a chain of 1000 handlers
'exn-chain
(time
 (for/fold ([v #f]) ([i (in-range (quotient Q 100))])
   (let/ec esc
     (call-with-exception-handler
      (lambda (exn) (esc 'done))
      (lambda ()
        (let loop ([n 1000])
          (if (zero? n)
              (raise 'exn)
              (call-with-exception-handler
               (lambda (exn) exn)
               (lambda ()
                 (values (loop (sub1 n))))))))))))

