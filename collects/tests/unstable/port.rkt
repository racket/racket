#lang racket

(require rackunit rackunit/text-ui unstable/port "helpers.rkt")

(run-tests
 (test-suite "port.ss"
   (test-suite "read-all"
     (test-ok (check-equal? (read-all read (open-input-string "1 2 3"))
                            (list 1 2 3)))
     (test-ok (check-equal?
               (parameterize ([current-input-port
                               (open-input-string "1 2 3")])
                 (read-all))
               (list 1 2 3))))
   (test-suite "read-all-syntax"
     (test-ok (check-equal?
               (syntax->datum
                (read-all-syntax read-syntax (open-input-string "1 2 3")))
               (list 1 2 3)))
     (test-ok (check-equal?
               (syntax->datum
                (parameterize ([current-input-port
                                (open-input-string "1 2 3")])
                  (read-all-syntax)))
               (list 1 2 3))))
   
   (test-suite "port->srcloc"
     (test-ok (define port (open-input-string "\n x "))
              (port-count-lines! port)
              (check-equal? (port->srcloc port)
                            (make-srcloc 'string 1 0 1 0))
              (read port)
              (check-equal? (port->srcloc port 'here 1)
                            (make-srcloc 'here 2 2 4 1))))

   (test-suite "read-available-bytes"
     (test-ok (define-values [in out] (make-pipe))
              (check-equal? (read-available-bytes in) #"")
              (write-byte (char->integer #\c) out)
              (check-equal? (read-available-bytes in) #"c")
              (close-output-port out)
              (check-equal? (read-available-bytes in) eof)))))

