#lang racket

(define (spin-a-while)
  (let loop ([n (random 1000000)])
    (if (zero? n)
        'done
        (loop (sub1 n)))))

(for ([i 100])
  (define f (make-temporary-file))
  (with-output-to-file f #:exists 'truncate
    (lambda () (write 1)))
  (define t1 (thread (lambda ()
                       (spin-a-while)
                       (for ([i 100])
                         (unless (string? (file->string f))
                           (error "oops"))))))
  (define t2 (thread (lambda ()
                       (spin-a-while)
                       (with-output-to-file f #:exists 'truncate
                         (lambda ()
                           (spin-a-while)
                           (display "now there's data")
                           (flush-output))))))
  (thread-wait t1)
  (thread-wait t2)
  (delete-file f))
