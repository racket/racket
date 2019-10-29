#lang racket/base
(require racket/place
         racket/file)

(define (go)
  (place
   pch
   (define path (make-temporary-file))
   (call-with-output-file* path #:exists 'append void)
   (for ([i (in-range 50)])
     (define c (make-custodian))
     (parameterize ([current-custodian c])
       (define fcs
         (for/list ([j (in-range 100)])
           (define fc (filesystem-change-evt path))
           (sync/timeout 0 fc)
           (if (even? j)
               (call-with-output-file* path #:exists 'append (lambda (o) (write-byte 48 o)))
               (filesystem-change-evt-cancel fc))
           fc))
       (map sync fcs))
     (custodian-shutdown-all c))
   (delete-file path)))

(module+ main
  (map place-wait (for/list ([i 4]) (go))))
