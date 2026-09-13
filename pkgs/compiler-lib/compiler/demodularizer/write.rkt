#lang racket/base
(require compiler/zo-marshal)

(provide write-module)

(define (write-module output-file bundle)
  (if (output-port? output-file)
       (zo-marshal-to bundle output-file)
       (call-with-output-file*
        output-file
        #:exists 'truncate/replace
        (lambda (o)
          (zo-marshal-to bundle o)))))
