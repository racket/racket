#lang racket/base
(require compiler/zo-marshal)

(provide write-module)

(define (write-module output-file bundle)
  (call-with-output-file*
   output-file
   #:exists 'truncate/replace
   (lambda (o)
     (zo-marshal-to bundle o))))
