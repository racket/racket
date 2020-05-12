#lang racket/base
(require racket/cmdline)

(command-line
 #:args
 (src dest)

 (copy-file src dest #t)

 (define terminator
   (cond
     [(equal? #"\0\0\0\0chez" (call-with-input-file*
                               dest
                               (lambda (i) (read-bytes 8 i))))
      ;; Not compressed
      #"\177"]
     [else
      ;; Compressed
      #"\0"]))

 (call-with-output-file*
  dest
  #:exists 'update
  (lambda (o)
    (file-position o (file-size dest))
    (write-bytes terminator o)))

 (void))
