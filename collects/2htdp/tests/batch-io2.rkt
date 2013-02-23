#lang racket

;; test for pr11445

(require 2htdp/batch-io)

(define file "batch-io2.txt")

(with-output-to-file file
  (lambda ()
    (display "hello")
    (display #\return)
    (display #\linefeed))
  #:exists 'replace)

(require rackunit)
(check-equal? (read-lines "batch-io2.txt") '("hello"))

(when (file-exists? file)
  (delete-file file))
