#lang racket

;; test for pr11445

(require 2htdp/batch-io)

(with-output-to-file "batch-io2.txt"
  (lambda ()
    (display "hello")
    (display #\return)
    (display #\linefeed))
  #:exists 'replace)

(read-lines "batch-io2.txt")



