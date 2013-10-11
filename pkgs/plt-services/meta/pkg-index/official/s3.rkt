#lang racket/base
(require racket/list
         "common.rkt")

(define (upload-all)
  (error 'upload-all "XXX"))
(define (upload-pkgs pkgs)
  ;; FUTURE make this more efficient
  (upload-all))

(module+ main
  (require racket/cmdline)
  (command-line
   #:program "s3"
   #:args pkgs
   (cond
     [(empty? pkgs)
      (upload-all)]
     [else
      (upload-pkgs pkgs)])))
