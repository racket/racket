#lang racket/base

(require "core/read.rkt"
         racket/pretty
         racket/cmdline)

;; Helpful debug things for Honu
;;  * read a Honu program and view its structure as an s-expression (before enforestation)

(define (read-file file)
  (printf "Read file ~a\n" file)
  (pretty-print (with-input-from-file file
                                      (lambda () (honu-read)))))

(define (do-parse-command-line)
  (command-line
    #:program "debug"
    #:args files
    files))

(for ([file (do-parse-command-line)])
  (read-file file))
