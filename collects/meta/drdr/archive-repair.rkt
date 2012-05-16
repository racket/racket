#lang racket
(require "config.rkt"
         "archive.rkt"
         "path-utils.rkt"
         "dirstruct.rkt"
         "make-archive-lib.rkt")

(init-revisions!)

(define rev
  (command-line #:program "archive-repair"
                #:args (n) (string->number n)))

(when (file-exists? (revision-archive rev))
  (archive-extract-to (revision-archive rev) (revision-dir rev) (revision-dir rev))
  (delete-file (revision-archive rev))
  (make-archive rev))
