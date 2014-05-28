#lang racket
(require racket/system
         "config.rkt"
         "archive.rkt"
         "path-utils.rkt"
         "dirstruct.rkt"
         "make-archive-lib.rkt")

(define mode (make-parameter 'single))

(init-revisions!)

(command-line #:program "make-archive"
              #:once-any
              ["--single" "Archive a single revision" (mode 'single)]
              ["--many" "Archive many revisions" (mode 'many)]
              #:args (ns)
              (local [(define n (string->number ns))]
                     (case (mode)
                       [(many)
                        (local [(define all-revisions
                                  (sort revisions >=))]
                               (for/or ([rev (in-list (list-tail all-revisions n))])
                                 (make-archive rev)))]
                       [(single)
                        (make-archive n)])))
