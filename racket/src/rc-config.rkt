#lang racket/base

(require racket/format
         racket/path)

(define config-dir-path (build-path "racket" "etc"))
(define config-rc-path (build-path config-dir-path "racketrc"))

(when (file-exists? config-rc-path)
  (call-with-input-file* config-rc-path
    (lambda (i)
      (define r (read i))
      (define xrepl?
        (equal? r
                '(when (collection-file-path "main.rkt" "xrepl"
                                             #:fail (lambda _ #f))
                   (dynamic-require 'xrepl #f))))
      (unless xrepl?
        (error 'racketrc
               (~a "Global racketrc file exists, but is mismatched.\n"
                   "  possible solution: delete the racketrc file"))))))

(unless (file-exists? config-rc-path)
  (printf "Writing ~a\n" config-rc-path)
  (call-with-output-file*
   config-rc-path
   (lambda (o)
     (write '(when (collection-file-path "main.rkt" "xrepl"
                                         #:fail (lambda _ #f))
               (dynamic-require 'xrepl #f)) o)
     (newline o))))
