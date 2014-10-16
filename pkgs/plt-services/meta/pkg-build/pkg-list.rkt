#lang racket/base
(require racket/cmdline
         pkg/lib)

(define scope 'installation)

(command-line
 #:once-each
 [("--user") "User scope" (set! scope 'user)])

(write (installed-pkg-names #:scope scope))

