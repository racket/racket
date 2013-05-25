#lang racket/base
(require racket/cmdline
         "installer-sh.rkt"
         "installer-dmg.rkt"
         "installer-exe.rkt")

(define release? #f)

(define-values (short-human-name human-name dir-name)
  (command-line
   #:once-each
   [("--release") "Create a release installer"
    (set! release? #t)]
   #:args
   (human-name dir-name)
   (values human-name
           (format "~a v~a" human-name (version))
           (if release?
               dir-name
               (format "~a-~a" dir-name (version))))))

(define installer-file
  (case (system-type)
    [(unix) (installer-sh human-name dir-name release?)]
    [(macosx) (installer-dmg human-name dir-name)]
    [(windows) (installer-exe short-human-name dir-name release?)]))

(call-with-output-file*
 (build-path "bundle" "installer.txt")
 #:exists 'truncate/replace
 (lambda (o) (fprintf o "~a\n" installer-file)))
