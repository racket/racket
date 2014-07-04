#lang racket/base
(require racket/cmdline
         pkg/lib)

;; This module is copied to the virtual machine to extract
;; a package -> documentation mapping.

(define all-pkgs? #f)

(define want-pkgs
  (command-line
   #:once-each
   [("--all") "All packages"
    (set! all-pkgs? #t)]
   #:args
   want-pkg
   want-pkg))

(define ns (make-base-namespace))

(define ht
  (for/hash ([pkg (in-list
                   (if all-pkgs?
                       (installed-pkg-names #:scope 'installation)
                       want-pkgs))])
    (define dir (pkg-directory pkg))
    (values pkg
            (pkg-directory->additional-installs dir pkg #:namespace ns))))

(write ht)
(newline)
