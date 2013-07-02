#lang racket/base
(require racket/cmdline
         racket/string
         (only-in "config.rkt" extract-options))

(define-values (config-file config-mode default-pkgs flags)
  (command-line
   #:args
   (config-file config-mode pkgs . flag)
   (values config-file config-mode pkgs flag)))

(define pkgs (or (hash-ref (extract-options config-file config-mode)
                           '#:pkgs
                           #f)
                 (string-split default-pkgs)))

(parameterize ([current-command-line-arguments
                (list->vector (append (list "pkg" "install")
                                      flags
                                      pkgs))])
  (dynamic-require 'raco #f))
