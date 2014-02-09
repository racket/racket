#lang plt-web

(require "resources.rkt")

(provide (all-defined-out))

(define (in-ftp . paths)
  (string-join (cons "/var/ftp/pub/racket" paths) "/"))

(define docs       (symlink #:site download-site "/www/docs"))
(define installers (symlink #:site download-site (in-ftp "installers")))
(define libs       (symlink #:site download-site (in-ftp "libs/tags") "libs"))
(define stubs      (symlink #:site download-site "/www/stubs"))
