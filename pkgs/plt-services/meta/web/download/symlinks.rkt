#lang meta/web

(require "resources.rkt")

(provide (all-defined-out))

(define (in-ftp . paths)
  (string-join (cons "/var/ftp/pub/racket" paths) "/"))

(define docs       (symlink "/www/docs"))
(define installers (symlink (in-ftp "installers")))
(define libs       (symlink (in-ftp "libs/tags") "libs"))
(define stubs      (symlink "/www/stubs"))

(define releases-dir (symlink (in-ftp "releases")))
