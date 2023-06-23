#lang racket/base
(require racket/cmdline
         (submod compiler/private/collects-path set-executable-tag))

;; This module is executed by the install process to update
;;  the embedded path to "collects" and "lib" in an executable.

(command-line
 #:args (srcdir dest dir-path config-path)

 (define (fix-one label desc path-in)
   (set-executable-tag 'set-collects-path label desc dest #f path-in
                       (path->bytes
                        (if (string? path-in)
                            (string->path path-in)
                            path-in))))
 
 (fix-one #rx#"coLLECTs dIRECTORy:" "collects" dir-path)
 (fix-one #rx#"coNFIg dIRECTORy:" "config" config-path))
