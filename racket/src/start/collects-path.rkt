#lang racket/base
(require racket/cmdline
         racket/system)

;; This module is executed by the install process to update
;;  the embedded path to "collects" and "lib" in an executable.

(command-line
 #:args (srcdir dest dir-path config-path)

 (define (fix-one label path-in)
   (define-values (i o) (open-input-output-file dest #:exists 'update))
   (define m (regexp-match-positions label i))
   (define path (if (string? path-in)
                    (string->path path-in)
                    path-in))
   (unless m
     (error 'set-collects-path
            "cannot find collection-path label in executable file"))
   (file-position o (cdar m))
   (write-bytes (path->bytes path) o)
   (write-byte 0 o)
   (write-byte 0 o)
   (close-input-port i)
   (close-output-port o))
 
 (fix-one #rx#"coLLECTs dIRECTORy:" dir-path)
 (fix-one #rx#"coNFIg dIRECTORy:" config-path))
