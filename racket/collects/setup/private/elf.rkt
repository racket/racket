#lang racket/base
(require compiler/private/elf
         setup/dirs)

(provide copy-file/install-elf-rpath
         copy-file/uninstall-elf-rpath)

(define (make-rpath)
  (bytes-append #"$ORIGIN:"
                (path->bytes (find-lib-dir))))

(define (copy-file/install-elf-rpath src dest)
  (define rpath (get-rpath src))
  (cond
   [(equal? rpath #"$ORIGIN")
    (set-rpath src dest (make-rpath))]
   [else
    (copy-file src dest)]))

(define (copy-file/uninstall-elf-rpath src dest)
  (define rpath (get-rpath src))
  (cond
   [(equal? rpath (make-rpath))
    (set-rpath src dest #"$ORIGIN")]
   [else
    (copy-file src dest)]))
