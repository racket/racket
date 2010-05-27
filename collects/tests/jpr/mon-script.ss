#!/usr/bin/env  mzscheme
#lang scheme
;;; a Unix script but also a plain Scheme file...

(define (get-scheme-files)      ; la A-liste ((fichier nb-defs) ...)
  (map (lambda (f) (list f (nb-defs f)))
       (filter (lambda (f)
                 (and (file-exists? f) (regexp-match ".ss$" f)))
               (map path->string (directory-list)))))

(define (nb-defs f)              ; number of definitions in f
  (define (is-def? x)            ; x is a definition ?
    (and (pair? x) (equal? (car x) 'define)))
  (call-with-input-file f
    (lambda (p-in)
      (let ((x (read p-in)))     ; is f a module ?
        ;(printf "x=~s\n\n" x)
        (if (and (pair? x) (equal? (car x) 'module))   ; yes
            (length (filter is-def? (list-ref x 3)))   ; one only read is enough !
            (do ((e (read p-in) (read p-in))           ; non
                 (acc (if (is-def? x) 1 0) (if (is-def? e) (+ acc 1) acc)))
            ((eof-object? e) acc)))))))

(read-accept-reader #t)          ; for the #lang line
(printf "Current directory is :\n   ~a\n" (current-directory))
(define FILES (get-scheme-files))
(printf "It contains ~a Scheme files. " (length FILES))
(printf "Here they are, sorted by the number of definitions :\n")
(printf "~s\n" (sort FILES (lambda (L1 L2) 
                              (<= (second L1) (second L2)))))
