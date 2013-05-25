; $Id: wordfreq-mzscheme.code,v 1.10 2006/06/21 15:05:34 bfulgham Exp $
;  http://shootout.alioth.debian.org/
;  wordfreq.mzscheme by Grzegorz Chrupaa
;  Updated and corrected by Brent Fulgham
;  Re-written by Matthew Flatt with some inspriation from the Python example

#lang racket

(require mzlib/list)

(define t (make-hash))

(define (register-word! s)
  (let ([s (string-downcase (bytes->string/utf-8 s))])
    (hash-set! t s (add1 (hash-ref t s (lambda () 0))))))

(let ([in (current-input-port)])
  (let loop ()
    (let ([m (regexp-match #rx#"[a-zA-Z]+" in)])
      (when m
        (register-word! (car m))
        (loop)))))

(for-each display
          (sort (hash-map
                 t
                 (lambda (word count)
                   (let ((count (number->string count)))
                     (format"~a~a ~a\n"
                            (make-string (- 7 (string-length count)) #\space)
                            count
                            word))))
                string>?))
