#! /usr/bin/scheme --program

;;; fixbibtex.ss

;;; fixbibtex removes the line breaks inserted by bibtex, sometimes
;;; in the middle of tex commands or urls.

(import (chezscheme))
(unless (= (length (command-line-arguments)) 1)
  (printf "usage: fixbibtex <filename>\n")
  (exit 1))
(define fn (car (command-line-arguments)))

(let ([s (call-with-port (open-input-file fn) get-string-all)])
  (with-input-from-string s
    (lambda ()
      (with-output-to-file fn
        (lambda ()
          (define (s0 c)
            (unless (eof-object? c)
              (case c
                [(#\\) (write-char c) (s1 (read-char))]
                [(#\%) (s2 (read-char))]
                [else (write-char c) (s0 (read-char))])))
          (define (s1 c) ; seen \
            (unless (eof-object? c)
              (write-char c)
              (s0 (read-char))))
          (define (s2 c) ; seen %
            (case c
              [(#\newline) (s0 (read-char))]
              [else (write-char #\%) (s0 c)]))
          (s0 (read-char)))
        'replace))))
