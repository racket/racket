;;;
;;; <mime-util.rkt> ---- Extra utilities
;;; Time-stamp: <01/05/07 17:41:12 solsona>
;;;
;;; Copyright (C) 2001 by Francisco Solsona.
;;;
;;; This file is part of mime-plt.

;;; mime-plt is free software; you can redistribute it and/or
;;; modify it under the terms of the GNU Lesser General Public
;;; License as published by the Free Software Foundation; either
;;; version 2.1 of the License, or (at your option) any later version.

;;; mime-plt is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;;; Lesser General Public License for more details.

;;; You should have received a copy of the GNU Lesser General Public
;;; License along with mime-plt; if not, write to the Free Software
;;; Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
;;; 02110-1301 USA.

;;; Author: Francisco Solsona <solsona@acm.org>
;;
;;
;; Commentary:

#lang racket/base

(provide string-tokenizer
         trim-all-spaces
         trim-spaces
         trim-comments
         lowercase
         warning
         cat)

;; string-index returns the leftmost index in string s
;; that has character c
(define (string-index s c)
  (let ([n (string-length s)])
    (let loop ([i 0])
      (cond [(>= i n) #f]
            [(char=? (string-ref s i) c) i]
            [else (loop (+ i 1))]))))

;; string-tokenizer breaks string s into substrings separated by character c
(define (string-tokenizer c s)
  (let loop ([s s])
    (if (string=? s "") '()
        (let ([i (string-index s c)])
          (if i (cons (substring s 0 i)
                      (loop (substring s (+ i 1) (string-length s))))
              (list s))))))

;; Trim all spaces, except those in quoted strings.
(define (trim-all-spaces str)
  ;; Break out alternate quoted and unquoted parts.
  ;; Initial and final string are unquoted.
  (let-values ([(unquoted quoted)
                (let loop ([str str] [unquoted null] [quoted null])
                  (let ([m (regexp-match-positions #rx"\"" str)])
                    (if m
                      (let ([prefix (substring str 0 (caar m))]
                            [rest (substring str (add1 (caar m)) (string-length str))])
                        ;; Find closing quote
                        (let ([m (regexp-match-positions #rx"\"" rest)])
                          (if m
                            (let ([inside (substring rest 0 (caar m))]
                                  [rest (substring rest (add1 (caar m)) (string-length rest))])
                              (loop rest (cons prefix unquoted) (cons (format "\"~a\"" inside) quoted)))
                            ;; No closing quote!
                            (loop "" (cons prefix unquoted) (cons (format "\"~a" rest) quoted)))))
                      (values (reverse (cons str unquoted)) (reverse quoted)))))])
    ;; Put the pieces back together, stripping spaces for unquoted parts:
    (apply
     string-append
     (let loop ([unquoted unquoted] [quoted quoted])
       (let ([clean (regexp-replace* #rx"[ \t\n\r\v]" (car unquoted) "")])
         (if (null? quoted)
           (list clean)
           (list* clean
                  (car quoted)
                  (loop (cdr unquoted) (cdr quoted)))))))))

;; Only trims left and right spaces:
(define (trim-spaces str)
  (regexp-replace #rx"[ \t\r\n\v]+$"
                  (regexp-replace #rx"^[ \t\r\n\v]+" str "")
                  ""))

(define (trim-comments str)
  (define positions
    (regexp-match-positions #rx"^[^\"]*(\"[^\"]*\")*[^\"]*(\\(.*\\))" str))
  (if positions
    (string-append (substring str 0 (caaddr positions))
                   (substring str (cdaddr positions) (string-length str)))
    str))

(define lowercase string-downcase)

(define warning
  (lambda (msg . args)
    (log-warning (apply format (cons msg args)))))

;; Copies its input `in' to its ouput port if given, it uses
;; current-output-port if out is not provided.
(define (cat in [out (current-output-port)])
  (let loop ([ln (read-line in)])
    (unless (eof-object? ln)
      (fprintf out "~a\n" ln)
      (loop (read-line in)))))

;;; mime-util.rkt ends here
