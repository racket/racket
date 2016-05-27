;;;
;;; <qp.rkt> ---- Quoted Printable Encoding/Decoding
;;;
;;; Copyright (C) 2002 by PLT.
;;; Copyright (C) 2001 by Francisco Solsona.
;;;
;;; This file is part of mime-plt.

;;; qp is free software; you can redistribute it and/or
;;; modify it under the terms of the GNU Lesser General Public
;;; License as published by the Free Software Foundation; either
;;; version 2.1 of the License, or (at your option) any later version.

;;; qp is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;;; Lesser General Public License for more details.

;;; You should have received a copy of the GNU Lesser General Public
;;; License along with qp; if not, write to the Free Software
;;; Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
;;; 02110-1301 USA.

;;; Author: Francisco Solsona <solsona@acm.org>
;;
;; Commentary:

#lang racket/base

(provide
 ;; -- exceptions raised --
 (struct-out qp-error)
 (struct-out qp-wrong-input)
 (struct-out qp-wrong-line-size)

 ;; -- qp methods --
 qp-encode
 qp-decode
 qp-encode-stream
 qp-decode-stream)

;; Exceptions:
;; String or input-port expected:
(define-struct qp-error ())
(define-struct (qp-wrong-input qp-error) ())
(define-struct (qp-wrong-line-size qp-error) (size))

;; qp-encode : bytes -> bytes
;; returns the quoted printable representation of STR.
(define (qp-encode str)
  (let ([out (open-output-bytes)])
    (qp-encode-stream (open-input-bytes str) out #"\r\n")
    (get-output-bytes out)))

;; qp-decode : string -> string
;; returns STR unqp.
(define (qp-decode str)
  (let ([out (open-output-bytes)])
    (qp-decode-stream (open-input-bytes str) out)
    (get-output-bytes out)))

(define (qp-decode-stream in out)
  (let loop ([ch (read-byte in)])
    (unless (eof-object? ch)
      (case ch
        [(61) ;; A "=", which is quoted-printable stuff
         (let ([next (read-byte in)])
           (cond
             [(eq? next 10)
              ;; Soft-newline -- drop it
              (void)]
             [(eq? next 13)
              ;; Expect a newline for a soft CRLF...
              (let ([next-next (read-byte in)])
                (if (eq? next-next 10)
                  ;; Good.
                  (loop (read-byte in))
                  ;; Not a LF? Well, ok.
                  (loop next-next)))]
             [(eof-object? next)
              (warning "Illegal qp sequence: `='")
              (display "=" out)]
             [(hex-digit? next)
              (let ([next-next (read-byte in)])
                (cond [(eof-object? next-next)
                       (warning "Illegal qp sequence: `=~a'" next)
                       (display "=" out)
                       (display next out)]
                      [(hex-digit? next-next)
                       ;; qp-encoded
                       (write-byte (hex-bytes->byte next next-next)
                                   out)]
                      [else
                       (warning "Illegal qp sequence: `=~a~a'" next next-next)
                       (write-byte 61 out)
                       (write-byte next out)
                       (write-byte next-next out)]))]
             [else
              ;; Warning: invalid
              (warning "Illegal qp sequence: `=~a'" next)
              (write-byte 61 out)
              (write-byte next out)])
           (loop (read-byte in)))]
        [else
         (write-byte ch out)
         (loop (read-byte in))]))))

(define (warning msg . args)
  (when #f (eprintf "~a\n" (apply format msg args))))

(define (hex-digit? i)
  (vector-ref hex-values i))

(define (hex-bytes->byte b1 b2)
  (+ (* 16 (vector-ref hex-values b1))
     (vector-ref hex-values b2)))

(define (write-hex-bytes byte p)
  (write-byte 61 p)
  (write-byte (vector-ref hex-bytes (arithmetic-shift byte -4)) p)
  (write-byte (vector-ref hex-bytes (bitwise-and byte 15)) p))

(define (qp-encode-stream in out [newline-string #"\n"])
  (let loop ([col 0])
    (if (= col 75)
      (begin
        ;; Soft newline:
        (write-byte 61 out)
        (display newline-string out)
        (loop 0))
      (let ([i (read-byte in)])
        (cond
          [(eof-object? i) (void)]
          [(or (= i 10) (= i 13))
           (write-byte i out)
           (loop 0)]
          [(or (<= 33 i 60) (<= 62 i 126)
               (and (or (= i 32) (= i 9))
                    (not (let ([next (peek-byte in)])
                           (or (eof-object? next) (= next 10) (= next 13))))))
           ;; single-byte mode:
           (write-byte i out)
           (loop (add1 col))]
          [(>= col 73)
           ;; need a soft newline first
           (write-byte 61 out)
           (display newline-string out)
           ;; now the octect
           (write-hex-bytes i out)
           (loop 3)]
          [else
           ;; an octect
           (write-hex-bytes i out)
           (loop (+ col 3))])))))

;; Tables
(define hex-values (make-vector 256 #f))
(define hex-bytes (make-vector 16))
(let loop ([i 0])
  (unless (= i 10)
    (vector-set! hex-values (+ i 48) i)
    (vector-set! hex-bytes i (+ i 48))
    (loop (add1 i))))
(let loop ([i 0])
  (unless (= i 6)
    (vector-set! hex-values (+ i 65) (+ 10 i))
    (vector-set! hex-values (+ i 97) (+ 10 i))
    (vector-set! hex-bytes (+ 10 i) (+ i 65))
    (loop (add1 i))))

;;; qp.rkt ends here
