; Library streams/primitive
; Adapted for PLT Scheme by Jacob J. A. Koot
;   from original version of Philip L. Bewig.
; Further adapted by Matthew to allow any kind
;  of Racket stream as the rest of a lazy stream.

; Copyright (C) 2007 by Philip L. Bewig of Saint Louis, Missouri, USA.  All rights
; reserved.  Permission is hereby granted, free of charge, to any person obtaining a copy of
; this software and associated documentation files (the "Software"), to deal in the Software
; without restriction, including without limitation the rights to use, copy, modify, merge,
; publish, distribute, sublicense, and/or sell copies of the Software, and to permit persons to
; whom the Software is furnished to do so, subject to the following conditions: The above
; copyright notice and this permission notice shall be included in all copies or substantial
; portions of the Software.  THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
; EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS
; FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT
; HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF
; CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR
; THE USE OR OTHER DEALINGS IN THE SOFTWARE.

#lang racket/base
(require (prefix-in for: racket/private/for))

(provide stream-null stream-cons stream? stream-null? stream-pair?
         stream-car stream-cdr stream-lambda)

(define-syntax stream-lazy
 (syntax-rules ()
  ((stream-lazy expr)
   (make-stream
    #false (lambda () expr)))))

(define (stream-eager expr)
 (make-stream
  #true expr))

(define-syntax stream-delay
 (syntax-rules ()
  ((stream-delay expr)
   (stream-lazy (stream-eager expr)))))

(define (stream-force stream)
 (let ([value (stream-value stream)])
   (case (stream-forced? stream)
     [(#t) value]
     [(#f) (let* ([value* (value)])
             ;; check stream-forced? again, it can it was set in the
             ;; process of evaluating `(value)':
             (if (stream-forced? stream)
                 ;; yes, it was set
                 (stream-value stream)
                 ;; normal case: no, it wasn't set:
                 (if (stream? value*)
                     ;; Flatten the result lazy stream and try again:
                     (let ()
                       (set-stream-forced?! stream (stream-forced? value*))
                       (set-stream-value! stream (stream-value value*))
                       
                       (stream-force stream))
                     ;; Forced result is not a lazy stream:
                     (begin
                       (unless (for:stream? value*)
                         (raise-mismatch-error 
                          'stream-cons
                          "rest expression produced a non-stream: "
                          value*))
                       (set-stream-value! stream value*)
                       (set-stream-forced?! stream #true)
                       value*))))])))


(define-syntax stream-lambda
 (syntax-rules ()
  ((stream-lambda formals body0 body1 ...)
   (lambda formals (stream-lazy (let () body0 body1 ...))))))

(define-struct stream-pare (kar kdr))

(define (stream-null? obj)
  (let ([v (stream-force obj)])
    (if (stream-pare? v)
        #f
        (or (eqv? v (stream-force stream-null))
            (for:stream-empty? v)))))

(define (stream-pair? obj)
 (and (stream? obj) (stream-pare? (stream-force obj))))

(define-syntax stream-cons
 (syntax-rules ()
  ((stream-cons obj strm)
   (stream-eager (make-stream-pare (stream-delay obj) (stream-lazy strm))))))

(define (stream-car strm)
  (let ([v (stream-force strm)])
    (if (stream-pare? v)
        (stream-force (stream-pare-kar v))
        (for:stream-first v))))

(define (stream-cdr strm)
  (let ([v (stream-force strm)])
    (if (stream-pare? v)
        (stream-pare-kdr v)
        (for:stream-rest v))))

(define-struct stream (forced? value) 
  #:mutable
  #:property for:prop:stream (vector
                              stream-null?
                              stream-car
                              stream-cdr))

(define stream-null (stream-delay (cons 'stream 'null)))
