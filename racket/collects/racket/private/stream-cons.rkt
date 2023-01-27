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
         stream-car stream-cdr stream-lambda stream-lazy stream-force
         unpack-multivalue thunk->multivalue)

(struct multivalue (content))

;; An eagerly constructed stream has a lazy first element, and
;; normally its rest is a lazily constructed stream.
(define-struct eagerly-created-stream ([first-forced? #:mutable] [first #:mutable] rest)
  #:reflection-name 'stream
  #:property for:prop:stream (vector
                              (lambda (p) #f)
                              (lambda (p) (stream-force-first p))
                              (lambda (p) (eagerly-created-stream-rest p))))

;; A lazily constructed stream starts with #f if the stream is forced,
;; a symbol for the constructing form otherwise; a #t in place of a
;; symbol means 'stream-cons
(define-struct lazily-created-stream (creator-if-unforced thunk-or-stream)
  #:mutable
  #:reflection-name 'stream
  #:property for:prop:stream (vector
                              (lambda (p) (stream-null? p))
                              (lambda (p) (stream-car p))
                              (lambda (p) (stream-cdr p))))

;; Recognize just the streams created by this layer:
(define (stream? p)
  (or (eagerly-created-stream? p)
      (lazily-created-stream? p)))

(define-syntax stream-lazy
 (syntax-rules ()
   [(stream-lazy expr)
    (make-lazily-created-stream 'stream-lazy (lambda () expr))]
   [(stream-lazy #:who who-expr expr)
    (make-lazily-created-stream (or who-expr 'stream-lazy) (lambda () expr))]))

(define reentrant-error
  (lambda () (raise-arguments-error 'stream "reentrant or broken delay")))

;; Forces a lazily constructed stream to a stream of any other kind
(define (stream-force s)
  (cond
    [(lazily-created-stream? s)
     (let loop ([s s] [dep-ses '()])
       (cond
         [(not (lazily-created-stream-creator-if-unforced s))
          (define v (lazily-created-stream-thunk-or-stream s))
          (for ([dep-s (in-list dep-ses)])
            (set-lazily-created-stream-creator-if-unforced! dep-s #f)
            (set-lazily-created-stream-thunk-or-stream! dep-s v))
          v]
         [else
          (define thunk (lazily-created-stream-thunk-or-stream s))
          (set-lazily-created-stream-thunk-or-stream! s reentrant-error)
          (define v (thunk))
          (cond
            [(lazily-created-stream? v)
             ;; try again
             (loop v (cons s dep-ses))]
            [(for:stream? v)
             ;; any other kind of stream is success
             (set-lazily-created-stream-creator-if-unforced! s #f)
             (set-lazily-created-stream-thunk-or-stream! s v)
             (if (null? dep-ses)
                 v
                 (loop s dep-ses))]
            [else
             (define who (lazily-created-stream-creator-if-unforced s))
             (if (symbol? who)
                 (raise-arguments-error
                  who
                  "delayed expression produced a non-stream"
                  "result" v)
                 (raise-arguments-error
                  'stream-cons
                  "rest expression produced a non-stream"
                  "rest result" v))])]))]
    [(for:stream? s) s]
    [else (raise-argument-error 'stream-force "stream?" s)]))

(define (unpack-multivalue v)
  (cond
    [(multivalue? v) (apply values (multivalue-content v))]
    [else v]))

(define (thunk->multivalue thk)
  (call-with-values
   thk
   (case-lambda
     [(v) v]
     [vs (multivalue vs)])))

;; Forces the first element of an eagerly constructed stream
(define (stream-force-first p)
  (cond
    [(eagerly-created-stream-first-forced? p)
     (unpack-multivalue (eagerly-created-stream-first p))]
    [else
     (define thunk (eagerly-created-stream-first p))
     (set-eagerly-created-stream-first! p reentrant-error)
     (define v (thunk->multivalue thunk))
     (set-eagerly-created-stream-first! p v)
     (set-eagerly-created-stream-first-forced?! p #t)
     (unpack-multivalue v)]))

(define-syntax stream-lambda
 (syntax-rules ()
  ((stream-lambda formals body0 body1 ...)
   (lambda formals (stream-lazy (let () body0 body1 ...))))))

(define (stream-null? obj)
  (for:stream-empty? (stream-force obj)))

(define (stream-pair? obj)
  (eagerly-created-stream? (stream-force obj)))

(define-syntax stream-cons
  (syntax-rules ()
    ((stream-cons obj strm)
     (eagerly-created-stream #f (lambda () obj)
                             (lazily-created-stream #t (lambda () strm))))
    ((stream-cons #:eager obj strm)
     (eagerly-created-stream #t (thunk->multivalue (lambda () obj))
                             (lazily-created-stream #t (lambda () strm))))
    ((stream-cons obj #:eager strm)
     (eagerly-created-stream #f (lambda () obj)
                             (stream-assert strm)))
    ((stream-cons #:eager obj #:eager strm)
     (eagerly-created-stream #t (thunk->multivalue (lambda () obj))
                             (stream-assert strm)))))

(define (stream-assert v)
  (if (for:stream? v)
      v
      (raise-argument-error 'stream-cons "stream?" v)))

(define (stream-car strm)
  (let ([v (stream-force strm)])
    (if (eagerly-created-stream? v) ; shortcut
        (stream-force-first v)
        (for:stream-first v))))

(define (stream-cdr strm)
  (let ([v (stream-force strm)])
    (if (eagerly-created-stream? v) ; shortcut
        (eagerly-created-stream-rest v)
        (for:stream-rest v))))

(define stream-null (lazily-created-stream #f '()))
