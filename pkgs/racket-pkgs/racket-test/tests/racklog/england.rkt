#lang racket

(require racklog
         rackunit)

;The following is a simple database about a certain family in England.
;Should be a piece of cake, but given here so that you can hone
;your ability to read the syntax.

;This file is written using `%rel' for a more Prolog-like syntax.
;The file england2.scm uses a Scheme-like syntax.

(define %male
  (%rel ()
    (('philip)) (('charles)) (('andrew)) (('edward))
    (('mark)) (('william)) (('harry)) (('peter))))

(define %female
  (%rel ()
    (('elizabeth)) (('anne)) (('diana)) (('sarah)) (('zara))))

(define %husband-of
  (%rel ()
    (('philip 'elizabeth)) (('charles 'diana))
    (('mark 'anne)) (('andrew 'sarah))))

(define %wife-of
  (%rel (w h)
    ((w h) (%husband-of h w))))

(define %married-to
  (%rel (x y)
    ((x y) (%husband-of x y))
    ((x y) (%wife-of x y))))

(define %father-of
  (%rel ()
   (('philip 'charles)) (('philip 'anne)) (('philip 'andrew))
   (('philip 'edward)) (('charles 'william)) (('charles 'harry))
   (('mark 'peter)) (('mark 'zara))))

(define %mother-of
  (%rel (m c f)
    ((m c) (%wife-of m f) (%father-of f c))))

(define %child-of
  (%rel (c p)
    ((c p) (%father-of p c))
    ((c p) (%mother-of p c))))

(define %parent-of
  (%rel (p c)
    ((p c) (%child-of c p))))

(define %brother-of
  (%rel (b x f)
    ((b x) (%male b) (%father-of f b) (%father-of f x) (%/= b x))))
