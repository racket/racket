#lang racket

(require racklog)

;The following is a simple database about a certain family in England.
;Should be a piece of cake, but given here so that you can hone
;your ability to read the syntax.

;This file is written using goal combinations like %or, %and
;like you would use Scheme procedures.  For a more Prolog-like
;syntax of the same program, see england.scm.

(define %male
  (lambda (x)
    (%or (%= x 'philip)
	 (%= x 'charles)
	 (%= x 'andrew)
	 (%= x 'edward)
	 (%= x 'mark)
	 (%= x 'william)
	 (%= x 'harry)
	 (%= x 'peter))))

(define %female
  (lambda (x)
    (%or (%= x 'elizabeth)
	 (%= x 'anne)
	 (%= x 'diana)
	 (%= x 'sarah)
	 (%= x 'zara))))

(define %husband-of
  (lambda (h w)
    (%or (%and (%= h 'philip) (%= w 'elizabeth))
	 (%and (%= h 'charles) (%= w 'diana))
	 (%and (%= h 'mark) (%= w 'anne))
	 (%and (%= h 'andrew) (%= w 'sarah)))))

(define %wife-of
  (lambda (w h)
    (%husband-of h w)))

(define %married-to
  (lambda (x y)
    (%or (%husband-of x y) (%wife-of x y))))

(define %father-of
  (lambda (x y)
    (%or (%and (%= x 'philip) (%= y 'charles))
	 (%and (%= x 'philip) (%= y 'anne))
	 (%and (%= x 'philip) (%= y 'andrew))
	 (%and (%= x 'philip) (%= y 'edward))
	 (%and (%= x 'charles) (%= y 'william))
	 (%and (%= x 'charles) (%= y 'harry))
	 (%and (%= x 'mark) (%= y 'peter))
	 (%and (%= x 'mark) (%= y 'zara)))))

(define %mother-of
  (lambda (m c)
    (%let (f)
      (%and (%wife-of m f) (%father-of f c)))))

(define %child-of
  (lambda (c p)
    (%or (%father-of p c) (%mother-of p c))))

(define %parent-of
  (lambda (p c)
    (%child-of c p)))

(define %brother-of
  (lambda (b x)
    (%let (f)
      (%and (%male b)
	    (%father-of f b)
	    (%father-of f x)
	    (%/= b x)))))

