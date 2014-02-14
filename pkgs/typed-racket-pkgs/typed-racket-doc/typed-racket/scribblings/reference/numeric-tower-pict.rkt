#lang racket/base

;; This module provides a pict version of the Typed Racket numeric type
;; hierarchy diagram from "Typing the Numeric Tower" (pg 5) from PADL 2012

(require pict)

(provide numeric-tower-pict
         integer-pict)

;; large type category heading
(define (text/1 str)
  (text str null 20))

;; small type category
(define (text/2 str)
  (text str null 15))

(define base-complex
  (linewidth
   2
   (cc-superimpose
    (colorize (filled-rectangle 560 350) "white")
    (rectangle 560 350)
    (ellipse 510 280)
    (vline 1 350))))

(define left-side
  (ct-superimpose
   (cc-superimpose
    (ghost (rectangle 270 350))
    (hc-append (blank 30 1)
               (cb-superimpose (linewidth 2 (ellipse 140 110))
                               (vc-append (text/1 "Integer")
                                          (blank 1 10))))
    (hc-append (blank 25 1)
               (linewidth 2 (linestyle 'dot (hline 250 2))))
    (hc-append (blank 30 1)
               (cc-superimpose (colorize (filled-ellipse 50 35) "white")
                               (linewidth 2 (linestyle 'dot (ellipse 50 35)))
                               (text/2 "Zero"))))
   (vc-append (blank 1 7) (text/1 "Exact-Number"))
   (vr-append (blank 265 70)
              ;; this doesn't line up without the blank for
              ;; some reason (due to text formatting)
              ;(hc-append (text/1 "Exact-Rational") (blank 7 1))
              (text/1 "Exact-Rational")
              (text/2 "Positive-Exact-Rational")
              (blank 1 130)
              (text/2 "Negative-Exact-Rational"))))

(define right-side
  (ct-superimpose
   (cc-superimpose
    (ghost (rectangle 270 350))
    (hc-append (linewidth 2 (linestyle 'dot (hline 260 2)))
               (blank 25 1))
    (hc-append (cc-superimpose (colorize (filled-ellipse 100 35) "white")
                               (linewidth 2 (linestyle 'dot (ellipse 100 35)))
                               (text/2 "Float-Zero"))
               (blank 30 1)))
   (vc-append (blank 1 7) (text/1 "Float-Complex"))
   (vl-append (blank 245 70)
              (text/1 "Float")
              (text/2 "Positive-Float")
              (blank 1 130)
              (text/2 "Negative-Float"))))

(define *integer-pict
  (ct-superimpose
   (linewidth
    2
    (cc-superimpose
     (cc-superimpose
      (colorize (filled-rectangle 200 350) "white")
      (rectangle 200 350))
     (rectangle 170 280)
     (ct-superimpose (ghost (rectangle 140 210))
                     (inset/clip (linewidth 3 (rectangle 140 210)) 0 0 0 -105))
     (ct-superimpose (ghost (rectangle 110 140))
                     (inset/clip (linewidth 3 (rectangle 110 140)) 0 0 0 -70))
     (hline 200 2)
     (cc-superimpose
      (colorize (filled-ellipse 50 30) "white")
      (ellipse 50 30)
      (text/2 "Zero"))))
   (vc-append (blank 1 7)
              (text/2 "Positive-Integer")
              (blank 1 20)
              (text/2 "Positive-Fixnum")
              (blank 1 20)
              (text/2 "Positive-Index")
              (blank 1 20)
              (text/2 "Positive-Byte")
              (blank 1 100)
              (text/2 "Negative-Fixnum")
              (blank 1 70)
              (text/2 "Negative-Integer"))))

(define complex-pict
  (rc-superimpose
   (lc-superimpose
    base-complex
    left-side)
   right-side))

(define numeric-tower-pict
  ;; inset to avoid clipping a border
  (inset (ht-append
          30
          (vc-append
           10
           (vl-append
            10
            (text/1 "Complex / Number")
            complex-pict)
           (text/1 "Exact-Rational ∪ Float = Real")))
         1))

(define integer-pict
  (inset (vl-append 10 (text/1 "Integer") *integer-pict) 1))
