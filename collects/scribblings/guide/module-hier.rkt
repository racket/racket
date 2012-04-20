#lang racket/base
(require slideshow/pict
         racket/draw
         racket/class
         racket/math)

(provide module-hierarchy)

(define GAP 12)

(define file-color (make-object color% #xEC #xF5 #xF5))

(define folder
  (let ()
    (define W 200)
    (define H 144)
    (define dy (* -8/3 2))
    (define p (make-object dc-path%))
    (send p move-to 0 50)
    (send p arc 0 (+ 22 dy) 8 8 pi (/ pi 2) #f)
    (send p arc 2 (+ 18 dy) 4 4 (/ pi -2) 0 #t)
    (send p arc 6 0 20 20 pi (/ pi 2) #f)
    (send p line-to 60 0)
    (send p arc 50 0 20 20 (/ pi 2) 0 #f)
    (send p arc 70 (+ 22 dy) 2 2 pi (* 3/2 pi))
    (send p arc 180 (+ 24 dy) 20 20 (/ pi 2) 0 #f)
    (send p arc 180 120 20 20 0 (/ pi -2) #f)
    (send p arc 0 120 20 20 (/ pi -2) (- pi) #f)
    (send p close)

    (scale
     (dc (lambda (dc x y)
           (define b (send dc get-brush))
           (send dc set-brush file-color 'solid)
           (send dc draw-path p x y)
           (send dc set-brush b))
         W H)
     12/32)))

(define file
  (file-icon (/ 75 2) 54 file-color))

(define (lbl i t)
  (vc-append 4 i (text t '(bold . modern) 12)))

(define (listing p)
  (frame (inset p GAP)
         #:color "blue"))

(define db-folder (launder folder))
(define mach-folder (launder folder))
(define mach-listing 
  (listing
   (vc-append
    GAP
    (lbl file "control.rkt")
    (hc-append (* 2 GAP)
               (lbl file "sensors.rkt")
               (lbl file "actuators.rkt")))))
(define db-listing
  (listing
   (vc-append
    GAP
    (lbl file "lookup.rkt")
    (hc-append (* 2 GAP)
               (lbl file "barcodes.rkt")
               (lbl file "makers.rkt")))))

(define (zoom from to p)
  (pin-line
   (pin-line p
             from lb-find
             to lt-find
             #:style 'dot)
   from rb-find
   to rt-find
   #:style 'dot))
              

(define module-hierarchy
  (inset
   (zoom
    db-folder db-listing
    (zoom
     mach-folder mach-listing
     (vc-append
      (* 3 GAP)
      (listing
       (hc-append (* 4 GAP)
                  (lbl file "sort.rkt")
                  (lbl db-folder "db")
                  (lbl mach-folder "machine")))
      (hc-append
       (* 2 GAP)
       db-listing
       mach-listing))))
   2))
