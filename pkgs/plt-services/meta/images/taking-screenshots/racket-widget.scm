;;; -*- scheme -*-

(script-fu-register "script-fu-racket-widget"
                    "Racket Widget"
                    "Take a screen shot of a Racket Widget"
                    "Diogo F. S. Ramos"
                    "copyright 2012-2014, Diogo F. S. Ramos"
                    "August, 2012"
                    "")
(script-fu-menu-register "script-fu-racket-widget" "<Image>/File/Create")

(define (script-fu-racket-widget)
  (define (crop-to-selection selection image)
    (let ((x1 (list-ref selection 0))
          (y1 (list-ref selection 1))
          (x2 (list-ref selection 2))
          (y2 (list-ref selection 3)))
      (gimp-image-crop image
                       (- x2 x1)
                       (- y2 y1)
                       x1 y1)))
  (let ((image (car (plug-in-screenshot RUN-INTERACTIVE 0 0 0 0 0 0))))
    (gimp-fuzzy-select (car (gimp-image-get-active-drawable image))
                       (/ (car (gimp-image-width image)) 2)
                       (/ (car (gimp-image-height image)) 2)
                       15.0
                       CHANNEL-OP-REPLACE
                       TRUE
                       FALSE
                       0.0
                       TRUE)
    (gimp-selection-invert image)
    (gimp-selection-shrink image 1.0)
    (crop-to-selection (cdr (gimp-selection-bounds image)) image)
    (script-fu-addborder image
                         (car (gimp-image-flatten image))
                         1.0
                         1.0
                         '(0 0 0)
                         25)
    (script-fu-drop-shadow image
                           (car (gimp-image-flatten image))
                           8.0
                           8.0
                           9.0
                           '(0 0 0)
                           80.0
                           1)))
