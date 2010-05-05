#lang scribble/doc
@(require scribble/eval
          "common.ss"
          "diagrams.ss")

@title[#:tag "drawing-overview"]{Drawing}

Drawing in Racket requires a @deftech{device context}
(@deftech{DC}), which is an instance of the @scheme[dc<%>]
interface. For example, the @method[canvas<%> get-dc] method of a
canvas returns a @scheme[dc<%>] instance for drawing into the canvas
window.  Other kinds of DCs draw to different kinds of devices:

@itemize[

 @item{@scheme[bitmap-dc%] --- a @deftech{bitmap DC} draws to an
 offscreen bitmap.}

 @item{@scheme[post-script-dc%] --- a @deftech{PostScript DC}
 records drawing commands to a PostScript file.}

 @item{@scheme[printer-dc%] --- a @deftech{printer DC} draws to a
 platform-specific printer device (Windows, Mac OS X).}

]

Tools that are used for drawing include the following: @scheme[pen%]
 objects for drawing lines and shape outlines, @scheme[brush%]
 objects for filling shapes, @scheme[bitmap%] objects for storing
 bitmaps, and @scheme[dc-path%] objects for describing paths to draw
 and fill.

The following example creates a frame with a drawing canvas, and then
 draws a round, blue face with square, yellow eyes and a smiling, red
 mouth:

@schemeblock[
(code:comment @#,t{Make a 300 x 300 frame})
(define frame (new frame% [label "Drawing Example"]
                          [width 300]
                          [height 300]))
(code:comment @#,t{Make the drawing area})
(define canvas (new canvas% [parent frame]))
(code:comment @#,t{Get the canvas's drawing context})
(define dc (send canvas #,(:: canvas<%> get-dc)))

(code:comment @#,t{Make some pens and brushes})
(define no-pen (make-object pen% "BLACK" 1 'transparent))
(define no-brush (make-object brush% "BLACK" 'transparent))
(define blue-brush (make-object brush% "BLUE" 'solid))
(define yellow-brush (make-object brush% "YELLOW" 'solid))
(define red-pen (make-object pen% "RED" 2 'solid))

(code:comment @#,t{Define a procedure to draw a face})
(define (draw-face dc) 
  (send dc #,(:: dc<%> set-pen) no-pen) 
  (send dc #,(:: dc<%> set-brush) blue-brush) 
  (send dc #,(:: dc<%> draw-ellipse) 50 50 200 200) 

  (send dc #,(:: dc<%> set-brush) yellow-brush) 
  (send dc #,(:: dc<%> draw-rectangle) 100 100 10 10) 
  (send dc #,(:: dc<%> draw-rectangle) 200 100 10 10) 

  (send dc #,(:: dc<%> set-brush) no-brush) 
  (send dc #,(:: dc<%> set-pen) red-pen) 
  (let ([-pi (atan 0 -1)]) 
    (send dc #,(:: dc<%> draw-arc) 75 75 150 150 (* 5/4 -pi) (* 7/4 -pi))))

(code:comment @#,t{Show the frame})
(send frame #,(:: top-level-window<%> show) #t) 
(code:comment @#,t{Wait a second to let the window get ready})
(sleep/yield 1) 
(code:comment @#,t{Draw the face})
(draw-face dc)
]

The @scheme[sleep/yield] call is necessary under X because
 drawing to the canvas has no effect when the canvas is not
 shown. Although the @scheme[(send frame #,(:: top-level-window<%> show) #t)]
 expression queues a show request for the frame, the actual display of
 the frame and its canvas requires handling several events. The
 @scheme[sleep/yield] procedure pauses for a specified number
 of seconds, handling events while it pauses.

One second is plenty of time for the frame to show itself, but a
 better solution is to create a canvas with a paint callback function
 (or overriding @method[canvas<%> on-paint]). Using a paint
 callback function is better for all platforms; when the canvas in the
 above example is resized or temporarily covered by another window,
 the face disappears. To ensure that the face is redrawn whenever the
 canvas itself is repainted, we provide a paint callback when creating
 the canvas:

@schemeblock[
(code:comment @#,t{Make a 300 x 300 frame})
(define frame (new frame% [label "Drawing Example"]
                          [width 300]
                          [height 300]))

(code:comment @#,t{Make the drawing area with a paint callback})
(define canvas
  (new canvas% [parent frame]
               [paint-callback
                (lambda (canvas dc) (draw-face dc))]))

(code:comment @#,t{... pens, brushes, and @scheme[draw-face] are the same as above ...})

(code:comment @#,t{Show the frame})
(send frame #,(:: top-level-window<%> show) #t)
]

Suppose that @scheme[draw-face] creates a particularly complex face that
 takes a long time to draw. We might want to draw the face once into
 an offscreen bitmap, and then have the paint callback copy the cached
 bitmap image onto the canvas whenever the canvas is updated. To draw
 into a bitmap, we first create a @scheme[bitmap%] object, and then
 we create a @scheme[bitmap-dc%] to direct drawing commands into the
 bitmap:

@schemeblock[
(code:comment @#,t{... pens, brushes, and @scheme[draw-face] are the same as above ...})
 
(code:comment @#,t{Create a 300 x 300 bitmap})
(define face-bitmap (make-object bitmap% 300 300))
(code:comment @#,t{Create a drawing context for the bitmap})
(define bm-dc (make-object bitmap-dc% face-bitmap))
(code:comment @#,t{A bitmap's initial content is undefined; clear it before drawing})
(send bm-dc #,(:: dc<%> clear))
 
(code:comment @#,t{Draw the face into the bitmap})
(draw-face bm-dc) 
 
(code:comment @#,t{Make a 300 x 300 frame})
(define frame (new frame% [label "Drawing Example"]
                          [width 300]
                          [height 300]))

(code:comment @#,t{Make a drawing area whose paint callback copies the bitmap})
(define canvas
  (new canvas% [parent frame]
               [paint-callback
                (lambda (canvas dc)
                  (send dc #,(:: dc<%> draw-bitmap) face-bitmap 0 0))]))
 
(code:comment @#,t{Show the frame})
(send frame #,(:: top-level-window<%> show) #t)
]

For all types of DCs, the drawing origin is the top-left corner of the
 DC. When drawing to a window or bitmap, DC units initially correspond
 to pixels, but the @method[dc<%> set-scale] method changes the
 scale. When drawing to a PostScript or printer device, DC units
 initially correspond to points (1/72 of an inch).

More complex shapes are typically best implemented with
 @deftech{paths}. The following example uses paths to draw the
 Racket logo. It also enables smoothing, so that the logo's curves are
 anti-aliased when smoothing is available. (Smoothing is always
 available under Mac OS X, smoothing is available under Windows XP or
 when @filepath{gdiplus.dll} is installed, and smoothing is available
 under X when Cairo is installed before GRacket is compiled.)

@(begin
#readerscribble/comment-reader
[schemeblock
(require mzlib/math) ; for @scheme[pi]

;; Construct paths for a 630 x 630 logo

(define left-lambda-path ;; left side of the lambda
  (let ([p (new dc-path%)])
    (send p #,(:: dc-path% move-to) 153 44)
    (send p #,(:: dc-path% line-to) 161.5 60)
    (send p #,(:: dc-path% curve-to) 202.5 49 230 42 245 61)
    (send p #,(:: dc-path% curve-to) 280.06 105.41 287.5 141 296.5 186)
    (send p #,(:: dc-path% curve-to) 301.12 209.08 299.11 223.38 293.96 244)
    (send p #,(:: dc-path% curve-to) 281.34 294.54 259.18 331.61 233.5 375)
    (send p #,(:: dc-path% curve-to) 198.21 434.63 164.68 505.6 125.5 564)
    (send p #,(:: dc-path% line-to) 135 572)
    p))

(define left-logo-path ;; left side of the lambda and circle
  (let ([p (new dc-path%)])
    (send p #,(:: dc-path% append) left-lambda-path)
    (send p #,(:: dc-path% arc) 0 0 630 630 (* 235/360 2 pi) (* 121/360 2 pi) #f)
    p))

(define bottom-lambda-path 
  (let ([p (new dc-path%)])
    (send p #,(:: dc-path% move-to) 135 572)
    (send p #,(:: dc-path% line-to) 188.5 564)
    (send p #,(:: dc-path% curve-to) 208.5 517 230.91 465.21 251 420)
    (send p #,(:: dc-path% curve-to) 267 384 278.5 348 296.5 312)
    (send p #,(:: dc-path% curve-to) 301.01 302.98 318 258 329 274)
    (send p #,(:: dc-path% curve-to) 338.89 288.39 351 314 358 332)
    (send p #,(:: dc-path% curve-to) 377.28 381.58 395.57 429.61 414 477)
    (send p #,(:: dc-path% curve-to) 428 513 436.5 540 449.5 573)
    (send p #,(:: dc-path% line-to) 465 580)
    (send p #,(:: dc-path% line-to) 529 545)
    p))

(define bottom-logo-path
  (let ([p (new dc-path%)])
    (send p #,(:: dc-path% append) bottom-lambda-path)
    (send p #,(:: dc-path% arc) 0 0 630 630 (* 314/360 2 pi) (* 235/360 2 pi) #f)
    p))

(define right-lambda-path
  (let ([p (new dc-path%)])
    (send p #,(:: dc-path% move-to) 153 44)
    (send p #,(:: dc-path% curve-to) 192.21 30.69 233.21 14.23 275 20)
    (send p #,(:: dc-path% curve-to) 328.6 27.4 350.23 103.08 364 151)
    (send p #,(:: dc-path% curve-to) 378.75 202.32 400.5 244 418 294)
    (send p #,(:: dc-path% curve-to) 446.56 375.6 494.5 456 530.5 537)
    (send p #,(:: dc-path% line-to) 529 545)
    p))

(define right-logo-path
  (let ([p (new dc-path%)])
    (send p #,(:: dc-path% append) right-lambda-path)
    (send p #,(:: dc-path% arc) 0 0 630 630 (* 314/360 2 pi) (* 121/360 2 pi) #t)    
    p))

(define lambda-path ;; the lambda by itself (no circle)
  (let ([p (new dc-path%)])
    (send p #,(:: dc-path% append) left-lambda-path)
    (send p #,(:: dc-path% append) bottom-lambda-path)
    (let ([t (make-object dc-path%)])
        (send t #,(:: dc-path% append) right-lambda-path)
        (send t #,(:: dc-path% reverse))
        (send p #,(:: dc-path% append) t))
    (send p #,(:: dc-path% close))
    p))

;; This function draws the paths with suitable colors:
(define (paint-plt dc)
  ;; Paint white lambda, no outline:
  (send dc #,(:: dc<%> set-pen) "BLACK" 0 'transparent)
  (send dc #,(:: dc<%> set-brush) "WHITE" 'solid)
  (send dc #,(:: dc<%> draw-path) lambda-path)
  ;; Paint outline and colors...
  (send dc #,(:: dc<%> set-pen) "BLACK" 0 'solid)
  ;; Draw red regions
  (send dc #,(:: dc<%> set-brush) "RED" 'solid)
  (send dc #,(:: dc<%> draw-path) left-logo-path)
  (send dc #,(:: dc<%> draw-path) bottom-logo-path)
  ;; Draw blue region
  (send dc #,(:: dc<%> set-brush) "BLUE" 'solid)
  (send dc #,(:: dc<%> draw-path) right-logo-path))

;; Create a frame to display the logo on a light-purple background:
(define f (new frame% [label "Racket Logo"]))
(define c
  (new canvas% 
       [parent f]
       [paint-callback
        (lambda (c dc)
          (send dc #,(:: dc<%> set-background) (make-object color% 220 200 255))
          (send dc #,(:: dc<%> clear))
          (send dc #,(:: dc<%> set-smoothing) 'smoothed)
          (send dc #,(:: dc<%> set-origin) 5 5)
          (send dc #,(:: dc<%> set-scale) 0.5 0.5)
          (paint-plt dc))]))
(send c #,(:: canvas<%> min-client-width) (/ 650 2))
(send c #,(:: canvas<%> min-client-height) (/ 650 2))
(send f show #t)
])

Drawing effects are not completely portable across platforms or across
 types of DC. Drawing in smoothed mode tends to produce more reliable
 and portable results than in unsmoothed mode, and drawing with paths
 tends to produce more reliable results even in unsmoothed
 mode. Drawing with a pen of width 0 or 1 in unsmoothed mode in an
 unscaled DC produces relatively consistent results for all platforms,
 but a pen width of 2 or drawing to a scaled DC looks significantly
 different in unsmoothed mode on different platforms and destinations.
