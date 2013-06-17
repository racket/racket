#lang racket
(require racket/gui
         racket/class)

(provide bullet-snip%
         get-bullet-width
         bullet-size
         bullet-snip-class)

(define snip-class-name "(lib \"bullet-snip.rkt\" \"browser\")")

(define bullet-size 
  (make-parameter
   (let ([s (send (send (send (make-object text%) get-style-list) basic-style)
                  get-size)])
     (max 7 (quotient s 2)))))

(define (get-bullet-width)
  (* 2 (bullet-size)))

(define transparent-brush (send the-brush-list find-or-create-brush "WHITE" 'transparent))

(define bullet-snip%
  (class snip% 
    (init-field depth)
    (inherit set-snipclass set-count get-style)
    (define bsize (bullet-size))
    (define/private (zero b) (when b (set-box! b 0)))
    [define/private get-height
      (lambda (dc)
        (let ([s (get-style)])
          (max bsize (- (send s get-text-height dc)
                        (send s get-text-descent dc)))))]
    
    [define/override get-extent
      (lambda (dc x y wbox hbox descentbox spacebox
                  lspacebox rspacebox)
        (when hbox
          (set-box! hbox (get-height dc)))
        (when wbox
          (set-box! wbox (* 2 bsize)))
        (zero descentbox)
        (zero spacebox)
        (zero rspacebox)
        (zero lspacebox))]
    [define/override draw
      (lambda (dc x y . other)
        (let ([y (+ y (ceiling (/ (- (get-height dc) bsize) 2)))])
          (let-values ([(draw solid?)
                        (case depth
                          [(0) (values (lambda (x y w h) (send dc draw-ellipse x y w h)) #t)]
                          [(1) (values (lambda (x y w h) (send dc draw-ellipse x y w h)) #f)]
                          [else (values (lambda (x y w h) (send dc draw-rectangle x y w h)) #f)])])
            (let ([b (send dc get-brush)])
              (send dc set-brush
                    (if solid?
                        (send the-brush-list
                              find-or-create-brush
                              (send (send dc get-pen) get-color)
                              'solid)
                        transparent-brush))
              (draw x y bsize bsize)
              (send dc set-brush b)))))]
    [define/override copy
      (lambda ()
        (make-object bullet-snip% depth))]
    [define/override write
      (lambda (stream)
        (send stream put depth))]
    [define/override get-text
      (lambda (offset num flattened?)
        (if (< num 1)
            ""
            (if flattened?
                "* "
                "*")))]
    (super-new)
    (set-snipclass bullet-snip-class)
    (set-count 1)))

(define bullet-snip-class
  (make-object 
      (class snip-class%
        (inherit set-classname)
        [define/override read
          (lambda (stream)
            (let ([d-box (box 0)])
              (send stream get d-box)
              (make-object bullet-snip% (unbox d-box))))]
        (super-new)
        (set-classname snip-class-name))))

(send (get-the-snip-class-list) add bullet-snip-class)

