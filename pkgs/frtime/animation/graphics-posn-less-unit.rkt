; Simple graphics routines for GRacket
; Originally written by Johnathan Franklin
;
; modified by Gregory Cooper to support FrTime

#lang racket/unit

(require (for-syntax syntax/parse racket/base)
         racket/class
         mred/mred-sig
         frtime/core/frp
         "graphics-sig.rkt")

(import (prefix mred: mred^) graphics:posn^)
(export graphics:posn-less^)

(define-syntax (rec stx)
  (syntax-parse stx
                [((~literal rec) var:identifier rhs:expr)
                 #'(letrec ([var rhs])
                     var)]))

(define send/proc
  (lambda (class method . args)
    (send-generic class (make-generic mred:dc<%> method) . args)))

(define send/proc2
  (lambda (class method . args)
    (send-generic class (make-generic sixlib-canvas% method) . args)))

(define-struct viewport (label canvas))
(define-struct sixmouse (x y left? middle? right?))
(define-struct sixkey (value shift control meta alt))
(define graphics-flag #f)
(define global-viewport-list '())
(define global-color-vector (make-vector 300 #f))
(define global-pen-vector (make-vector 300 #f))
(define global-brush-vector (make-vector 300 #f))
(define default-font (make-object mred:font% 12 'roman 'normal 'normal))
(define black-color (make-object mred:color% "BLACK"))

(define sixlib-canvas%
  (class mred:canvas%
    (super-new)
    (inherit get-parent
             min-client-width min-client-height
             stretchable-width stretchable-height)
    (define current-mouse-pos (make-posn 0 0))
    (define mouse-listener #f)
    (define key-listener #f)
    (private*
     [reset-size
      (lambda ()
        (min-client-width width)
        (min-client-height height)
        (stretchable-width #f)
        (stretchable-height #f)
        (set! bitmap (make-object mred:bitmap% width height))
        (unless (send bitmap ok?)
          (error "cannot allocate viewport"))
        (send buffer-dc set-bitmap bitmap)
        (send buffer-dc set-brush (send dc get-brush))
        (send buffer-dc set-pen (send dc get-pen))
        (send buffer-dc set-smoothing 'aligned)
        (let ([f (send dc get-font)])
          (when f
            (send buffer-dc set-font f)))
        (send buffer-dc clear)
        (send dc clear))])
    
    ;; were public
    (define viewport (void))
    (define height 0)
    (define width 0)
    (define label 0)
    (define current-pen 'uninitialized-pen)
    (define current-brush 'uninitialized-brush)
    (define bitmap 'uninitalized-bitmap)
    (define dc 'uninitialized-dc)
    (define buffer-dc 'uninitialized-buffer-dc)
    
    (public*
     [get-mouse-listener (lambda () mouse-listener)]
     [get-key-listener (lambda () key-listener)]
     [set-mouse-listener (lambda (ml) (set! mouse-listener ml))]
     [set-key-listener (lambda (kl) (set! key-listener kl))]
     [get-posn (lambda () current-mouse-pos)]
     [get-viewport (lambda () viewport)]
     [set-viewport (lambda (x) (set! viewport x))]
     [get-sixlib-height (lambda () height)]
     [get-sixlib-width (lambda () width)]
     [get-current-pen (lambda () current-pen)]
     [get-current-brush (lambda () current-brush)]
     [get-bitmap (lambda () bitmap)]
     [get-sixlib-dc (lambda () dc)]
     [get-buffer-dc (lambda () buffer-dc)]
     [remember-pen (lambda (pen) (set! current-pen pen))]
     [remember-brush (lambda (brush) (set! current-brush brush))])
    
    (override*
     [on-paint
      (lambda ()
        (let ([bm (send buffer-dc get-bitmap)])
          (send dc draw-bitmap bm 0 0)))]
     
     [on-event 
      (lambda (mouse-event)
        (set! current-mouse-pos (make-posn (send mouse-event get-x)
                                           (send mouse-event get-y)))
        (send-event mouse-listener mouse-event))]
     #|
	  (let* ([x (send mouse-event get-x)]
		 [y (send mouse-event get-y)]
		 [left? (send mouse-event button-down? 'left)]
		 [middle? (send mouse-event button-down? 'middle)]
		 [right? (send mouse-event button-down? 'right)]
		 [sixm (make-sixmouse x y left? middle? right?)])
            (set! current-mouse-pos (make-posn x y))
            (if mouse-listener
                (send-event mouse-listener sixm))))]
|#       
     [on-char
      (lambda (key-event)
        (when key-listener
          (send-event
           key-listener
           (make-sixkey
            (send key-event get-key-code)
            (send key-event get-shift-down)
            (send key-event get-control-down)
            (send key-event get-meta-down)
            (send key-event get-alt-down)))))])
    
    (public*
     [set-dc (lambda (new-dc) (set! dc new-dc))]
     [set-buffer-dc (lambda (new-buffer-dc) (set! buffer-dc
                                                  new-buffer-dc))]
     
     [set-geometry
      (lambda (new-width new-height)
        (set! height new-height)
        (set! width new-width)
        (reset-size))]
     [set-height (lambda (new-height) 
                   (set! height new-height)
                   (reset-size))]
     [set-width (lambda (new-width) 
                  (set! width new-width)
                  (reset-size))])))

(define open-frames-timer (make-object mred:timer%))

(define sixlib-frame%
  (class mred:frame%
    (field [canvas #f])
    (define/public (set-canvas x) (set! canvas x))
    (define/augment (on-close)
      (close-viewport (send canvas get-viewport))
      (inner (void) on-close))
    (super-instantiate ())))

(define (query-mouse-posn viewport)
  (send (viewport-canvas viewport) get-posn))

(define repaint
  (lambda (viewport)
    (send (viewport-canvas viewport) on-paint)))

(define viewport-mouse-events
  (lambda (viewport)
    (send (viewport-canvas viewport) get-mouse-listener)))

(define viewport-key-events
  (lambda (viewport)
    (send (viewport-canvas viewport) get-key-listener)))

(define viewport-dc 
  (lambda (viewport)
    (send (viewport-canvas viewport) get-sixlib-dc)))

(define viewport-buffer-dc
  (lambda (viewport)
    (send (viewport-canvas viewport) get-buffer-dc)))

(define viewport-bitmap
  (lambda (viewport)
    (send (viewport-canvas viewport) get-bitmap)))

(define viewport-frame
  (lambda (viewport)
    (send (send (viewport-canvas viewport) get-parent) get-parent)))

(define viewport-height
  (lambda (viewport)
    (send (viewport-canvas viewport) get-sixlib-height)))

(define viewport-width
  (lambda (viewport)
    (send (viewport-canvas viewport) get-sixlib-width)))

(define clear-viewport
  (lambda (viewport)
    (let* ([vdc (viewport-dc viewport)]
           [vbdc (viewport-buffer-dc viewport)])
      (lambda ()
        (send vdc clear)
        (send vbdc clear)))))



(define draw-viewport
  (lambda (viewport)
    (let* ([dc (viewport-dc viewport)]
           [buffer-dc (viewport-buffer-dc viewport)]
           [w (viewport-width viewport)]
           [h (viewport-height viewport)])
      (rec draw-viewport/color
        (case-lambda
          [(color)
           (let ([new-pen (send mred:the-pen-list find-or-create-pen color 1 'solid)]
                 [new-brush (send mred:the-brush-list find-or-create-brush color 'solid)]
                 [old-pen (send dc get-pen)]
                 [old-brush (send dc get-brush)])
             (send dc set-pen new-pen)
             (send dc set-brush new-brush)
             (send buffer-dc set-pen new-pen)
             (send buffer-dc set-brush new-brush)
             (send dc draw-rectangle 0 0 w h)
             (send buffer-dc draw-rectangle 0 0 w h)
             (send dc set-pen old-pen)
             (send buffer-dc set-pen old-pen)
             (send dc set-brush old-brush)
             (send buffer-dc set-brush old-brush))]
          [() (draw-viewport/color (make-rgb 0 0 0))])))))

(define flip-viewport
  (lambda (viewport)
    (let* ([dc (viewport-dc viewport)]
           [dc2 (viewport-buffer-dc viewport)]
           [w (viewport-width viewport)]
           [h (viewport-height viewport)])
      (lambda ()
        (let ([pen (send dc get-pen)]
              [pen2 (send dc2 get-pen)]
              [brush (send dc get-brush)]
              [brush2 (send dc2 get-brush)])
          (send dc set-pen xor-pen)
          (send dc2 set-pen xor-pen)
          (send dc set-brush xor-brush)
          (send dc2 set-brush xor-brush)
          (send dc draw-rectangle 0 0 w h)
          (send dc2 draw-rectangle 0 0 w h)
          (send dc set-pen pen)
          (send dc2 set-pen pen2)
          (send dc set-brush brush)
          (send dc2 set-brush brush2))))))

(define close-viewport
  (lambda (viewport)
    (set! global-viewport-list 
          (let loop ([l global-viewport-list])
            (cond
              [(null? l) '()]
              [(eq? (car l) viewport) (cdr l)]
              [else (cons (car l) (loop (cdr l)))])))
    (send (viewport-frame viewport) show #f)
    (send (viewport-canvas viewport) show #f)
    (when (null? global-viewport-list)
      (send open-frames-timer stop))))

(define open-graphics
  (lambda ()
    (set! graphics-flag #t)))

(define close-graphics 
  (lambda ()
    (map close-viewport global-viewport-list)
    (set! graphics-flag #f)
    (set! global-viewport-list '())
    (send open-frames-timer stop)))

(define graphics-open? (lambda () graphics-flag))

(define make-rgb
  (lambda (red green blue)
    (when (or (< red 0.) (< blue 0.) (< green 0.)
              (> red 1.) (> blue 1.) (> green 1.))
      (error 'make-rgb
             "all color indices should be in [0.0, 1.0]; provided ~s"
             (list red green blue)))
    (let* ([convert (lambda (num) (inexact->exact (round (* 255 num))))]
           [nred (convert red)]
           [ngreen (convert green)]
           [nblue (convert blue)])
      (make-object mred:color% nred ngreen nblue))))

(define make-color make-rgb)

(define (rgb-red rgb) (/ (send rgb red) 255))
(define (rgb-blue rgb) (/ (send rgb blue) 255))
(define (rgb-green rgb) (/ (send rgb green) 255))

(define rgb? (lambda (object) (is-a? object mred:color%)))
(define (color? x)
  (or (rgb? x)
      (not (not (send mred:the-color-database find-color x)))))

(define change-color
  (lambda (index color)
    (vector-set! global-color-vector index color)
    (vector-set! global-pen-vector index (get-pen color))
    (vector-set! global-brush-vector index (get-brush color))))

(define (get-color index)
  (cond
    [(is-a? index mred:color%) index]
    [(string? index) (make-object mred:color% index)]
    [else (vector-ref global-color-vector index)]))

(define get-pen
  (lambda (index)
    (cond
      [(is-a? index mred:pen%) index]
      [(or (string? index) (is-a? index mred:color%))
       (send mred:the-pen-list find-or-create-pen index 1 'solid)]
      [else (vector-ref global-pen-vector index)])))

(define get-brush
  (lambda (index)
    (cond
      [(is-a? index mred:brush%) index]
      [(or (string? index) (is-a? index mred:color%))
       (send mred:the-brush-list find-or-create-brush index 'solid)]
      [else (vector-ref global-brush-vector index)])))

(define pen? (lambda (object) (is-a? object mred:pen%)))
(define brush? (lambda (object) (is-a? object mred:brush%)))

(define display-color-vector
  (lambda ()
    (do
        ([index 0 (+ index 1)])
      ((eq? index 100))
      (display (list (/ (rgb-red (get-color index)) 255)
                     (/ (rgb-green (get-color index)) 255)
                     (/ (rgb-blue (get-color index)) 255))))))

(define make-font
  (lambda (name)
    (cond
      [(eq? name 'large-deco)
       (make-object mred:font% 40 'decorative 'normal 'normal)]
      [(eq? name 'small-roman)
       (make-object mred:font% 12 'roman 'normal 'normal)]
      [(eq? name 'medium-roman)
       (make-object mred:font% 24 'roman 'normal 'normal)]
      [(eq? name 'large-roman)
       (make-object mred:font% 32 'roman 'normal 'normal)]
      [else "no such font ~a; only 'large-deco, 'small-roman, 'medium-roman, and 'large-roman"
            name])))

(define custom-roman
  (lambda (size)
    (make-object mred:font%
      size 'roman 'normal 'normal)))

(define custom-deco
  (lambda (size)
    (make-object mred:font% size 'decorative 'normal 'normal)))

(define set-viewport-pen
  (lambda (viewport pen)
    (send (viewport-canvas viewport) remember-pen pen)
    (let ([pen (get-pen pen)])
      (send (viewport-dc viewport) set-pen pen)
      (send (viewport-buffer-dc viewport) set-pen pen))))

(define set-viewport-brush
  (lambda (viewport brush)
    (send (viewport-canvas viewport) remember-brush brush)
    (let ([brush (get-brush brush)])
      (send (viewport-dc viewport) set-brush brush)
      (send (viewport-buffer-dc viewport) set-brush brush))))

(define set-text-foreground
  (lambda (viewport color)
    (let ([color (get-color color)])
      (send (viewport-dc viewport) set-text-foreground color)
      (send (viewport-buffer-dc viewport) set-text-foreground color))))

(define set-text-background
  (lambda (viewport color)
    (let ([color (get-color color)])
      (send (viewport-dc viewport) set-text-background color)
      (send (viewport-buffer-dc viewport) set-text-background color))))

(define set-viewport-font
  (lambda (viewport font)
    (send (viewport-dc viewport) set-font font)
    (send (viewport-buffer-dc viewport) set-font font)))

(define set-viewport-background
  (lambda (viewport color)
    (send (viewport-dc viewport) set-background color)
    (send (viewport-buffer-dc viewport) set-background color)))

(define set-viewport-logical-function
  (lambda (viewport logical-function)
    (send (viewport-dc viewport) set-logical-function logical-function)
    (send (viewport-buffer-dc viewport) set-logical-function
          logical-function)))

(define white (make-rgb 1 1 1))
(define black (make-rgb 0 0 0))
(define red (make-rgb 1 0 0))
(define green (make-rgb 0 1 0))
(define blue (make-rgb 0 0 1))
(define white-pen (get-pen white))
(define black-pen (get-pen black))
(define red-pen (get-pen red))
(define blue-pen (get-pen blue))
(define green-pen (get-pen green))
(define white-brush (get-brush white))
(define black-brush (get-brush black))
(define red-brush (get-brush red))
(define green-brush (get-brush green))
(define blue-brush (get-brush blue))

(define invisi-pen (send mred:the-pen-list find-or-create-pen "WHITE" 0 'transparent))
(define invisi-brush (send mred:the-brush-list find-or-create-brush "WHITE" 'transparent))

(define xor-pen (send mred:the-pen-list find-or-create-pen "BLACK" 1 'xor))
(define xor-brush (send mred:the-brush-list find-or-create-brush "BLACK" 'xor))

(define draw-it (lambda (draw flip clear) (draw)))
(define flip-it (lambda (draw flip clear) (flip)))
(define clear-it (lambda (draw flip clear) (clear)))

(define make-draw-proc
  (lambda (get-pen-name set-pen-name 
                        get-current-pen-name set-viewport-pen white-pen)
    (lambda (viewport) 
      (let* ([vdc (viewport-dc viewport)]
             [vbdc (viewport-buffer-dc viewport)])
        (lambda (color go)
          (let ([orig (and color
                           (begin0
                             (send/proc2 (viewport-canvas viewport) 
                                         get-current-pen-name)
                             (set-viewport-pen viewport (get-color color))))])
            (go (lambda (draw)
                  (let ([pen (send vdc get-pen)]
                        [brush (send vdc get-brush)])
                    (send vdc set-brush xor-brush)
                    (send vbdc set-brush xor-brush)
                    (send vdc set-pen xor-pen)
                    (send vbdc set-pen xor-pen)
                    (draw)
                    (send vdc set-brush brush)
                    (send vbdc set-brush brush)
                    (send vdc set-pen pen)
                    (send vbdc set-pen pen)))
                (lambda (draw)
                  (let ([pen (send/proc vdc get-pen-name)])
                    (send/proc vdc set-pen-name white-pen)
                    (send/proc vbdc set-pen-name white-pen)
                    (draw)
                    (send/proc vdc set-pen-name pen)
                    (send/proc vbdc set-pen-name pen))))
            (when orig
              (set-viewport-pen viewport orig))))))))

(define make-do-line
  (lambda (go)
    (let ([f (make-draw-proc 'get-pen 'set-pen 
                             'get-current-pen set-viewport-pen white-pen)])
      (lambda (viewport)
        (let ([f (f viewport)])
          (letrec ([the-function
                    (case-lambda
                      [(posn1 posn2) (the-function posn1 posn2 #f)]
                      [(posn1 posn2 color)
                       (f color
                          (lambda (flip clear)
                            (let* ([x1 (posn-x posn1)]
                                   [y1 (posn-y posn1)]
                                   [x2 (posn-x posn2)]
                                   [y2 (posn-y posn2)]
                                   [draw (lambda ()
                                           (send (viewport-dc viewport) 
                                                 draw-line 
                                                 x1 y1 x2 y2)
                                           (send (viewport-buffer-dc viewport)
                                                 draw-line
                                                 x1 y1 x2 y2))])
                              (go draw
                                  (lambda () (flip draw))
                                  (lambda () (clear draw))))))])])
            the-function))))))

(define draw-line (make-do-line draw-it))
(define (clear-line viewport)
  (let ([f ((make-do-line clear-it) viewport)])
    (rec clear-line-viewport
      (lambda (p1 p2)
        (f p1 p2)))))
(define (flip-line viewport)
  (let ([f ((make-do-line flip-it) viewport)])
    (rec flip-line-viewport
      (lambda (p1 p2)
        (f p1 p2)))))

(define (draw/clear/flip ivar)
  (lambda (init-dc viewport p width height)
    (let ([dc (viewport-dc viewport)]
          [buffer-dc (viewport-buffer-dc viewport)])
      (init-dc dc)
      (init-dc buffer-dc)
      (send/proc dc ivar (posn-x p) (posn-y p) width height)
      (send/proc buffer-dc ivar (posn-x p) (posn-y p) width height))))

(define draw/clear/flip-rectangle (draw/clear/flip 'draw-rectangle))
(define draw/clear/flip-ellipse (draw/clear/flip 'draw-ellipse))

(define (draw-arc viewport)
  (check-viewport 'draw-arc viewport)
  (rec draw-arc-viewport
    (case-lambda
      [(p width height start-radians end-radians)
       (draw-arc-viewport p width height start-radians end-radians (make-rgb 0 0 0))]
      [(p width height start-radians end-radians color)
       (check 'draw-arc
              posn? p "posn"
              number? width "number"
              number? height "number"
              number? start-radians "number"
              number? end-radians "number"
              (orp color? number?) color "color or index")
       (let ([dc (viewport-dc viewport)]
             [buffer-dc (viewport-buffer-dc viewport)])
         (send dc set-pen (send mred:the-pen-list find-or-create-pen (get-color color) 1 'solid))
         (send dc set-brush (send mred:the-brush-list find-or-create-brush "BLACK" 'transparent))
         (send buffer-dc set-pen (send mred:the-pen-list find-or-create-pen (get-color color) 1 'solid))
         (send buffer-dc set-brush (send mred:the-brush-list find-or-create-brush "BLACK" 'transparent))
         (send dc draw-arc (posn-x p) (posn-y p) width height start-radians end-radians)
         (send buffer-dc draw-arc (posn-x p) (posn-y p) width height start-radians end-radians))])))

(define (draw-solid-arc viewport)
  (check-viewport 'draw-solid-arc viewport)
  (rec draw-arc-viewport
    (case-lambda
      [(p width height start-radians end-radians)
       (draw-arc-viewport p width height (make-rgb 0 0 0))]
      [(p width height start-radians end-radians color)
       (check 'draw-solid-arc
              posn? p "posn"
              number? width "number"
              number? height "number"
              number? start-radians "number"
              number? end-radians "number"
              (orp color? number?) color "color or index")
       (let ([dc (viewport-dc viewport)]
             [buffer-dc (viewport-buffer-dc viewport)])
         (send dc set-pen (send mred:the-pen-list find-or-create-pen (get-color color) 1 'solid))
         (send dc set-brush (send mred:the-brush-list find-or-create-brush (get-color color) 'solid))
         (send buffer-dc set-pen (send mred:the-pen-list find-or-create-pen (get-color color) 1 'solid))
         (send buffer-dc set-brush (send mred:the-brush-list find-or-create-brush (get-color color) 'solid))
         (send dc draw-arc (posn-x p) (posn-y p) width height start-radians end-radians)
         (send buffer-dc draw-arc (posn-x p) (posn-y p) width height start-radians end-radians))])))

(define (draw-rectangle viewport)
  (check-viewport 'draw-rectangle viewport)
  (rec draw-rectangle-viewport
    (case-lambda 
      [(p width height) (draw-rectangle-viewport p width height (make-rgb 0 0 0))]
      [(p width height color)
       (check 'draw-rectangle
              posn? p "posn"
              number? width "number"
              number? height "number"
              (orp color? number?) color "color or index")
       (draw/clear/flip-rectangle
        (lambda (dc)
          (send dc set-pen (send mred:the-pen-list find-or-create-pen (get-color color) 1 'solid))
          (send dc set-brush (send mred:the-brush-list find-or-create-brush "BLACK" 'transparent)))
        viewport p width height)])))

(define (draw-solid-rectangle viewport)
  (check-viewport 'draw-solid-rectangle viewport)
  (rec draw-solid-rectangle-viewport
    (case-lambda 
      [(p width height) (draw-solid-rectangle-viewport p width height (make-rgb 0 0 0))]
      [(p width height color)
       (check 'draw-solid-rectangle
              posn? p "posn"
              number? width "number"
              number? height "number"
              (orp color? number?) color "color or index")
       (draw/clear/flip-rectangle
        (lambda (dc)
          (send dc set-pen (send mred:the-pen-list find-or-create-pen (get-color color) 1 'solid))
          (send dc set-brush (send mred:the-brush-list find-or-create-brush (get-color color) 'solid)))
        viewport p width height)])))

(define (flip-rectangle viewport)
  (check-viewport 'flip-rectangle viewport)
  (rec flip-rectangle-viewport
    (case-lambda 
      [(p width height) (flip-rectangle-viewport p width height (make-rgb 0 0 0))]
      [(p width height color)
       (check 'flip-rectangle
              posn? p "posn"
              number? width "number"
              number? height "number"
              (orp color? number?) color "color or index")
       (draw/clear/flip-rectangle
        (lambda (dc)
          (send dc set-pen (send mred:the-pen-list find-or-create-pen (get-color color) 1 'xor))
          (send dc set-brush (send mred:the-brush-list find-or-create-brush "BLACK" 'transparent)))
        viewport p width height)])))

(define (flip-solid-rectangle viewport)
  (check-viewport 'flip-solid-rectangle viewport)
  (rec flip-solid-rectangle-viewport
    (case-lambda 
      [(p width height) (flip-solid-rectangle-viewport p width height (make-rgb 0 0 0))]
      [(p width height color)
       (check 'flip-solid-rectangle
              posn? p "posn"
              number? width "number"
              number? height "number"
              (orp color? number?) color "color or index")
       (draw/clear/flip-rectangle
        (lambda (dc)
          (send dc set-pen (send mred:the-pen-list find-or-create-pen "BLACK" 1 'transparent))
          (send dc set-brush (send mred:the-brush-list find-or-create-brush (get-color color) 'xor)))
        viewport p width height)])))

(define (draw-ellipse viewport)
  (check-viewport 'draw-ellipse viewport)
  (rec draw-ellipse-viewport
    (case-lambda 
      [(p width height) (draw-ellipse-viewport p width height (make-rgb 0 0 0))]
      [(p width height color)
       (check 'draw-ellipse
              posn? p "posn"
              number? width "number"
              number? height "number"
              (orp color? number?) color "color or index")
       (draw/clear/flip-ellipse
        (lambda (dc)
          (send dc set-pen (send mred:the-pen-list find-or-create-pen (get-color color) 1 'solid))
          (send dc set-brush (send mred:the-brush-list find-or-create-brush "BLACK" 'transparent)))
        viewport p width height)])))

(define (draw-solid-ellipse viewport)
  (check-viewport 'draw-solid-ellipse viewport)
  (rec draw-solid-ellipse-viewport
    (case-lambda 
      [(p width height) (draw-solid-ellipse-viewport p width height (make-rgb 0 0 0))]
      [(p width height color)
       (check 'draw-solid-ellipse
              posn? p "posn"
              number? width "number"
              number? height "number"
              (orp color? number?) color "color or index")
       (draw/clear/flip-ellipse
        (lambda (dc)
          (send dc set-pen (send mred:the-pen-list find-or-create-pen (get-color color) 1 'solid))
          (send dc set-brush (send mred:the-brush-list find-or-create-brush (get-color color) 'solid)))
        viewport p width height)])))

(define (flip-ellipse viewport)
  (check-viewport 'flip-ellipse viewport)
  (rec flip-ellipse-viewport
    (case-lambda 
      [(p width height) (flip-ellipse-viewport p width height (make-rgb 0 0 0))]
      [(p width height color)
       (check 'flip-ellipse
              posn? p "posn"
              number? width "number"
              number? height "number"
              (orp color? number?) color "color or index")
       (draw/clear/flip-ellipse
        (lambda (dc)
          (send dc set-pen (send mred:the-pen-list find-or-create-pen (get-color color) 1 'xor))
          (send dc set-brush (send mred:the-brush-list find-or-create-brush "BLACK" 'transparent)))
        viewport p width height)])))

(define (flip-solid-ellipse viewport)
  (check-viewport 'flip-solid-rectangle viewport)
  (rec flip-solid-ellipse-viewport
    (case-lambda 
      [(p width height) (flip-solid-ellipse-viewport p width height (make-rgb 0 0 0))]
      [(p width height color)
       (check 'flip-solid-ellipse
              posn? p "posn"
              number? width "number"
              number? height "number"
              (orp color? number?) color "color or index")
       (draw/clear/flip-ellipse
        (lambda (dc)
          (send dc set-pen (send mred:the-pen-list find-or-create-pen "BLACK" 1 'transparent))
          (send dc set-brush (send mred:the-brush-list find-or-create-brush (get-color color) 'xor)))
        viewport p width height)])))

(define (clear-rectangle viewport)
  (check-viewport 'clear-rectangle viewport)
  (rec clear-rectangle-viewport
    (lambda (p width height)
      (check 'clear-rectangle
             posn? p "posn"
             number? width "number"
             number? height "number")
      (draw/clear/flip-rectangle
       (lambda (dc)
         (send dc set-pen (send mred:the-pen-list find-or-create-pen "WHITE" 1 'solid))
         (send dc set-brush (send mred:the-brush-list find-or-create-brush "BLACK" 'transparent)))
       viewport p width height))))

(define (clear-solid-rectangle viewport)
  (check-viewport 'clear-solid-rectangle viewport)
  (rec clear-solid-rectangle-viewport
    (lambda (p width height)
      (check 'clear-solid-rectangle
             posn? p "posn"
             number? width "number"
             number? height "number")
      (draw/clear/flip-rectangle
       (lambda (dc)
         (send dc set-pen (send mred:the-pen-list find-or-create-pen "WHITE" 1 'solid))
         (send dc set-brush (send mred:the-brush-list find-or-create-brush "WHITE" 'solid)))
       viewport p width height))))

(define (clear-ellipse viewport)
  (check-viewport 'clear-ellipse viewport)
  (rec clear-ellipse-viewport
    (lambda (p width height)
      (check 'clear-ellipse
             posn? p "posn"
             number? width "number"
             number? height "number")
      (draw/clear/flip-ellipse
       (lambda (dc)
         (send dc set-pen (send mred:the-pen-list find-or-create-pen "WHITE" 1 'solid))
         (send dc set-brush (send mred:the-brush-list find-or-create-brush "BLACK" 'transparent)))
       viewport p width height))))

(define (clear-solid-ellipse viewport)
  (check-viewport 'clear-solid-ellipse viewport)
  (rec clear-solid-ellipse-viewport
    (lambda (p width height)
      (check 'clear-solid-ellipse
             posn? p "posn"
             number? width "number"
             number? height "number")
      (draw/clear/flip-ellipse
       (lambda (dc)
         (send dc set-pen (send mred:the-pen-list find-or-create-pen "WHITE" 1 'solid))
         (send dc set-brush (send mred:the-brush-list find-or-create-brush "WHITE" 'solid)))
       viewport p width height))))

(define make-do-pointlist
  (lambda (go name get-pen-name set-pen-name
              get-current-pen-name set-viewport-pen white-pen
              get-brush-name set-brush-name invisi-brush)
    (let ([f (make-draw-proc get-pen-name set-pen-name 
                             get-current-pen-name set-viewport-pen white-pen)])
      (lambda (viewport)
        (let ([f (f viewport)]
              [vdc (viewport-dc viewport)]
              [vbdc (viewport-buffer-dc viewport)])
          (letrec ([the-function
                    (case-lambda
                      [(posns offset) (the-function posns offset #f)]
                      [(posns offset color)
                       (f color
                          (lambda (flip clear)
                            (let* ([points (map (lambda (p)
                                                  (make-object mred:point% (posn-x p) (posn-y p)))
                                                posns)]
                                   [x (posn-x offset)]
                                   [y (posn-y offset)]
                                   [orig (send/proc vdc get-brush-name)]
                                   [draw (lambda ()
                                           (send/proc vdc set-brush-name
                                                      invisi-brush)
                                           (send/proc vbdc set-brush-name
                                                      invisi-brush)
                                           (send/proc 
                                            (viewport-dc viewport) name
                                            points x y)
                                           (send/proc 
                                            (viewport-buffer-dc viewport) name
                                            points x y)
                                           (send/proc vdc set-brush-name orig)
                                           (send/proc vbdc set-brush-name
                                                      orig))])
                              (go draw
                                  (lambda () (flip draw))
                                  (lambda () (clear draw))))))])])
            the-function))))))

(define make-do-polygon
  (lambda (go)
    (make-do-pointlist go 'draw-polygon 'get-pen 'set-pen 
                       'get-current-pen set-viewport-pen white-pen
                       'get-brush 'set-brush invisi-brush)))

(define make-do-solid-polygon
  (lambda (go)
    (make-do-pointlist go 'draw-polygon 'get-brush 'set-brush  
                       'get-current-brush set-viewport-brush white-brush
                       'get-pen 'set-pen invisi-pen)))

(define draw-polygon (make-do-polygon draw-it))
(define (clear-polygon viewport)
  (let ([f ((make-do-polygon clear-it) viewport)])
    (rec clear-polygon-viewport
      (lambda (posns offset)
        (f posns offset)))))
(define (flip-polygon viewport)
  (let ([f ((make-do-polygon flip-it) viewport)])
    (rec flip-polygon-viewport
      (lambda (posns offset)
        (f posns offset)))))

(define draw-solid-polygon (make-do-solid-polygon draw-it))
(define (clear-solid-polygon viewport)
  (let ([f ((make-do-solid-polygon clear-it) viewport)])
    (rec clear-solid-polygon-viewport
      (lambda (posns offset)
        (f posns offset)))))
(define (flip-solid-polygon viewport)
  (let ([f ((make-do-solid-polygon flip-it) viewport)])
    (rec flip-solid-polygon-viewport
      (lambda (posns offset)
        (f posns offset)))))

(define make-do-pixel
  (lambda (go)
    (let ([f (make-draw-proc 'get-pen 'set-pen  
                             'get-current-pen set-viewport-pen white-pen)])
      (lambda (viewport)
        (let ([f (f viewport)])
          (letrec ([the-function
                    (case-lambda
                      [(posn) (the-function posn #f)]
                      [(posn color)
                       (f color
                          (lambda (flip clear)
                            (let* ([x (posn-x posn)]
                                   [y (posn-y posn)]
                                   [draw (lambda ()
                                           (send 
                                            (viewport-dc viewport) draw-point
                                            x y)
                                           (send 
                                            (viewport-buffer-dc viewport)
                                            draw-point 
                                            x y))])
                              (go draw 
                                  (lambda () (flip draw)) 
                                  (lambda () (clear draw))))))])])
            the-function))))))

(define draw-pixel (make-do-pixel draw-it))
(define (clear-pixel viewport)
  (let ([f ((make-do-pixel clear-it) viewport)])
    (rec clear-pixel-viewport
      (lambda (posns offset)
        (f posns offset)))))
(define (flip-pixel viewport)
  (let ([f ((make-do-pixel flip-it) viewport)])
    (rec flip-pixel-viewport
      (lambda (posns offset)
        (f posns offset)))))

(define string-functions
  (lambda (string-op)
    (letrec ([outer-function
              (case-lambda
                [(viewport) (outer-function viewport default-font)]
                [(viewport font)
                 (letrec ([the-function
                           (case-lambda
                             [(posn text) (the-function posn text #f)]
                             [(posn text color)
                              (let*-values ([(dc) (viewport-dc viewport)]
                                            [(x) (posn-x posn)]
                                            [(w h d a) (send dc get-text-extent "X" font)]
                                            [(y) (- (posn-y posn) (- h d))]
                                            [(buffer) (viewport-buffer-dc viewport)]
                                            [(string-create)
                                             (lambda ()
                                               (send dc draw-text text x y)
                                               (send buffer draw-text text x y))])
                                (cond
                                  [(eq? string-op 'draw)
                                   (when color
                                     (set-text-foreground viewport color))
                                   (set-viewport-font viewport font)
                                   (send dc draw-text text x y)
                                   (send buffer draw-text text x y)]
                                  [(eq? string-op 'flip)
                                   (when color
                                     (set-text-foreground viewport color))
                                   (set-viewport-font viewport font)
                                   (string-create)]
                                  [(eq? string-op 'clear)
                                   (set-text-foreground viewport white)
                                   (set-viewport-font viewport font)
                                   (string-create)
                                   (set-text-foreground viewport black)]))])])
                   the-function)])])
      outer-function)))

(define draw-string (string-functions 'draw))
(define (clear-string viewport)
  (let ([f ((string-functions 'clear) viewport)])
    (rec clear-string-viewport
      (lambda (posns offset)
        (f posns offset)))))
(define (flip-string viewport)
  (let ([f ((string-functions 'flip) viewport)])
    (rec flip-string-viewport
      (lambda (posns offset)
        (f posns offset)))))

(define get-string-size
  (case-lambda
    [(viewport) (get-string-size viewport default-font)]
    [(viewport font)
     (lambda (text)
       (let-values ([(w h d a) (send (viewport-dc viewport) get-text-extent text font)])
         (list w h)))]))

(define get-color-pixel 
  (lambda (viewport)
    (lambda (posn)
      (let ([c (make-object mred:color%)]
            [x (posn-x posn)]
            [y (posn-y posn)])
        (unless (send (viewport-buffer-dc viewport) get-pixel x y c)
          (error 'get-color-pixel "specified point is out-of-range"))
        c))))

(define get-pixel 
  (lambda (viewport)
    (lambda (posn)
      (let ([c (make-object mred:color%)]
            [x (posn-x posn)]
            [y (posn-y posn)])
        (unless (send (viewport-buffer-dc viewport) get-pixel x y c)
          (error 'get-pixel "specified point is out-of-range"))
        (if (or (< (send c blue) 255)
                (< (send c red) 255)
                (< (send c green) 255))
            1
            0)))))

(define (test-pixel viewport)
  (lambda (color)
    (let ([c (make-object mred:color%)])
      (send (viewport-buffer-dc viewport) try-color color c)
      c)))

(define draw-pixmap-posn
  (lambda (filename [type 'unknown/mask])
    (check 'draw-pixmap-posn
           string? filename "filename"
           (lambda (x) (memq x '(gif xbm xpm bmp pict unknown unknown/mask gif/mask))) type "file type symbol")
    (let* ([bitmap (make-object mred:bitmap% filename type)])
      (lambda (viewport)
        (check 'draw-pixmap-posn
               viewport? viewport "viewport")
        (lambda (posn [color #f])
          (check 'draw-pixmap-posn
                 posn? posn "posn"
                 (orp not color?) color (format "color or ~e" #f))
          (when color
            (set-viewport-pen viewport (get-color color)))
          (let ([x (posn-x posn)]
                [y (posn-y posn)])
            (send (viewport-dc viewport) draw-bitmap bitmap x y 'solid black-color (send bitmap get-loaded-mask))
            (send (viewport-buffer-dc viewport) draw-bitmap bitmap x y 'solid black-color (send bitmap get-loaded-mask))))))))

(define draw-pixmap
  (lambda (viewport)
    (check 'draw-pixmap
           viewport? viewport "viewport")
    (lambda (filename p [color #f])
      (check 'draw-pixmap
             (andp string? file-exists?) filename "filename"
             posn? p "posn"
             (orp not color?) color (format "color or ~e" #f))
      (((draw-pixmap-posn filename 'unknown) viewport) p color))))

(define copy-viewport 
  (lambda (source target)
    (check 'copy-viewport
           viewport? source "viewport"
           viewport? target "viewport")
    (let* ([source-bitmap (viewport-bitmap source)]
           [target-dc (viewport-dc target)]
           [target-buffer-dc (viewport-buffer-dc target)])
      (send target-dc draw-bitmap source-bitmap 0 0)
      (send target-buffer-dc draw-bitmap source-bitmap 0 0))))

(define save-pixmap
  (lambda (viewport)
    (check 'save-pixmap
           viewport? viewport "viewport")
    (lambda (filename [kind 'xpm])
      (check 'save-pixmap
             (andp string? (orp relative-path? absolute-path?)) filename "filename"
             (lambda (x) (memq x '(xpm xbm bmp pict))) kind "file type symbol")
      (let ([bm (viewport-bitmap viewport)])
        (send bm save-file filename kind)))))

(define sixlib-eventspace #f)

(define make-open-viewport
  (lambda (name show?)
    (unless sixlib-eventspace
      (set! sixlib-eventspace 
            (parameterize ([uncaught-exception-handler
                            (lambda (x)
                              ((error-display-handler)
                               (format "internal error in graphics library: ~a"
                                       (if (exn? x)
                                           (exn-message x)
                                           (format "~e" x))))
                              ((error-escape-handler)))])
              (mred:make-eventspace))))
    (letrec ([open-viewport
              (case-lambda
                [(label point) 
                 (cond
                   [(posn? point) (open-viewport label (posn-x point) (posn-y point))]
                   [(and (list? point) (= (length point) 2))
                    (open-viewport label (car point) (cadr point))]
                   [else (error name "bad argument ~s" point)])]
                [(label width height)
                 (cond
                   [graphics-flag
                    (let*
                        ([frame
                          (parameterize ([mred:current-eventspace sixlib-eventspace])
                            (make-object sixlib-frame%
                              label #f width height))]
                         [panel (make-object mred:vertical-panel% frame)]
                         [canvas (make-object sixlib-canvas% panel)]
                         [_ (begin
                              (send canvas min-height height)
                              (send canvas min-width width))]
                         [dc (send canvas get-dc)]
                         [buffer-dc (make-object mred:bitmap-dc%)]
                         [viewport (make-viewport label canvas)]
                         [ml (event-receiver)]
                         [kl (event-receiver)])
                      (send panel set-alignment 'center 'center)
                      (send frame set-canvas canvas)
                      (send canvas set-viewport viewport)
                      (send canvas set-dc dc)
                      (send canvas set-buffer-dc buffer-dc)
                      (send canvas set-geometry width height)
                      (send canvas set-mouse-listener ml)
                      (send canvas set-key-listener kl)
                      (when show? 
                        (send frame show #t)
                        (send canvas focus))
                      (set-text-foreground viewport black)
                      (set-text-background viewport white)
                      (set-viewport-background viewport white)
                      (set-viewport-pen viewport black-pen)
                      (set-viewport-brush viewport black-brush)
                      ((clear-viewport viewport))
                      (when (null? global-viewport-list)
                        (send open-frames-timer start 100000000))
                      (set! global-viewport-list (cons viewport global-viewport-list))
                      viewport)]
                   [else (error "graphics not open")])])])
      open-viewport)))

(define open-viewport (make-open-viewport 'open-viewport #t))
(define open-pixmap (make-open-viewport 'open-pixmap #f))

(define (default-display-is-color?) (mred:is-color-display?))

(define position-display
  (lambda (viewport counter)
    (cond
      [(equal? counter 0) '()]
      [else (begin 
              (display (query-mouse-posn viewport))
              (position-display viewport (- counter 1)))])))


(define create-cmap
  (lambda ()
    (do ([index 0 (+ 1 index)])
      ((> index 20))
      (let* ([r (* 0.05 index)]
             [b (- 1 r)]
             [g (- 1 r)])
        (change-color index (make-rgb r g b))))))

(define viewport->snip
  (lambda (viewport)
    (let ([orig-bitmap (send (viewport-canvas viewport) get-bitmap)]
          [orig-dc (viewport-buffer-dc viewport)])
      (let* ([h (send orig-bitmap get-height)]
             [w (send orig-bitmap get-width)]
             [new-bitmap (make-object mred:bitmap% w h)]
             [tmp-mem-dc (make-object mred:bitmap-dc%)])
        (send tmp-mem-dc set-bitmap new-bitmap)
        (send tmp-mem-dc draw-bitmap (send orig-dc get-bitmap) 0 0)
        (send tmp-mem-dc set-bitmap #f)
        (let ([snip (make-object mred:image-snip%)])
          (send snip set-bitmap new-bitmap)
          snip)))))

(create-cmap)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;                                                             ;;;
;;;                        ERROR CHECKING                       ;;;
;;;                                                             ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; check-viewport : symbol TST -> void
(define (check-viewport f-name obj)
  (unless (viewport? obj)
    (error f-name "expected viewport as first argument, got ~e" obj)))

;; (define-type arg/pred/name-list (list* (TST -> bool) TST string arg/pred/name-list))
;; check : (symbol arg/pred/name-list *-> void)
(define (check f-name . in-args)
  (let loop ([args in-args]
             [n 0])
    (cond
      [(null? args) (void)]
      [else (let ([pred? (car args)]
                  [val (cadr args)]
                  [name (caddr args)])
              (unless (pred? val)
                (error f-name "expected ~a as arg ~a, got: ~e, all args: ~a"
                       name n val
                       (let loop ([args in-args])
                         (cond
                           [(null? args) ""]
                           [else (string-append (format "~e" (cadr args))
                                                " "
                                                (loop (cdddr args)))]))))
              (loop (cdddr args)
                    (+ n 1)))])))

(define (orp . preds)
  (lambda (TST)
    (ormap (lambda (p) (p TST)) preds)))

(define (andp . preds)
  (lambda (TST)
    (andmap (lambda (p) (p TST)) preds)))
