#lang racket/base

;; Code used to generate card images, just in case the cards
;; need to be regenarted in a similar way. This code relies
;; on fonts that were available on a Mac OS X 10.9 machine
;; at the time that the cards were generated.

(module generate racket
  (require racket/draw)
  
  (define W 71)
  (define H 96)
  
  (define IW 41)
  (define IH 73)
  
  ;; Numbers: Lucida Grande
  ;; Letters: Helvetica
  ;; Suits: Osaka
  
  (define (extract-color)
    (define bm0 (read-bitmap "card-1-1.png"))
    (define c (new color%))
    (send (send bm0 make-dc) get-pixel (sub1 (sub1 W)) 20 c)
    (list
     (send c red)
     (send c green)
     (send c blue)))

  ;; Extract the core of a face card so that it can be re-decorated.
  ;; some by-hand editing may be needed to generate images
  ;; in "faces-clean". Create initial cards by scaling up the originals
  ;; such as ImageMagick's `-adaptive-resize` mode.
  (define (extract-faces)
    (define D 0)
    (define dh 2)
    (define tw (* 2 (+ D IW)))
    (define th (- (* 2 (+ D IH)) (* 2 dh)))
    (define T 200)
    
    (for* ([val (in-range 10 13)]
           [suit 4])

      (define (white-out bstr x y)
        (define Zx (cond
                    [(and (= val 11) (= suit 3)) 26]
                    [(and (= val 12) (= suit 0)) 28]
                    [(and (= val 12) (= suit 2)) 28]
                    [else 32]))
        (define Zy 40)
        (when (or (and (< 0 x Zx) (< 0 y Zy))
                  (and (< (- tw Zx) x tw) (< (- th Zy) y th)))
          (define p (* 4 (+ x (* y tw))))
          (define r (bytes-ref bstr (+ p 1)))
          (define g (bytes-ref bstr (+ p 2)))
          (define b (bytes-ref bstr (+ p 3)))
          (unless (and (r . > . T)
                       (g . > . T)
                       (b . > . T))
            (bytes-set! bstr (+ p 1) 255)
            (bytes-set! bstr (+ p 2) 255)
            (bytes-set! bstr (+ p 3) 255)
            (white-out bstr (- x 1) y)
            (white-out bstr (+ x 1) y)
            (white-out bstr x (- y 1))
            (white-out bstr x (+ y 1)))))

      (define old-bm (read-bitmap
                      (collection-file-path (format "card-~a-~a@2x.png" val suit) "games/cards/hicolor")))
      (define bm2 (make-bitmap tw th))
      (define dc (send bm2 make-dc))
      (define dx (quotient (- W (+ D IW)) 2))
      (define dy (quotient (- H (+ D IH)) 2))
      (send dc draw-bitmap-section old-bm 0 0 (* 2 dx) (+ (* 2 dy) dh) tw th)
      (define bstr (make-bytes (* tw th 4)))
      (send bm2 get-argb-pixels 0 0 tw th bstr)
      (define P 16)
      (white-out bstr P P)
      (white-out bstr (- tw P) (- th P))
      (send bm2 set-argb-pixels 0 0 tw th bstr)
      (send bm2 save-file (format "faces/face-~a-~a.png" val suit) 'png)))

  (define (card suit val)
    (define bm (make-bitmap W H #:backing-scale 2))
    (define dc (send bm make-dc))

    (define clip-path (new dc-path%))
    (define R 4)
    (send clip-path move-to R 0)
    (send clip-path arc (- W R) 0 R R (* pi 1/2) 0 #f)
    (send clip-path arc (- W R) (- H R) R R 0 (* pi -1/2) #f)
    (send clip-path arc 0 (- H R) R R (* pi -1/2) (* pi -1) #f)
    (send clip-path arc 0 0 R R (* pi -1) (* pi -3/2) #f)
    (send clip-path close)
    (define region (new region%))
    (send region set-path clip-path)
    (send dc set-clipping-region region)

    (send dc set-pen "black" 1 'transparent)
    (send dc set-brush (make-color 236 236 186) 'solid)
    (send dc draw-rectangle 0 0 W H)

    (send dc set-smoothing 'smoothed)
    (send dc set-brush "black" 'transparent)
    (define tr (send dc get-transformation))
    (send dc translate -1.0 -1.0)
    (send dc set-pen (make-color 200 200 180) 2 'solid)
    (send dc draw-path clip-path 0 0)
    (send dc set-transformation tr)
    (send dc set-pen (make-color 150 150 100) 2 'solid)
    (send dc draw-path clip-path 0 0)
    (send dc set-smoothing 'aligned)

    (cond
     [(and (not suit) (not val))
      (define dx 4)
      (send dc set-pen "black" 1 'transparent)
      (send dc set-brush (make-color 0 170 0) 'solid)
      (send dc draw-rectangle dx dx (- W dx dx) (- H dx dx))
      (send dc set-clipping-rect dx dx (- W dx dx) (- H dx dx))
      (send dc set-text-foreground (make-color 0 240 0))
      (send dc set-font (make-font #:face "Athelas" #:size 16))
      (define-values (lw lh ld la) (send dc get-text-extent "\u3BB"))
      (define d (/ (- W dx dx) 8))
      (for* ([j (in-range -4 5)]
             [i (in-range 8)])
        (define x (+ dx (* i d)))
        (define y (+ -2 (/ W 2) (* j (+ 1 (/ (- H dx dx) 8)))))
        (define flip? (odd? (+ i j)))
        (send dc draw-text "\u3BB"
              (+ x (if flip? 0 lw))
              (+ y (if flip? 0 lh))
              #f 0 (if flip? 0 pi)))]
     [else
      (define pip
        (case suit
          [(0) "\u2663"] ; club
          [(1) "\u2666"] ; diamond
          [(2) "\u2665"] ; heart
          [(3) "\u2660"])) ; spade
      (define color
        (case suit
          [(0 3) "black"]
          [(1 2) (make-color 240 0 0)]))
      (define num
        (case val
          [(0) "A"]
          [(9) "I0"]
          [(10) "J"]
          [(11) "Q"]
          [(12) "K"]
          [else (format "~a" (add1 val))]))
      (define squish
        (case val
          [(9) #e0.9]
          [(10) #e1.1]
          [(11) #e0.8]
          [else 1]))

      (send dc set-text-foreground color)

      (when (val . > . 9)
        (define (get)
          (read-bitmap (format "faces-clean/face-~a-~a.png" val suit)))
        (define old-bm (get))
        (define old-bm2 (get))

        (define odc (send old-bm make-dc))
        (send odc rotate pi)
        (send odc translate (- (send old-bm get-width)) (- (send old-bm get-height)))
        (send odc set-clipping-rect 0 0 (send old-bm get-width) (quotient (send old-bm get-height) 2))
        (send odc draw-bitmap old-bm2 0 0)
        
        (define tr (send dc get-transformation))
        (send dc scale 0.5 0.5)
        (define D 0)
        (define dh 2)
        (define dx (quotient (- W (+ D IW)) 2))
        (define dy (quotient (- H (+ D IH)) 2))
        (send dc draw-bitmap old-bm (* 2 dx) (+ (* 2 dy) dh))
        (send dc set-transformation tr)
        (send dc set-pen (make-color 150 150 100) 1 'solid)
        (send dc draw-rectangle (- dx 1) (- dy 0) (+ IW 2) (+ IH 0)))

      (define number? (<= 1 val 8))

      (define (half first?)
        (send dc set-font (make-font #:face (if number?
                                                "Lucida Grande"
                                                "Helvetica")
                                     #:weight 'bold #:size 18))

        (define tr (send dc get-transformation))
        (send dc scale squish 1)
        (send dc draw-text num (/ 1 squish) (if number? 0 4) #t)
        (define-values (nw nh nd na) (send dc get-text-extent num))
        (send dc set-transformation tr)

        (send dc set-font (make-font #:face "Osaka" #:size 12))
        (define-values (spw sph spd spa) (send dc get-text-extent pip))
        (send dc draw-text pip (+ 1 (quotient (- (floor (* squish nw)) spw) 2)) 20)

        (send dc set-font (make-font #:face "Osaka" #:size 16))
        (define-values (pw ph pd pa) (send dc get-text-extent pip))

        (define dx (quotient (- W IW) 2))
        (define dy (quotient (- H IH) 2))
        (define dy2 (+ dy ph (/ (- (/ IH 2) ph ph) 2)))

        (define (pips n)
          (case n
            [(1)
             (when first?
               (cond
                [(and (= val 0) (= suit 3))
                 (define S 6)
                 
                 (define plt (read-bitmap (collection-file-path "PLT-206.png" "icons")))
                 (define w (send plt get-width))
                 (define h (send plt get-height))
                 
                 (define spade (make-bitmap w h))
                 (define sdc (send spade make-dc))
                 (define f (make-font #:face (send (send dc get-font) get-face) #:size (* 4 64)))
                 (define-values (pw ph pd pa) (send sdc get-text-extent pip f))
                 (send sdc set-font f)
                 (send sdc draw-text pip (quotient (- w pw) 2) (quotient (- h ph) 2))
                 
                 (define bstr (make-bytes (* w h 4)))
                 (send plt get-argb-pixels 0 0 w h bstr)
                 (define sbstr (make-bytes (* w h 4)))
                 (send spade get-argb-pixels 0 0 w h sbstr)

                 (for ([i (in-range 0 (* w h 4) 4)])
                   (define a (quotient (+ (bytes-ref bstr (+ i 1))
                                          (bytes-ref bstr (+ i 2))
                                          (bytes-ref bstr (+ i 3)))
                                       3))
                   (bytes-set! bstr i (bytes-ref sbstr i))
                   (bytes-set! bstr (+ i 1) a)
                   (bytes-set! bstr (+ i 2) a)
                   (bytes-set! bstr (+ i 3) a))
                 (send plt set-argb-pixels 0 0 w h bstr)
                 (define tr (send dc get-transformation))
                 (send dc scale (/ 1 S) (/ 1 S))
                 (send dc draw-bitmap plt
                       (* S (/ (- W (quotient w S)) 2))
                       (* S (/ (- H (quotient h S)) 2)))
                 (send dc set-transformation tr)]
                [else
                 (send dc draw-text pip (quotient (- W pw) 2) (quotient (- H ph) 2))]))]
            [(2)
             (send dc draw-text pip (+ dx (quotient (- IW pw) 2)) dy)]
            [(3)
             (pips 1)
             (pips 2)]
            [(4)
             (send dc draw-text pip dx dy)
             (send dc draw-text pip (- W dx pw) dy)]
            [(5)
             (pips 4)
             (pips 1)]
            [(6)
             (pips 4)
             (when first?
               (send dc draw-text pip dx (quotient (- H ph) 2))
               (send dc draw-text pip (- W dx pw) (quotient (- H ph) 2)))]
            [(7)
             (pips 6)
             (when first?
               (send dc draw-text pip (quotient (- W pw) 2) (+ dy (- (quotient IH 3) (quotient ph 2)))))]
            [(8)
             (pips 4)
             (send dc draw-text pip dx dy2)
             (send dc draw-text pip (- W dx pw) dy2)]
            [(9)
             (pips 8)
             (pips 1)]
            [(10)
             (pips 8)
             (send dc draw-text pip (quotient (- W pw) 2) (+ dy (- (quotient IH 4) (quotient ph 2))))]
            [else
             (send dc set-brush "white" 'solid)
             (send dc set-pen "black" 1 'transparent)
             (send dc draw-rectangle dx (+ dy 2) (- pw 2) (+ ph 3))
             (send dc draw-text pip (- dx 1) (+ dy 2))]))

        (pips (add1 val)))
      
      (half #t)
      (send dc rotate pi)
      (send dc translate (- W) (- H))
      (half #f)])
    
    bm)

  (for* ([s 4]
         [n 13])
    (send (card s n) save-file (format "/tmp/cards/card-~a-~a@2x.png" n s) 'png #:unscaled? #t))

  (send (card #f #f) save-file "/tmp/cards/card-back@2x.png" 'png #:unscaled? #t))
