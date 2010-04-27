
(module gen-tiles mzscheme
  (require mzlib/class
           mred
           mzlib/math)

  (define SIZE 24)

  (define bm (make-object bitmap% SIZE SIZE))
  (define dc (make-object bitmap-dc% bm))

  (define dir (build-path (collection-path "games" "mines")
                          "images"))

  ;; Bomb ----------------------------------------
  
  (define (draw-bomb color fuse?)
    (send dc set-smoothing 'smoothed)
    (send dc set-pen (make-object pen% color 1 'solid))
    (send dc set-brush (make-object brush% color 'solid))
    (send dc draw-ellipse 5 7 14 14)
    (when fuse?
      (send dc set-pen (make-object pen% (make-object color% 100 100 100) 1 'solid))
      (send dc set-brush (make-object brush% "BLACK" 'transparent))
      (send dc draw-arc 12 2 24 14 (* 2/3 pi) pi)))

  (send dc clear)
  (draw-bomb "BLACK" #t)
  (send dc set-pen (make-object pen% "RED" 1 'solid))
  (send dc set-smoothing 'aligned)
  (send dc draw-line 14 0 16 2)
  (send dc draw-line 18 4 20 6)
  (send dc draw-line 18 2 20 0)
  (send bm save-file (build-path dir "bomb.png") 'png)

  (let ([path (make-object dc-path%)])
    (send path move-to 4 0)
    (send path line-to 12 4)
    (send path line-to 22 0)
    (send path line-to 20 12)
    (send path line-to 24 20)
    (send path line-to 20 20)
    (send path line-to 20 24)
    (send path line-to 12 20)
    (send path line-to 0 24)
    (send path line-to 4 18)
    (send path line-to 0 10)
    (send path line-to 6 6)
    (send path close)
    (send path translate -12 -12)
    
    (send dc clear)
    (send dc set-pen (make-object pen% "RED" 1 'solid))
    (send dc set-brush (make-object brush% "RED" 'solid))
    (send dc draw-path path 12 12)
    
    (send path scale 2/3 2/3)
    (send dc set-pen (make-object pen% "ORANGE" 1 'solid))
    (send dc set-brush (make-object brush% "ORANGE" 'solid))
    (send dc draw-path path 12 12)

    (send path scale 1/2 1/2)
    (send dc set-pen (make-object pen% "YELLOW" 1 'solid))
    (send dc set-brush (make-object brush% "YELLOW" 'solid))
    (send dc draw-path path 12 12)

    (void))
  
  (send bm save-file (build-path dir "explode.png") 'png)

  ;; Tiles ----------------------------------------

  (define bg (make-object bitmap% (build-path dir "bg.png")))

  (define (lighter n q)
    (- 255 (floor (* (if (zero? q) 3/4 4/5) (- 255 n)))))
  (define (darker n q)
    (floor (* (if (zero? q) 1/2 4/5) n)))

  (send dc draw-bitmap bg 0 0)
  (let ([c (make-object color%)])
    (let loop ([q 0])
      (unless (= q 2)
        (let loop ([i 0])
          (unless (= i SIZE)
            (let ([adjust
                   (lambda (adj x y)
                     (send dc get-pixel x y c)
                     (send c set 
                           (adj (send c red) q)
                           (adj (send c green) q)
                           (adj (send c blue) q))
                     (send dc set-pixel x y c))])
              (when (<= q i (- SIZE q))
                (adjust lighter q i)
                (unless (zero? i)
                  (adjust lighter i q))
                (adjust darker (- SIZE 1 q) i)
                (unless (= i (- SIZE q))
                  (adjust darker i (- SIZE 1 q)))))
            (loop (add1 i))))
        (loop (add1 q)))))
  
  (send bm save-file (build-path dir "tile.png") 'png)

  (define (bright r g b)
    (min
     (inexact->exact
      (floor
       (sqrt (+ (sqr r) (sqr g) (sqr g)))))
     255))

  (define (xform red green blue)
    (let ([c (make-object color%)])
      (let loop ([i 0])
        (unless (= i SIZE)
          (let loop ([j 0])
            (unless (= j SIZE)
              (send dc get-pixel i j c)
              (let ([r (send c red)]
                    [g (send c green)]
                    [b (send c blue)])
                (send c set 
                      (red r g b)
                      (green r g b)
                      (blue r g b))
                (send dc set-pixel i j c)
                (loop (add1 j)))))
          (loop (add1 i))))))

  (xform (lambda (r g b) r) (lambda (r g b) g) bright)

  (define tile-bm (make-object bitmap% (build-path dir "tile.png")))

  (send bm save-file (build-path dir "lclick-tile.png") 'png)
  
  (send dc draw-bitmap tile-bm 0 0)
  (xform bright (lambda (r g b) g) (lambda (r g b) b))
  (send bm save-file (build-path dir "rclick-tile.png") 'png)

  (define (semi-bright r g b)
    (floor (- 255 (* 2/3 (- 255 r)))))

  (send dc draw-bitmap tile-bm 0 0)
  (xform semi-bright semi-bright semi-bright)
  (send bm save-file (build-path dir "local-tile.png") 'png)

  (define (semi-dim r g b)
    (floor (* 4/5 r)))

  (send dc draw-bitmap tile-bm 0 0)
  (xform semi-dim semi-dim semi-dim)
  (send bm save-file (build-path dir "near-tile.png") 'png)

  ;; Flag -----------------------------------------
  
  (define (draw-flag dc color field?)
    (send dc clear)
    (send dc set-smoothing 'aligned)
    (send dc set-pen (make-object pen% "BLACK" 1 'solid))
    (send dc set-brush (make-object brush% "BLACK" 'solid))
    (send dc draw-rectangle 5 9 2 12)
    (send dc set-pen (make-object pen% color 1 'solid))
    (send dc set-brush (make-object brush% color 'solid))
    (send dc draw-polygon
          (list (make-object point% 5 4)
                (make-object point% 19 9)
                (make-object point% 5 14)))
    (when field?
      (send dc draw-rectangle 7 3 12 7)))

  (let* ([bm2 (make-object bitmap% SIZE SIZE)]
         [dc2 (make-object bitmap-dc% bm2)])
    (draw-flag dc2 "BLACK" #f)
    (send dc2 set-bitmap #f)
    (send bm set-loaded-mask bm2))

  (draw-flag dc "RED" #t)

  (send bm save-file (build-path dir "flag.png") 'png)
  
  )
