(module utils mzscheme
  (require sgl/gl-vectors
           sgl
           mzlib/math
           mred
           mzlib/list
           mzlib/etc
           mzlib/class
           mzlib/kw
           "doors.rkt")
  
  (provide door-bm
           magic-door-bm
           locked-door-bm

           door-drawer
           locked-door-drawer
           magic-door-drawer
           open-door-drawer
           
           make-i-player-icon
           make-key-thing-icon)
  
  (define light-black (gl-float-vector 0.0 0.0 0.0 0.25))
  (define green (gl-float-vector 0.0 1.0 0.0 1.0))
  (define yellow (gl-float-vector 1.0 1.0 0.0 1.0))
  (define black (gl-float-vector 0.0 0.0 0.0 1.0))
  (define dark-gray (gl-float-vector 0.2 0.2 0.2 1.0))
  
  (define door-bm 
    (make-object bitmap%
      (build-path (collection-path "games" "checkers") "light.jpg")))
  
  (define (door-drawer game)
    (bitmap->drawer door-bm game))
  
  (define (open-door-drawer game)
    void)

  (define (add-to-door draw)
    (let* ([w (send door-bm get-width)]
           [h (send door-bm get-height)]
           [bm (make-object bitmap% w h)]
           [dc (make-object bitmap-dc% bm)])
      (send dc draw-bitmap door-bm 0 0)
      (draw dc w h)
      (send dc set-bitmap #f)
      bm))

  (define magic-door-bm
    (add-to-door
     (lambda (dc w h)
       (send dc set-font (send the-font-list find-or-create-font 32 'default))
       (send dc set-text-foreground (make-object color% "yellow"))
       (let-values ([(sw sh sd sa) (send dc get-text-extent "\u2605")])
         (send dc draw-text "\u2605" (/ (- w sw) 2) (/ (- h sh) 2))))))

  (define (magic-door-drawer game)
    (bitmap->drawer magic-door-bm game))

  (define locked-door-bm
    (add-to-door
     (lambda (dc w h)
       (send dc set-brush (send the-brush-list find-or-create-brush "black" 'solid))
       (send dc set-pen (send the-pen-list find-or-create-pen "black" 1 'solid))
       (send dc draw-ellipse (/ (- w (* 0.2 h)) 2) (* 0.2 h)
             (* 0.2 h) (* 0.2 h))
       (send dc draw-rectangle (* w 0.45) (* 0.3 h)
             (* 0.1 w) (* 0.3 h)))))

  (define (locked-door-drawer game)
    (bitmap->drawer locked-door-bm game))

  (define (q game)
    (send game with-gl-context
          (lambda () 
            (let ([q (gl-new-quadric)])
              (gl-quadric-draw-style q 'fill)
              (gl-quadric-normals q 'smooth)
              q))))
  
  (define (sphere-dl game color)
    (send game with-gl-context
          (let ([q (q game)])
            (lambda ()
              (let ((list-id (gl-gen-lists 1)))
                (gl-new-list list-id 'compile)
                (gl-material-v 'front-and-back 'ambient-and-diffuse color)
                (gl-sphere q 0.5 20 20)
                (gl-end-list)
                list-id)))))
  
  (define (make-cylinder-dl game color disk?)
    (send game with-gl-context
          (lambda ()
            (let ((list-id (gl-gen-lists 1))
                  (q (q game)))
              (gl-new-list list-id 'compile)
              (gl-material-v 'front-and-back 'ambient-and-diffuse color)
              (gl-cylinder q 0.5 0.5 1.0 20 1)
              (when disk?
                (gl-push-matrix)
                (gl-translate 0 0 1.0)
                (gl-disk q 0.0 0.5 25 1)
                (gl-pop-matrix))
              (gl-end-list)
              list-id))))
  
  (define/kw (make-i-player-icon game
                                 #:optional
                                 [data #f]
                                 #:key
                                 [color green] )
    (let ([shadow-cylinder-dl (make-cylinder-dl game dark-gray #t)]
          [cylinder-dl (make-cylinder-dl game color #f)]
          [sphere-dl (sphere-dl game color)])
      (send game make-player-icon
            (lambda (just-shadow?)
              (with-light
               just-shadow?
               (lambda ()
                 (unless just-shadow?
                   (gl-push-matrix)
                   (gl-translate 0.0 0.0 0.30)
                   (gl-scale 0.25 0.25 0.25)
                   (gl-scale 0.5 0.5 0.5)
                   (gl-call-list sphere-dl)
                   (gl-pop-matrix))
                 (gl-push-matrix)
                 (gl-scale 0.25 0.25 0.5)
                 (gl-scale 0.5 0.5 0.5)
                 (gl-call-list (if just-shadow?
                                   shadow-cylinder-dl
                                   cylinder-dl))
                 (gl-pop-matrix))))
            data)))

  (define (make-key-dl game color) 
    (send game with-gl-context
          (lambda ()
            (let ((list-id (gl-gen-lists 1))
                  (q (q game)))
              (gl-new-list list-id 'compile)
              (gl-material-v 'front-and-back 'ambient-and-diffuse color)
              (gl-push-matrix)
              (gl-translate -0.25 0 0)
              (gl-cylinder q 0.25 0.25 0.2 20 1)
              (gl-cylinder q 0.1 0.1 0.2 20 1)
              (gl-disk q 0.1 0.25 20 2)
              (gl-translate 0 0 0.2)
              (gl-disk q 0.1 0.25 20 2)
              (gl-pop-matrix)
              (gl-push-matrix)
              (gl-translate -0.05 0 0.1)
              (gl-rotate 90 0 1 0)
              (gl-cylinder q 0.1 0.1 0.5 16 1)
              (gl-push-matrix)
              (gl-translate 0 0 0.5)
              (gl-disk q 0 0.1 16 1)
              (gl-pop-matrix)
              (let ([tooth
                     (lambda ()
                       (gl-push-matrix)
                       (gl-rotate 90 1 0 0)
                       (gl-cylinder q 0.05 0.05 0.25 16 1)
                       (gl-translate 0 0 0.25)
                       (gl-disk q 0 0.05 16 1)
                       (gl-pop-matrix))])
                (gl-translate 0 0 0.2)
                (tooth)
                (gl-translate 0 0 0.2)
                (tooth))
              (gl-pop-matrix)      
              (gl-end-list)
              list-id))))

    (define (with-light just-shadow? thunk)
      (unless just-shadow?
        (gl-enable 'light0)
        (gl-light-model-v 'light-model-ambient (gl-float-vector 0.5 0.5 0.5 0.0)))
      (thunk)
      (unless just-shadow?
        (gl-light-model-v 'light-model-ambient (gl-float-vector 1.0 1.0 1.0 0.0))
        (gl-disable 'light0)))

  (define/kw (make-key-thing-icon game
                                  #:optional
                                  [data #f]
                                  #:key
                                  [color yellow])
    (let ([dl (make-key-dl game color)])
      (send game make-thing-icon
            (lambda/kw (#:optional [just-shadow? #f]) 
              (with-light just-shadow? (lambda ()
                                         (gl-scale 0.5 0.5 0.5)
                                         (gl-call-list dl))))
            data))))
