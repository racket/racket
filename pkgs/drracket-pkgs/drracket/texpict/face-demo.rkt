(module face-demo mzscheme
  (require mred
           mzlib/class
           texpict/utils
           texpict/mrpict
           texpict/face)

  (define f (new frame% (label "frame")))

  (define canvas-scale 1)
  
  (define c (new canvas%
                 (parent f)
                 (paint-callback
                  (lambda (x dc) 
                    (send dc set-scale canvas-scale canvas-scale)
                    (send dc set-smoothing 'aligned)
                    (cb dc)))))
  
  
  (define shading-parameters
    (list #:mouth-shading?
          #:eye-shading?
          #:eyebrow-shading?
          #:tongue-shading?
          #:face-background-shading?
          #:teeth?))
  
  (define moods
    '(unhappiest
      unhappier
      unhappy
      sortof-unhappy
      sortof-happy
      happy
      happier
      happiest
      embarrassed
      badly-embarrassed
      mad
      mean
      surprised))
  
  (new button%
       (label "Face Color...")
       (parent f)
       (callback (lambda (b e) 
                   (let ([c (get-color-from-user "Face Color" f face-color)])
                     (when c
                       (set! face-color c)
                       (new-face-callback))))))
  (new choice%
       (label #f)
       (choices (append (map symbol->string moods) (list "Custom")))
       (parent f)
       (callback (lambda (c e) 
                   (let ([v (send c get-selection)])
                     (set! face-mood (and (v . < . (length moods))
                                          (list-ref moods v)))
                     (new-face-callback)))))
  
  (define custom-panel (new group-box-panel% (label "Custom") (parent f)))
  (define hp (new horizontal-panel% (parent custom-panel)))
  (define custom-left-panel (new vertical-panel% (parent hp)))
  (define custom-right-panel (new vertical-panel% (parent hp) (alignment '(left top))))
  
  (define shading-checkboxes
    (map
     (λ (parameter)
       (new check-box%
            (label (format "~a" parameter))
            (parent custom-right-panel)
            (value #t)
            (callback (λ (cb _) (new-face-callback)))))
     shading-parameters))
  
  (let ([choices '(none normal worried angry raised)])
    (new choice%
         (label "Eyebrows")
         (choices (map symbol->string choices)) 
         (parent custom-left-panel)
         (callback (lambda (c e)
                     (set! face-eyebrow-kind (list-ref choices (send c get-selection)))
                     (new-face-callback)))))
  (let ([choices '(plain narrow medium large huge grimace oh tongue)])
    (new choice%
         (label "Mouth")
         (choices (map symbol->string choices)) 
         (parent custom-left-panel)
         (callback (lambda (c e)
                     (set! face-mouth-kind (list-ref choices (send c get-selection)))
                     (new-face-callback)))))
  (new check-box%
       (label "Frown")
       (parent custom-left-panel)
       (callback (lambda (c e)
                   (set! face-frown? (send c get-value))
                   (new-face-callback))))
  (new slider%
       (label "Eye Inset")
       (parent custom-left-panel)
       (min-value -10)
       (max-value 10)
       (init-value 0)
       (callback (lambda (s e)
                   (set! face-eye-inset (send s get-value))
                   (new-face-callback))))
  (new slider%
       (label "Eyebrow Y")
       (parent custom-left-panel)
       (min-value -5)
       (max-value 5)
       (init-value 0)
       (callback (lambda (s e)
                   (set! face-eyebrow-dy (send s get-value))
                   (new-face-callback))))
  (let ([pupils
         (lambda (label setter hi)
           (new slider%
                (label label)
                (parent custom-left-panel)
                (min-value (- hi))
                (max-value hi)
                (init-value 0)
                (callback (lambda (s e)
                            (setter (send s get-value))
                            (new-face-callback)))))])
    (pupils "Pupil X" (lambda (v) (set! face-pupils-dx v)) 10)
    (pupils "Pupil Y" (lambda (v) (set! face-pupils-dy v)) 15))
  
  
  (send custom-panel enable #f)
  
  (dc-for-text-size (send c get-dc))
  
  (define face-color default-face-color)
  (define face-mood (car moods))
  (define face-eyebrow-kind 'none)
  (define face-mouth-kind 'plain)
  (define face-frown? #f)
  (define face-eye-inset 0)
  (define face-eyebrow-dy 0)
  (define face-pupils-dx 0)
  (define face-pupils-dy 0)
  
  (define the-pict (face face-mood face-color))
  
  (define (new-face-callback)
    (send custom-panel enable (not face-mood))
    (set! the-pict (if face-mood
                       (face face-mood face-color)
                       (apply face*
                              face-eyebrow-kind
                              face-mouth-kind
                              face-frown?
                              face-color
                              face-eye-inset
                              face-eyebrow-dy
                              face-pupils-dx
                              face-pupils-dy
                              (apply
                               append
                               (map (λ (kw cb) (list kw (send cb get-value)))
                                    shading-parameters
                                    shading-checkboxes)))))
    (send c on-paint))
  
  (define (cb dc) (draw-pict the-pict dc 0 0))
  
  (send c min-width (inexact->exact (floor (* canvas-scale (pict-width the-pict)))))
  (send c min-height (inexact->exact (floor (* canvas-scale (pict-height the-pict)))))
 
  (send f show #t))
