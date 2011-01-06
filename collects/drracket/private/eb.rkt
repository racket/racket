#lang racket/base
(require racket/class
         framework/splash
         racket/gui/base)

(provide install-eb)
(define (install-eb)
  (define main-size 260)
  (define pi (atan 0 -1))
  
  (define eli (make-object bitmap% (collection-file-path "eli-purple.jpg" "icons")))
  (define bitmap (make-object bitmap% main-size main-size))
  (define bdc (make-object bitmap-dc% bitmap))
  
  (define outer-color (send the-color-database find-color "darkorange"))
  (define inner-color (send the-color-database find-color "green"))
  (define omega-str "(λ (x) (x x)) (λ (x) (x x)) ")
  (define hebrew-str "  ףוס ןיא  ףוס ןיא")
  
  (define (draw-letter dc cx cy angle radius letter color)
    (let ([x (+ cx (* (cos angle) radius))]
          [y (- cy (* (sin angle) radius))])
      (send bdc set-text-foreground color)
      (send dc draw-text letter x y #f 0 (- angle (/ pi 2)))))
  
  (define (draw-single-loop str dc offset cx cy radius font-size color)
    (send dc set-font (send the-font-list find-or-create-font font-size 'modern))
    (for ([i (in-range 0 (string-length str))])
      (draw-letter dc
                   cx
                   cy
                   (normalize-angle
                    (+ (- (* 2 pi) (* (* 2 pi) (/ (- i 1) (string-length str))))
                       (/ pi 2)
                       offset))
                   radius
                   (substring str i (+ i 1))
                   color)))
  
  (define (normalize-angle angle)
    (cond
      [(<= 0 angle (* 2 pi)) angle]
      [(< angle 0) (normalize-angle (+ angle (* 2 pi)))]
      [else (normalize-angle (- angle (* 2 pi)))]))
  
  (define splash-canvas (get-splash-canvas))
  (define (draw-single-step offset)
    (send bdc draw-bitmap eli 0 0)
    (draw-single-loop omega-str bdc offset (/ main-size 2) (/ main-size 2) 120 32 outer-color)
    (draw-single-loop hebrew-str bdc (+ (- (* 2 pi) offset) (* 2 pi)) (/ main-size 2) (/ main-size 2) 70 20 inner-color)
    (refresh-splash))
  
  (define gc-b
    (with-handlers ([exn:fail? (lambda (x)
                                 (printf "~s\n" (exn-message x))
                                 #f)])
      (let ([b (make-object bitmap% (collection-file-path "recycle.png" "icons"))])
        (cond
          [(send b ok?)
           (let ([gbdc (make-object bitmap-dc% b)]
                 [ebdc (make-object bitmap-dc% eli)]
                 [color1 (make-object color%)]
                 [color2 (make-object color%)]
                 [avg (lambda (x y) (floor (* (/ x 255) y)))]
                 [ox (floor (- (/ main-size 2) (/ (send b get-width) 2)))]
                 [oy (floor (- (/ main-size 2) (/ (send b get-height) 2)))])
             (let loop ([i (send b get-width)])
               (unless (zero? i)
                 (let loop ([j (send b get-height)])
                   (unless (zero? j)
                     (let ([x (- i 1)]
                           [y (- j 1)])
                       (send gbdc get-pixel x y color1)
                       (send ebdc get-pixel (+ x ox) (+ y oy) color2)
                       (send color1 set 
                             (avg (send color1 red) (send color2 red))
                             (avg (send color1 green) (send color2 green))
                             (avg (send color1 blue) (send color2 blue)))
                       (send gbdc set-pixel x y color1)
                       (loop (- j 1)))))
                 (loop (- i 1))))
             (send gbdc set-bitmap #f)
             (send ebdc set-bitmap #f)
             b)]
          [else #f]))))
  
  
  (define (eli-paint dc)
    (send dc draw-bitmap bitmap 0 0))
  (define (eli-event evt)
    (cond
      [(send evt leaving?)
       (set-splash-paint-callback orig-paint)
       (when gc-b
         (unregister-collecting-blit splash-canvas))
       (send splash-canvas refresh)
       (when draw-thread
         (kill-thread draw-thread)
         (set! draw-thread #f))]
      [(send evt entering?)
       (set-splash-paint-callback eli-paint)
       (when gc-b
         (register-collecting-blit splash-canvas 
                                   (floor (- (/ main-size 2)
                                             (/ (send gc-b get-width) 2)))
                                   (floor (- (/ main-size 2)
                                             (/ (send gc-b get-height) 2)))
                                   (send gc-b get-width)
                                   (send gc-b get-height)
                                   gc-b gc-b))
       (send splash-canvas refresh)
       (unless draw-thread
         (start-thread))]))
  
  (define splash-eventspace (get-splash-eventspace))
  (define draw-next-state
    (let ([o 0])
      (lambda ()
        (let ([s (make-semaphore 0)])
          (parameterize ([current-eventspace splash-eventspace])
            (queue-callback 
             (λ () 
               (draw-single-step o)
               (semaphore-post s))))
          (semaphore-wait s))
        (let ([next (+ o (/ pi 60))])
          (set! o (if (< next (* 2 pi))
                      next
                      (- next (* 2 pi))))))))
  
  (define draw-thread #f)
  (define (start-thread)        
    (set! draw-thread
          (thread
           (λ ()
             (let loop ()
               (draw-next-state)
               (sleep .01)
               (loop))))))
  (define orig-paint (get-splash-paint-callback))
  
  (draw-next-state)
  (set-splash-event-callback eli-event)
  (send splash-canvas refresh))
