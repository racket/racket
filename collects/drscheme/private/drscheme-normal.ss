#lang scheme/base

(require mred
         scheme/class
         scheme/cmdline
         scheme/list
         framework/private/bday
         framework/splash)

(define files-to-open (command-line #:args filenames filenames))

;; updates the command-line-arguments with only the files
;; to open. See also main.ss.
(current-command-line-arguments (apply vector files-to-open))

(define-values (texas-independence-day? prince-kuhio-day? kamehameha-day? halloween?)
  (let* ([date (seconds->date (current-seconds))]
         [month (date-month date)]
         [day (date-day date)])
    (values (and (= 3 month) (= 2 day))
            (and (= 3 month) (= 26 day))
            (and (= 6 month) (= 11 day))
            (and (= 10 month) (= 31 day)))))

(define high-color? ((get-display-depth) . > . 8))
(define special-state #f)
(define normal-bitmap #f) ; set by load-magic-images

(define icons-bitmap
  (let ([icons (collection-path "icons")])
    (lambda (name)
      (make-object bitmap% (build-path icons name)))))

(define-struct magic-image (chars filename [bitmap #:mutable]))

(define (magic-img str img)
  (make-magic-image (reverse (string->list str)) img #f))

;; magic strings and their associated images.  There should not be a string
;; in this list that is a prefix of another.
(define magic-images
  (list #;(magic-img "larval" "PLT-206-larval.png")
        (magic-img "mars"   "PLT-206-mars.jpg")))

(define (load-magic-images)
  (set! load-magic-images void) ; run only once
  (unless normal-bitmap (set! normal-bitmap (icons-bitmap "PLT-206.png")))
  (for-each (λ (magic-image)
              (unless (magic-image-bitmap magic-image)
                (set-magic-image-bitmap!
                 magic-image
                 (icons-bitmap (magic-image-filename magic-image)))))
            magic-images))

(define longest-magic-string
  (apply max (map (λ (s) (length (magic-image-chars s))) magic-images)))

(define key-codes null)

(define (find-magic-image)
  (define (prefix? l1 l2)
    (or (null? l1)
        (and (pair? l2)
             (eq? (car l1) (car l2))
             (prefix? (cdr l1) (cdr l2)))))
  (ormap (λ (i) (and (prefix? (magic-image-chars i) key-codes) i))
         magic-images))

(define (add-key-code new-code)
  (set! key-codes (cons new-code key-codes))
  (when ((length key-codes) . > . longest-magic-string)
    (set! key-codes (take key-codes longest-magic-string))))

(set-splash-char-observer
 (λ (evt)
   (let ([ch (send evt get-key-code)])
     (when (char? ch)
       ;; as soon as something is typed, load the bitmaps
       (load-magic-images)
       (add-key-code ch)
       (let ([match (find-magic-image)])
         (when match
           (set! key-codes null)
           (set-splash-bitmap
            (if (eq? special-state match)
                (begin (set! special-state #f) normal-bitmap)
                (begin (set! special-state match)
                       (magic-image-bitmap match))))))))))

(when (eb-bday?)
  (let ()
    (define main-size 260)
    (define pi (atan 0 -1))
    
    (define eli (icons-bitmap "eli-purple.jpg"))
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
      (let loop ([i (string-length str)])
        (unless (zero? i)
          (draw-letter dc 
                       cx 
                       cy 
                       (normalize-angle
                        (+ (- (* 2 pi) (* (* 2 pi) (/ (- i 1) (string-length str))))
                           (/ pi 2)
                           offset))
                       radius
                       (string (string-ref str (- i 1)))
                       color)
          (loop (- i 1)))))
    
    (define (normalize-angle angle)
      (cond
        [(<= 0 angle (* 2 pi)) angle]
        [(< angle 0) (normalize-angle (+ angle (* 2 pi)))]
        [else (normalize-angle (- angle (* 2 pi)))]))
    
    (define splash-canvas (get-splash-canvas))
    (define (draw-single-step dc offset)
      (send bdc draw-bitmap eli 0 0)
      (draw-single-loop omega-str bdc offset (/ main-size 2) (/ main-size 2) 120 32 outer-color)
      (draw-single-loop hebrew-str bdc (+ (- (* 2 pi) offset) (* 2 pi)) (/ main-size 2) (/ main-size 2) 70 20 inner-color)
      (send splash-canvas on-paint))
    
    (define gc-b
      (with-handlers ([exn:fail? (lambda (x)
                                   (printf "~s\n" (exn-message x))
                                   #f)])
        (let ([b (icons-bitmap "recycle.gif")])
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
                 (draw-single-step bdc o)
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
    (send splash-canvas refresh)))

;; assumes that the width & height of all of the bitmaps
;; in this list are the same.
(define plt-logo-evolution
  (map (λ (x) (make-object bitmap% (build-path (collection-path "icons") x)))
       '("plt-logo-red-flat.png"
         "plt-logo-red-gradient.png"
         "plt-logo-red-diffuse.png"
         "plt-logo-red-shiny.png")))

;; the currently being drawing bitmap (for the splash-evolution startup screen)
(define splash-evolution-bitmap (car plt-logo-evolution))

;; a scratch bitmap that is used for the interpolations between the bitmaps in plt-logo-evolution
(define interpolate-evolution-bitmap #f)
(define interpolate-evolution-bdc (make-object bitmap-dc%))

;; number of greyscale stages (between the logos above)
(define stages 4)

;; number of increments (per cycle) to dedicate to 
;; an unfaded version of the logos. must be > 0.
(define pause-time 4)

(define stage-bitmaps
  (cond
    [(send (car plt-logo-evolution) ok?)
     (let ([bdc (make-object bitmap-dc%)]
           [w (send (car plt-logo-evolution) get-width)]
           [h (send (car plt-logo-evolution) get-height)])
       (set! interpolate-evolution-bitmap (make-object bitmap% w h))
       (let loop ([i 0])
         (cond
           [(= stages i) '()]
           [else
            (let ([bm (make-object bitmap% w h)]
                  [grey (floor (* 255 (/ (+ i 1) (+ stages 1))))])
              (send bdc set-bitmap bm)
              (send bdc set-pen "black" 1 'transparent)
              (send bdc set-brush (make-object color% grey grey grey) 'solid)
              (send bdc draw-rectangle 0 0 w h)
              (send bdc set-bitmap #f)
              (cons bm (loop (+ i 1))))])))]
    [else '()]))

(define (logo-index val range)
  (let ([low-end 0]
        [high-end (* (+ stages pause-time) 
                     (- (length plt-logo-evolution) 1))])
    (min (max (floor (* (+ high-end 1) (/ val range))) 
              low-end)
         high-end)))

(define (update-bitmap-stage val range)
  (let* ([index (logo-index val range)]
         [q (quotient index (+ stages pause-time))]
         [m (modulo index (+ stages pause-time))])
    (cond
      [(< m pause-time)
       (set! splash-evolution-bitmap (list-ref plt-logo-evolution q))
       (when (= q (+ stages pause-time -1))
         (set! stage-bitmaps 'cleared-out-stage-bitmaps)
         (set! splash-evolution-bitmap 'cleared-out-splash-evolution-bitmap)
         (set! plt-logo-evolution 'cleared-out-plt-logo-evolution))]
      [else
       (let* ([before-inc (- m pause-time)]
              [after-inc (- (- (length stage-bitmaps) 1) before-inc)])
         (send interpolate-evolution-bdc set-bitmap interpolate-evolution-bitmap)
         (send interpolate-evolution-bdc clear)
         (send interpolate-evolution-bdc draw-bitmap 
               (list-ref plt-logo-evolution q)
               0 0
               'solid
               (send the-color-database find-color "black")
               (list-ref stage-bitmaps before-inc))
         (send interpolate-evolution-bdc draw-bitmap 
               (list-ref plt-logo-evolution (+ q 1))
               0 0
               'solid
               (send the-color-database find-color "black")
               (list-ref stage-bitmaps after-inc))
         (send interpolate-evolution-bdc set-bitmap #f)
         (set! splash-evolution-bitmap interpolate-evolution-bitmap))])))

(define (splash-evolution dc val range w h)
  (send dc draw-bitmap 
        splash-evolution-bitmap
        0
        0))

(start-splash
 (cond
   [(or prince-kuhio-day? kamehameha-day?)
    (set-splash-progress-bar? #f)
    (let ([size ((dynamic-require 'drscheme/private/palaka 'palaka-pattern-size) 4)])
      (vector (dynamic-require 'drscheme/private/honu-logo 'draw-honu) 
              size 
              size))]
   [texas-independence-day?
    (build-path (collection-path "icons") "texas-plt-bw.gif")]
   [(and halloween? high-color?)
    (build-path (collection-path "icons") "PLT-pumpkin.png")]
   [(and high-color?  
         (send (car plt-logo-evolution) ok?))
    (set-refresh-splash-on-gauge-change?! 
     (λ (val range) 
       (cond
         [(equal? (logo-index val range) (logo-index (- val 1) range))
          #f]
         [else
          (update-bitmap-stage val range)
          #t])))
    (vector splash-evolution
            (send (car plt-logo-evolution) get-width)
            (send (car plt-logo-evolution) get-height))]
   [high-color?
    (build-path (collection-path "icons") "PLT-206.png")]
   [(= (get-display-depth) 1)
    (build-path (collection-path "icons") "pltbw.gif")]
   [else
    (build-path (collection-path "icons") "plt-flat.gif")])
 "DrScheme"
 99)

(when (getenv "PLTDRBREAK")
  (printf "PLTDRBREAK: creating break frame\n") (flush-output)
  (let ([to-break (eventspace-handler-thread (current-eventspace))])
    (parameterize ([current-eventspace (make-eventspace)])
      (let* ([f (new frame% (label "Break DrScheme"))]
             [b (new button% 
                     (label "Break Main Thread")
                     (callback
                      (λ (x y)
                        (break-thread to-break)))
                     (parent f))]
             [b (new button% 
                     (label "Break All Threads")
                     (callback
                      (λ (x y)
                        ((dynamic-require 'drscheme/private/key 'break-threads))))
                     (parent f))])
        (send f show #t)))))

(dynamic-require 'drscheme/tool-lib #f)
