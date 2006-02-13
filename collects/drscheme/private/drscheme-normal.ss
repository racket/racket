
(module drscheme-normal mzscheme
  (require (lib "mred.ss" "mred")
	   (lib "class.ss")
           (lib "cmdline.ss")
           (lib "bday.ss" "framework" "private"))
  
  ;; this used to be done by mred, but
  ;; since drscheme uses the -Z flag now,
  ;; we have to do it explicitly.
  (current-load text-editor-load-handler)

  (define files-to-open
    (command-line
     (case (system-type)
       [(windows) "DrScheme.exe"]
       [(macosx) "drscheme" #;"DrScheme"]
       [else "drscheme"])
     (current-command-line-arguments)
     (args filenames filenames)))
  
  ;; updates the command-line-arguments with only the files
  ;; to open. See also main.ss.
  (current-command-line-arguments (apply vector files-to-open))
  
  (define-values (texas-independence-day? halloween?)
    (let* ([date (seconds->date (current-seconds))]
           [month (date-month date)]
           [day (date-day date)])
      (values (and (= 3 month) (= 2 day))
              (and (= 10 month) (= 31 day)))))
  
  (define high-color? ((get-display-depth) . > . 8))
  (define special-state #f)
  (define normal-bitmap #f)
  (define (get-normal-bitmap)
    (unless normal-bitmap
      (set! normal-bitmap  (make-object bitmap% (build-path (collection-path "icons") "PLT-206.png"))))
    normal-bitmap)
  
  (define-struct magic-image (string filename bitmap)) 
  
  (define (magic-img str img)
    (make-magic-image (list->string (reverse (string->list str))) img #f))
  
  ;; magic strings and their associated images. Any string in this list that's a prefix of any other
  ;; is not going to be reachable; you could change that by removing the (set! key-codes null) line
  ;; where the match occurs
  (define magic-images
    (list 
     (magic-img "larval" "PLT-206-larval.png")
     (magic-img "mars" "PLT-206-mars.jpg")))
  
  (define (load-magic-images)
    (get-normal-bitmap)
    (for-each 
     (λ (magic-image)
       (unless (magic-image-bitmap magic-image)
         (set-magic-image-bitmap! 
          magic-image 
          (make-object bitmap% (build-path (collection-path "icons") (magic-image-filename magic-image))))))
     magic-images))
  
  (define longest-magic-string (apply max (map (λ (s) (string-length (magic-image-string s))) magic-images)))
  
  (define key-codes null)
  (define key-codes-len 0)
  
  (define (add-key-code new-code)
    (unless (eq? 'release new-code)
      (set! key-codes
            (let loop ([n longest-magic-string]
                       [l (cons new-code key-codes)])
              (cond
                [(zero? n) null]
                [(null? l) null]
                [else (cons (car l) (loop (- n 1) (cdr l)))])))))

  (let ([set-splash-bitmap (dynamic-require '(lib "splash.ss" "framework") 'set-splash-bitmap)])
    ((dynamic-require '(lib "splash.ss" "framework") 'set-splash-char-observer)
     (λ (evt)
       (add-key-code (send evt get-key-code))
       
       ;; as soon as something is typed, load the bitmaps
       (load-magic-images)
       
       (when (andmap char? key-codes)
         (let ((s (apply string key-codes)))
           (cond 
             [(ormap (λ (m)
                       (if (string=? s (magic-image-string m))
                           m
                           #f)) magic-images)
              => 
              (λ (match)
                (set! key-codes null)
                (set-splash-bitmap
                 (if (and special-state (string=? special-state (magic-image-string match)))
                     (begin
                       (set! special-state #f)
                       (get-normal-bitmap))
                     (begin
                       (set! special-state (magic-image-string match))
                       (magic-image-bitmap match)))))]
             [else (void)]))))))
  
  (when (eb-bday?)
    (let ()
      (define main-size 260)
      (define pi (atan 0 -1))
      
      (define eli (make-object bitmap% (build-path (collection-path "icons") "eli-purple.jpg")))
      (define bitmap (make-object bitmap% main-size main-size))
      (define bdc (make-object bitmap-dc% bitmap))
      
      (define outer-color (send the-color-database find-color "darkorange"))
      (define inner-color (send the-color-database find-color "green"))
      (define omega-str "λ (x) (x x)) (λ (x) (x x))) ((")
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
      
      (define splash-canvas ((dynamic-require '(lib "splash.ss" "framework") 'get-splash-canvas)))
      (define (draw-single-step dc offset)
        (send bdc draw-bitmap eli 0 0)
        (draw-single-loop omega-str bdc offset (/ main-size 2) (/ main-size 2) 120 32 outer-color)
        (draw-single-loop hebrew-str bdc (+ (- (* 2 pi) offset) (* 2 pi)) (/ main-size 2) (/ main-size 2) 70 20 inner-color)
        (send splash-canvas on-paint))
      
      (define gc-b 
        (with-handlers ([exn:fail? (lambda (x)
                                     (printf "~s\n" (exn-message x))
                                     #f)])
          (let ([b (make-object bitmap% (build-path (collection-path "icons") "recycle.gif"))])
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
           ((dynamic-require '(lib "splash.ss" "framework") 'set-splash-paint-callback) orig-paint)
           (when gc-b
             (unregister-collecting-blit splash-canvas))
           (send splash-canvas refresh)
           (when draw-thread
             (kill-thread draw-thread)
             (set! draw-thread #f))]
          [(send evt entering?)
           ((dynamic-require '(lib "splash.ss" "framework") 'set-splash-paint-callback) eli-paint)
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
      
      (define splash-eventspace ((dynamic-require '(lib "splash.ss" "framework") 'get-splash-eventspace)))
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
      (define orig-paint ((dynamic-require '(lib "splash.ss" "framework") 'get-splash-paint-callback)))
      
      (draw-next-state)
      ((dynamic-require '(lib "splash.ss" "framework") 'set-splash-event-callback) eli-event)
      (send splash-canvas refresh)))
  
  ((dynamic-require '(lib "splash.ss" "framework") 'start-splash)
   (build-path (collection-path "icons") 
               (cond
                 [texas-independence-day?
                  "texas-plt-bw.gif"]
                 [(and halloween? high-color?)
                  "PLT-pumpkin.png"]
                 [high-color? "PLT-206.png"]
                 [(= (get-display-depth) 1)
                  "pltbw.gif"]
                 [else
                  "plt-flat.gif"]))
   "DrScheme"
   99)
  
  (when (getenv "PLTDRBREAK")
    (printf "PLTDRBREAK: creating break frame\n")
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
                          ((dynamic-require '(lib "key.ss" "drscheme" "private") 'break-threads))))
                       (parent f))])
          (send f show #t)))))
  
  (dynamic-require '(lib "start.ss" "drscheme" "private") #f))
