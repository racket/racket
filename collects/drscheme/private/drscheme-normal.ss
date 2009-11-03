#lang scheme/base

(require mred
         scheme/class
         scheme/cmdline
         scheme/list
         framework/private/bday
         framework/splash
         scheme/file
         "eb.ss")

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

(define eb-today? (eb-bday?))
(when eb-today? (install-eb))

;; assumes that the width & height of all of the bitmaps
;; in this list are the same.
(define plt-logo-evolution
  (map icons-bitmap
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
(define stages 5)

;; number of increments (per cycle) to dedicate to 
;; an unfaded version of the logos. must be > 0.
(define pause-time 2)

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

(define evolution-last-stage 
  (* (+ stages pause-time) 
     (- (length plt-logo-evolution) 1)))

(define (logo-index val range)
  (min (max (floor (* (+ evolution-last-stage 1) (/ val range))) 
            0)
       evolution-last-stage))

(define (update-bitmap-stage val range)
  (update-bitmap-stage/index (logo-index val range)))

(define (update-bitmap-stage/index index)
  (let* ([q (quotient index (+ stages pause-time))]
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
               0 0)
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

(define start-time (current-milliseconds))
(define last-times-delta 
  (let ([pref (get-preference 'plt:drscheme-splash-timing)])
    (and pref
         (pair? pref)
         (equal? (car pref) (version))
         (cdr pref))))

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
    (let ([gc-bm (make-object bitmap% (build-path (collection-path "icons") "recycle.png") 'png/mask)]
          [w (send (car plt-logo-evolution) get-width)]
          [h (send (car plt-logo-evolution) get-height)])
      (when (send gc-bm ok?)
        (let* ([gc-w (send gc-bm get-width)]
               [gc-h (send gc-bm get-height)]
               [off-bm (make-object bitmap% gc-w gc-h)]
               [bdc (make-object bitmap-dc% off-bm)])
          (send bdc clear)
          (send bdc set-bitmap #f)
          (unless eb-today?
            (register-collecting-blit (get-splash-canvas) (- w gc-w 2) 2 gc-w gc-h gc-bm off-bm))))
      (cond
        [last-times-delta
         (thread
          (λ () 
            (let loop ([i 0])
              (when (<= i evolution-last-stage)
                (let* ([now (current-milliseconds)]
                       [next-stage-start (+ start-time (* last-times-delta (/ i (+ evolution-last-stage 1))))]
                       [delta (- next-stage-start now)])
                  (sleep (max 0 (/ delta 1000))))
                (parameterize ([current-eventspace (get-splash-eventspace)])
                  (queue-callback
                   (λ () 
                     (update-bitmap-stage/index i)
                     (refresh-splash))))
                (loop (+ i 1))))))
         (vector splash-evolution w h)]
        [else
         (build-path (collection-path "icons") "plt-logo-red-shiny.png")]))]
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

(define end-time (current-milliseconds))
(shutdown-splash)
(close-splash)
(put-preferences '(plt:drscheme-splash-timing)
                 (list (cons (version) (* .8 (- end-time start-time))))
                 void) ;; swallow errors 
