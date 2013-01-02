#lang racket/base

(require mred
         racket/class
         racket/cmdline
         racket/list
         framework/private/bday
         framework/splash
         racket/runtime-path
         racket/file
         "frame-icon.rkt"
         "eb.rkt")

(define-runtime-path doc-icon.rkt "dock-icon.rkt")

(define files-to-open (command-line #:args filenames filenames))

(define startup-date 
  (seconds->date
   (let ([ssec (getenv "PLTDREASTERSECONDS")])
     (if ssec
         (string->number ssec)
         (current-seconds)))))

;; updates the command-line-arguments with only the files
;; to open. See also main.rkt.
(current-command-line-arguments (apply vector files-to-open))

(define (weekend-date? date)
  (define dow (date-week-day date))
  (or (= dow 6) (= dow 0)))

(define (valentines-date? date)
  (and (= 2 (date-month date))
       (= 14 (date-day date))))

(define (icon-state date)
  (cond
    [(valentines-date? date) 'valentines]
    [(weekend-date? date) 'weekend]
    [else 'normal]))

(define-values (texas-independence-day? prince-kuhio-day? kamehameha-day? halloween?)
  (let* ([month (date-month startup-date)]
         [day (date-day startup-date)]
         [dow (date-week-day startup-date)])
    (values (and (= 3 month) (= 2 day))
            (and (= 3 month) (= 26 day))
            (and (= 6 month) (= 11 day))
            (and (= 10 month) (= 31 day)))))

(define special-state #f)

(define (icons-bitmap name)
  (make-object bitmap% (collection-file-path name "icons")))

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

(define (drracket-splash-char-observer evt)
  (let ([ch (send evt get-key-code)])
    (when (and (eq? ch #\q)
               (send evt get-control-down))
      (exit))
    (when (and the-splash-bitmap (char? ch))
      ;; as soon as something is typed, load the bitmaps
      (load-magic-images)
      (add-key-code ch)
      (let ([match (find-magic-image)])
        (when match
          (set! key-codes null)
          (set-splash-bitmap
           (if (eq? special-state match)
               (begin (set! special-state #f) the-splash-bitmap)
               (begin (set! special-state match)
                      (magic-image-bitmap match))))
          (refresh-splash))))))

(when (eb-bday?) (install-eb))

(define weekend-bitmap-spec (collection-file-path "plt-logo-red-shiny.png" "icons"))
(define normal-bitmap-spec (collection-file-path "plt-logo-red-diffuse.png" "icons"))
(define valentines-days-spec (collection-file-path "heart.png" "icons"))

(define the-bitmap-spec
  (cond
    [(valentines-date? startup-date)
     valentines-days-spec]
    [(or prince-kuhio-day? kamehameha-day?)
     (set-splash-progress-bar?! #f)
     (let ([size ((dynamic-require 'drracket/private/palaka 'palaka-pattern-size) 4)])
       (vector (dynamic-require 'drracket/private/honu-logo 'draw-honu) 
               size 
               size))]
    [texas-independence-day?
     (collection-file-path "texas-plt-bw.gif" "icons")]
    [halloween?
     (collection-file-path "PLT-pumpkin.png" "icons")]
    [(weekend-date? startup-date)
     weekend-bitmap-spec]
    [else normal-bitmap-spec]))

(define the-splash-bitmap (and (path? the-bitmap-spec) (read-bitmap the-bitmap-spec)))
(set-splash-char-observer drracket-splash-char-observer)

(when (eq? (system-type) 'macosx)
  (define initial-state (icon-state startup-date))
  (define weekend-bitmap (if (equal? the-bitmap-spec weekend-bitmap-spec)
                             the-splash-bitmap
                             #f))
  (define weekday-bitmap (if (equal? the-bitmap-spec normal-bitmap-spec)
                             the-splash-bitmap
                             #f))
  (define valentines-bitmap (if (equal? the-bitmap-spec valentines-days-spec)
                                the-splash-bitmap
                                #f))
  (define set-doc-tile-bitmap (dynamic-require doc-icon.rkt 'set-dock-tile-bitmap))
  (define (set-icon state)
    (case state
      [(valentines) 
       (unless valentines-bitmap (set! valentines-bitmap (read-bitmap valentines-days-spec)))
       (set-doc-tile-bitmap valentines-bitmap)]
      [(weekend)
       (unless weekend-bitmap (set! weekend-bitmap (read-bitmap weekend-bitmap-spec)))
       (set-doc-tile-bitmap weekend-bitmap)]
      [(normal) 
       (unless weekday-bitmap (set! weekday-bitmap (read-bitmap normal-bitmap-spec)))
       (set-doc-tile-bitmap weekday-bitmap)]))
  (set-icon initial-state)
  (void
   (thread
    (λ ()
      (let loop ([last-state initial-state])
        (sleep 10)
        (define next-state (icon-state (seconds->date (current-seconds))))
        (unless (equal? last-state next-state)
          (set-icon next-state))
        (loop next-state))))))

(start-splash (or the-splash-bitmap
                  the-bitmap-spec)
              (format "DrRacket ~a" (version))
              700
              #:allow-funny? #t
              #:frame-icon todays-icon)

(when (getenv "PLTDRBREAK")
  (printf "PLTDRBREAK: creating break frame\n") (flush-output)
  (let ([to-break (eventspace-handler-thread (current-eventspace))])
    (parameterize ([current-eventspace (make-eventspace)])
      (let* ([f (new frame% (label "Break DrRacket"))]
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
                        ((dynamic-require 'drracket/private/key 'break-threads))))
                     (parent f))])
        (send f show #t)))))

(dynamic-require 'drracket/tool-lib #f)
(shutdown-splash)
(close-splash)
