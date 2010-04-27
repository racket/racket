#lang racket/base

(require mred
         racket/class
         racket/cmdline
         racket/list
         framework/private/bday
         framework/splash
         racket/file
         "eb.rkt")

(define files-to-open (command-line #:args filenames filenames))

;; updates the command-line-arguments with only the files
;; to open. See also main.rkt.
(current-command-line-arguments (apply vector files-to-open))

(define-values (texas-independence-day? prince-kuhio-day? kamehameha-day? halloween? weekend?)
  (let* ([date (seconds->date (current-seconds))]
         [month (date-month date)]
         [day (date-day date)]
         [dow (date-week-day date)])
    (values (and (= 3 month) (= 2 day))
            (and (= 3 month) (= 26 day))
            (and (= 6 month) (= 11 day))
            (and (= 10 month) (= 31 day))
            (or (= dow 6) (= dow 0)))))

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
     (when (and (eq? ch #\q)
                (send evt get-control-down))
       (exit))
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

(when (eb-bday?) (install-eb))

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
   [(and high-color? weekend?)
    (build-path (collection-path "icons") "plt-logo-red-shiny.png")]
   [high-color?
    (build-path (collection-path "icons") "plt-logo-red-diffuse.png")]
   [(= (get-display-depth) 1)
    (build-path (collection-path "icons") "pltbw.gif")]
   [else
    (build-path (collection-path "icons") "plt-flat.gif")])
 "DrRacket"
 99)

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
                        ((dynamic-require 'drscheme/private/key 'break-threads))))
                     (parent f))])
        (send f show #t)))))

(dynamic-require 'drscheme/tool-lib #f)
(shutdown-splash)
(close-splash)
