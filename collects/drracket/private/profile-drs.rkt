#lang racket/base
(require racket/gui/base
         racket/class
         profile/sampler
         profile/render-text
         profile/analyzer
         framework/preferences)

#|

This file is loaded early in drscheme's startup when the PLTDRPROFILE
environment variable is setup. It sets up a profiler to examine drscheme
itself.

|#

(provide start-profile)

(define (start-profile super-custodian)
  (define drs-custodian (current-custodian))
  (define drs-main-thread (current-thread))
  (define profile-eventspace
    (parameterize ([current-custodian super-custodian])
      (make-eventspace)))
  (define frame (parameterize ([current-eventspace profile-eventspace])
                  (new frame% [label "Profiling"] [width 500] [height 550])))
  (define t (let ([t (new text%)])
              (send t lock #t)
              t))
  (define ec (new editor-canvas% [parent frame] [editor t]))
  (define bp (new horizontal-panel% [parent frame] [stretchable-height #f]))
  (define pause-b (new button% [label "Pause"] [parent bp] [callback (λ (a b) (pause))] [stretchable-width #t]))
  (define resume-b (new button% [label "Resume"] [parent bp] [callback (λ (a b) (resume))] [stretchable-width #t]))
  (define start-stop-b (new button% [label "Stop"] [parent bp] [callback (λ (a b) (start-stop))] [stretchable-width #t]))
  (define choice (new choice% [label #f] [choices '("12" "9")] [parent bp] [callback (λ (a b) (font-size))]))

  
  (define mb (new menu-bar% [parent frame]))
  (define edit-menu (new menu% [label "&Edit"] [parent mb]))
  (define sa (new menu-item% 
                  [parent edit-menu]
                  [label "Select al&l"]
                  [shortcut #\a] 
                  [callback (λ (x y) 
                              (send ec focus)
                              (send t set-position 0 (send t last-position)))]))
  (define copy (new menu-item%
                    [parent edit-menu]
                    [label "&Copy"]
                    [shortcut #\c]
                    [callback (λ (x y) (send t copy))]))
  
  (define (font-size)
    (let ([n (send choice get-string-selection)])
      (when n
        (send sd set-delta 'change-size (string->number n))
        (update-profile-report))))
  
  (define (update-buttons)
    (send resume-b enable (and current-sampler (not running?)))
    (send pause-b enable (and current-sampler running?))
    (send start-stop-b set-label (if current-sampler
                                "Stop"
                                "Start")))
  
  (define running? #f)
  (define current-sampler #f)
  (define (start-sampler)
    (let ([s (create-sampler (list drs-custodian drs-main-thread) 
                             #e0.05
                             super-custodian)])
      (set! current-sampler s)))
  (define (stop-sampler)
    (current-sampler 'stop)
    (set! current-sampler #f)
    (set! running? #f))

  (define sd (let ([sd (make-object style-delta%)])
               (send sd set-delta 'change-size 12)
               (send sd set-family 'modern)
               sd))
  
  (define updating-thread 
    (parameterize ([current-custodian super-custodian]
                   [current-eventspace profile-eventspace])
      (thread (λ ()
                (let loop ()
                  (queue-callback update-profile-report)
                  (sleep 5) 
                  (loop))))))
  
  (define (update-profile-report)
    (when current-sampler
      (send t begin-edit-sequence)
      (send t lock #f)
      (send t erase)
      (parameterize ([current-output-port (open-output-text-editor t)])
        (render (analyze-samples (current-sampler 'get-snapshots))))
      (send t change-style sd 0 (send t last-position))
      (send t set-position 0)
      (send t lock #t)
      (send t end-edit-sequence)))
  
  (define (resume) 
    (set! running? #t)
    (current-sampler 'resume)
    (update-buttons))
  (define (pause) 
    (set! running? #f)
    (current-sampler 'pause)
    (update-buttons)
    (update-profile-report))
  (define (start-stop) 
    (cond
      [current-sampler (stop-sampler)]
      [else 
       (start-sampler)
       (set! running? #t)])
    (update-profile-report)
    (update-buttons))

  (update-buttons)
  (send ec focus)
  (send frame show #t))
