#lang racket/base
(require racket/unit
         racket/class
         racket/gui/base
         drracket/private/drsig
         framework
         string-constants)

(define sc-smoothing-label (string-constant font-smoothing-label))
(define sc-smoothing-none (string-constant font-smoothing-none))
(define sc-smoothing-some (string-constant font-smoothing-some))
(define sc-smoothing-all (string-constant font-smoothing-all))
(define sc-smoothing-default (string-constant font-smoothing-default))

(provide font@)

(define-unit font@
  (import [prefix drracket:language-configuration: drracket:language-configuration/internal^])
  (export drracket:font^)
  
  (define (setup-preferences)
    (preferences:add-panel
     (list (string-constant font-prefs-panel-title))
     (λ (panel)
       (define main (make-object vertical-panel% panel))
       (define min-size 1)
       (define max-size 72)
       (define options-panel (make-object horizontal-panel% main))
       (define size-panel (new group-box-panel%
                               (parent options-panel)
                               (label (string-constant font-size))))
       (define (adjust-font-size f)
         (editor:set-current-preferred-font-size
          (f (editor:get-current-preferred-font-size))))
       (define size-slider
         (new slider%
              (label #f)
              (min-value min-size)
              (max-value max-size)
              (parent size-panel)
              (callback
               (λ (size evt)
                 (adjust-font-size
                  (λ (old-size)
                    (send size get-value)))))
              (init-value
               (editor:get-current-preferred-font-size))))
       (define size-hp (new horizontal-pane% (parent size-panel)))
       (define (mk-size-button label chng)
         (new button%
              (parent size-hp)
              (stretchable-width #t)
              (callback
               (λ (x y)
                 (adjust-font-size
                  (λ (old-size)
                    (min max-size (max min-size (chng old-size)))))))
              (label label)))
       (define size-sub1 (mk-size-button "-1" sub1))
       (define size-add1 (mk-size-button "+1" add1))
       
       (define mono-list 'mono-list-not-yet-computed)
       (define choice-panel 
         (new (class vertical-panel%
                (define/private (force-cache receiver)
                  (when (eq? receiver font-name-control)
                    (when (symbol? mono-list)
                      (begin-busy-cursor)
                      (set! mono-list (sort (get-face-list 'mono) string-ci<=?))
                      (send font-name-control clear)
                      (for-each
                       (λ (x) (send font-name-control append x))
                       (append mono-list (list (string-constant other...))))
                      (define pref (preferences:get 'framework:standard-style-list:font-name))
                      (cond 
                        [(member pref mono-list)
                         (send font-name-control set-string-selection pref)]
                        [else
                         (send font-name-control set-selection (length mono-list))])
                      (end-busy-cursor))))
                (define/override (on-subwindow-event receiver evt)
                  (unless (or (send evt moving?)
                              (send evt entering?)
                              (send evt leaving?))
                    (force-cache receiver))
                  (super on-subwindow-event receiver evt))
                (define/override (on-subwindow-char receiver evt)
                  (force-cache receiver)
                  (super on-subwindow-char receiver evt))
                (super-new [parent options-panel]))))
       (define font-name-control
         (new choice%
              [label (string-constant font-name)]
              [choices (list (preferences:get 'framework:standard-style-list:font-name))]
              [parent choice-panel]
              [stretchable-width #t]
              [callback
               (λ (font-name evt)
                 (define selection (send font-name get-selection))
                 (cond
                   [(< selection (length mono-list))
                    (preferences:set 
                     'framework:standard-style-list:font-name
                     (list-ref mono-list selection))]
                   [else
                    (define all-faces (get-face-list))
                    (define init (preferences:get 'framework:standard-style-list:font-name))
                    (define init-choices
                      (let loop ([faces all-faces]
                                 [num 0])
                        (cond
                          [(null? faces) null]
                          [else 
                           (define face (car faces))
                           (if (equal? init face)
                               (list num)
                               (loop (cdr faces) (+ num 1)))])))
                    (define choice
                      (get-choices-from-user
                       (string-constant select-font-name)
                       (string-constant select-font-name)
                       all-faces
                       #f
                       init-choices))
                    (when choice
                      (preferences:set 
                       'framework:standard-style-list:font-name 
                       (list-ref all-faces (car choice))))]))]))
       (define (set-choice-selection font-name)
         (cond
           [(send font-name-control find-string font-name)
            (send font-name-control set-string-selection font-name)]
           [else
            (send font-name-control set-selection (- (send font-name-control get-number) 1))]))
       (preferences:add-callback
        'framework:standard-style-list:font-name
        (λ (p v)
          (set-choice-selection v)))
       (set-choice-selection (preferences:get 'framework:standard-style-list:font-name))
       (define smoothing-contol
         (new choice%
              (label sc-smoothing-label)
              (choices (list sc-smoothing-none
                             sc-smoothing-some
                             sc-smoothing-all
                             sc-smoothing-default))
              (parent choice-panel)
              (stretchable-width #t)
              (selection (case (preferences:get 'framework:standard-style-list:smoothing)
                           [(unsmoothed) 0]
                           [(partly-smoothed) 1]
                           [(smoothed) 2]
                           [(default) 3]))
              (callback (λ (x y) 
                          (preferences:set 
                           'framework:standard-style-list:smoothing
                           (case (send x get-selection)
                             [(0) 'unsmoothed]
                             [(1) 'partly-smoothed]
                             [(2) 'smoothed]
                             [(3) 'default]))))))
       
       (define text (new (text:foreground-color-mixin
                          (editor:standard-style-list-mixin 
                           text:line-spacing%))))
       (define ex-panel (new horizontal-panel% [parent main]))
       (define msg (new message% 
                        [label (string-constant example-text)]
                        [parent ex-panel]))
       (define canvas (make-object canvas:color% main text))
       (define (update-text setting)
         (send text begin-edit-sequence)
         (send text lock #f)
         (send text erase)
         (send text insert 
               (format
                ";; howmany : list-of-numbers -> number~
               \n;; to determine how many numbers are in `a-lon'~
               \n(define (howmany a-lon)~
               \n  (cond~
               \n    [(empty? a-lon) 0]~
               \n    [else (+ 1 (howmany (rest a-lon)))]))~
               \n~
               \n;; examples as tests~
               \n(howmany empty)~
               \n\"should be\"~
               \n0~
               \n~
               \n(howmany (cons 1 (cons 2 (cons 3 empty))))~
               \n\"should be\"~
               \n3"))
         (send text set-position 0 0)
         (send text lock #t)
         (send text end-edit-sequence))
       (preferences:add-callback
        'framework:standard-style-list:font-size
        (λ (p v) (send size-slider set-value 
                       (editor:font-size-pref->current-font-size
                        v))))
       (preferences:add-callback
        drracket:language-configuration:settings-preferences-symbol
        (λ (p v)
          (update-text v)))
       (update-text (preferences:get drracket:language-configuration:settings-preferences-symbol))
       (send ex-panel set-alignment 'left 'center)
       (send ex-panel stretchable-height #f)
       (send canvas allow-tab-exit #t)
       (send options-panel stretchable-height #f)
       (send options-panel set-alignment 'center 'top)
       (send text lock #t)
       main))))
