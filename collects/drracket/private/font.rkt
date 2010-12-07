#lang racket/base
  (require racket/unit
           racket/class
           racket/gui/base
           "drsig.rkt"
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
       (list (string-constant font-prefs-panel-title)
             #;(string-constant drscheme))  ;; thre is no help desk font configuration anymore ...
       (λ (panel)
         (letrec ([main (make-object vertical-panel% panel)]
                [min-size 1]
                [max-size 72]
                [options-panel (make-object horizontal-panel% main)]
                [size-panel (new group-box-panel%
                                 (parent options-panel)
                                 (label (string-constant font-size)))]
                [adjust-font-size
                 (λ (f)
                   (preferences:set
                    'framework:standard-style-list:font-size
                    (f (preferences:get
                        'framework:standard-style-list:font-size))))]
                [size-slider
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
                       (preferences:get 'framework:standard-style-list:font-size)))]
                [size-hp (new horizontal-pane% (parent size-panel))]
                [mk-size-button
                 (λ (label chng)
                   (new button%
                        (parent size-hp)
                        (stretchable-width #t)
                        (callback
                         (λ (x y)
                           (adjust-font-size
                            (λ (old-size)
                              (min max-size (max min-size (chng old-size)))))))
                        (label label)))]
                [size-sub1 (mk-size-button "-1" sub1)]
                [size-add1 (mk-size-button "+1" add1)]
                
                [mono-list 'mono-list-not-yet-computed]
                [choice-panel 
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
                              (let ([pref (preferences:get 'framework:standard-style-list:font-name)])
                                (cond 
                                  [(member pref mono-list)
                                   (send font-name-control set-string-selection pref)]
                                  [else
                                   (send font-name-control set-selection (length mono-list))]))
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
                        (super-new [parent options-panel])))]
                [font-name-control
                 (let* ([choice
                         (new choice%
                              (label (string-constant font-name))
                              (choices (list (preferences:get 'framework:standard-style-list:font-name)))
                              (parent choice-panel)
                              (stretchable-width #t)
                              (callback
                               (λ (font-name evt)
                                 (let ([selection (send font-name get-selection)])
                                   (cond
                                     [(< selection (length mono-list))
                                      (preferences:set 
                                       'framework:standard-style-list:font-name
                                       (list-ref mono-list selection))]
                                     [else
                                      (let* ([all-faces (get-face-list)]
                                             [init-choices
                                              (let ([init (preferences:get 'framework:standard-style-list:font-name)])
                                                (let loop ([faces all-faces]
                                                           [num 0])
                                                  (cond
                                                    [(null? faces) null]
                                                    [else
                                                     (let ([face (car faces)])
                                                       (if (equal? init face)
                                                           (list num)
                                                           (loop (cdr faces)
                                                                 (+ num 1))))])))]
                                             [choice (get-choices-from-user
                                                      (string-constant select-font-name)
                                                      (string-constant select-font-name)
                                                      all-faces
                                                      #f
                                                      init-choices)])
                                        (when choice
                                          (preferences:set 
                                           'framework:standard-style-list:font-name 
                                           (list-ref all-faces (car choice)))))])))))]
                        [font-name (preferences:get 'framework:standard-style-list:font-name)]
                        [set-choice-selection
                         (λ (font-name)
                           (cond
                             [(send choice find-string font-name)
                              (send choice set-string-selection font-name)]
                             [else
                              (send choice set-selection (- (send choice get-number) 1))]))])
                   
                   (preferences:add-callback
                    'framework:standard-style-list:font-name
                    (λ (p v)
                      (set-choice-selection v)))
                   (set-choice-selection font-name)
                   choice)]
                [smoothing-contol
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
                                     [(3) 'default])))))]
                
                [text (make-object (text:foreground-color-mixin
                                    (editor:standard-style-list-mixin 
                                     text:basic%)))]
                [ex-panel (make-object horizontal-panel% main)]
                [msg (make-object message% (string-constant example-text) ex-panel)]
                [canvas (make-object canvas:color% main text)]
                [update-text
                 (λ (setting)
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
                   (send text end-edit-sequence))])
           (preferences:add-callback
            'framework:standard-style-list:font-size
            (λ (p v) (send size-slider set-value v)))
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
           main)))))
