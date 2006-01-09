(module font mzscheme
  (require (lib "unitsig.ss")
           (lib "class.ss")
           "drsig.ss"
           (lib "mred.ss" "mred")
           (lib "framework.ss" "framework")
           (lib "string-constant.ss" "string-constants"))
  
  (define sc-smoothing-label (string-constant font-smoothing-label))
  (define sc-smoothing-none (string-constant font-smoothing-none))
  (define sc-smoothing-some (string-constant font-smoothing-some))
  (define sc-smoothing-all (string-constant font-smoothing-all))
  (define sc-smoothing-default (string-constant font-smoothing-default))
  
  (provide font@)
  
  (define font@
    (unit/sig drscheme:font^
      (import [drscheme:language-configuration : drscheme:language-configuration/internal^])
      
      (define (setup-preferences)
        (preferences:add-panel
         (list (string-constant font-prefs-panel-title)
               (string-constant drscheme))
         (λ (panel)
           (let* ([main (make-object vertical-panel% panel)]
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
                  [size-sub1 (mk-size-button "+1" add1)]
                  
                  [choice-panel (new vertical-panel% (parent options-panel))]
                  [font-name-control
                   (case (system-type)
                     [(windows macos macosx)
                      (let* ([choice
                              (new choice%
                                   (label (string-constant font-name))
                                   (choices (get-face-list 'mono))
                                   (parent choice-panel)
                                   (stretchable-width #t)
                                   (callback
                                    (λ (font-name evt)
                                      (preferences:set 
                                       'framework:standard-style-list:font-name
                                       (send font-name get-string-selection)))))]
                             [font-name (preferences:get 'framework:standard-style-list:font-name)])
                        (preferences:add-callback
                         'framework:standard-style-list:font-name
                         (λ (p v)
                           (when (send choice find-string v)
                             (send choice set-string-selection v))))
                        (when (send choice find-string font-name)
                          (send choice set-string-selection font-name))
                        choice)]
                     [(unix)
                      (make-object button%
                        (string-constant set-font)
                        choice-panel
                        (λ xxx
                          (let* ([faces (get-face-list 'mono)]
                                 [init-choices
                                  (let ([init (preferences:get 'framework:standard-style-list:font-name)])
                                    (let loop ([faces faces]
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
                                          (get-face-list 'mono)
                                          #f
                                          init-choices)])
                            (when choice
                              (preferences:set 
                               'framework:standard-style-list:font-name 
                               (list-ref (get-face-list 'mono) (car choice)))))))]
                     [else (error 'font-name-control "unknown system type: ~s~n" (system-type))])]
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
              drscheme:language-configuration:settings-preferences-symbol
              (λ (p v)
                (update-text v)))
             (update-text (preferences:get drscheme:language-configuration:settings-preferences-symbol))
             (send ex-panel set-alignment 'left 'center)
             (send ex-panel stretchable-height #f)
             (send canvas allow-tab-exit #t)
             (send options-panel stretchable-height #f)
             (send options-panel set-alignment 'center 'top)
             (send text lock #t)
             main)))))))