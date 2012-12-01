#lang racket/unit
  (require mzlib/class
           mred
           string-constants
           racket/match
           "../preferences.rkt"
           "sig.rkt")
  
  (import [prefix preferences: framework:preferences^]
          [prefix editor: framework:editor^]
          [prefix panel: framework:panel^]
          [prefix canvas: framework:canvas^]
          [prefix racket: framework:racket^]
          [prefix color: framework:color^])
  (export framework:color-prefs^)
  (init-depend framework:editor^)
  
  (define standard-style-list-text% (editor:standard-style-list-mixin text%))
  
  ;; build-color-selection-panel : (is-a?/c area-container<%>) symbol string string -> void
  ;; constructs a panel containg controls to configure the preferences panel.
  (define (build-color-selection-panel parent pref-sym style-name example-text #:background? [background? #f])
    (define (update-style-delta func)
      (let ([working-delta (new style-delta%)])
        (send working-delta copy (preferences:get pref-sym))
        (func working-delta)
        (preferences:set pref-sym working-delta)))
    (define hp (new horizontal-panel%
                    [parent parent]
                    [style '(border)]
                    [alignment '(center top)]
                    [stretchable-height #f]))
    
    (define e (new (class standard-style-list-text%
                     (inherit change-style get-style-list)
                     (define/augment (after-insert pos offset)
                       (inner (void) after-insert pos offset)
                       (let ([style (send (get-style-list)
                                          find-named-style
                                          style-name)])
                         (change-style style pos (+ pos offset) #f)))
                     (super-new))))
    (define c (new canvas:color%
                   [parent hp]
                   [min-width 150]
                   [editor e]
                   [stretchable-height #t]
                   [style '(hide-hscroll hide-vscroll)]))
    
    (define (make-check name on off)
      (let* ([c (λ (check command)
                  (if (send check get-value)
                      (update-style-delta on)
                      (update-style-delta off)))]
             [check (new check-box% 
                         [label name]
                         [parent hp]
                         [callback c])])
        check))
    
    (define slant-check
      (make-check (string-constant cs-italic)
                  (λ (delta)
                    (send delta set-style-on 'italic)
                    (send delta set-style-off 'base))
                  (λ (delta)
                    (send delta set-style-on 'normal)
                    (send delta set-style-off 'base))))
    (define bold-check
      (make-check (string-constant cs-bold)
                  (λ (delta)
                    (send delta set-weight-on 'bold)
                    (send delta set-weight-off 'base))
                  (λ (delta)
                    (send delta set-weight-on 'normal)
                    (send delta set-weight-off 'base))))
    (define underline-check
      (make-check (string-constant cs-underline)
                  (λ (delta)
                    (send delta set-underlined-on #t)
                    (send delta set-underlined-off #f))
                  (λ (delta)
                    (send delta set-underlined-off #f)
                    (send delta set-underlined-on #f))))
    
    (define smoothing-options
      '(default
         partly-smoothed
         smoothed
         unsmoothed))
    (define smoothing-option-strings
      '("Default"
        "Partly smoothed"
        "Smoothed"
        "Unsmoothed"))
    
    (define (smoothing->index s)
      (let loop ([i 0]
                 [l smoothing-options])
        (cond
          [(null? l) 
           ;; if it is something strange or it is 'base, we go with 'default (which is 0)
           0]
          [else
           (if (eq? (car l) s)
               i
               (loop (+ i 1)
                     (cdr l)))])))
    
    (define smoothing-menu
      (new choice%
           [label #f]
           [parent hp]
           [choices smoothing-option-strings]
           [callback 
            (λ (c e)
              (update-style-delta
               (λ (delta)
                 (send delta set-smoothing-on 
                       (list-ref smoothing-options
                                 (send c get-selection))))))]))
    
    (define fore/back-panel
      (and background? 
           (new vertical-pane%
                [parent hp]
                [stretchable-width #f]
                [stretchable-height #f])))
    
    (define foreground-color-button
      (and (>= (get-display-depth) 8)
           (new button%
                [label (if background? 
                           (string-constant cs-foreground-color)
                           (string-constant cs-change-color))]
                [parent (if background? 
                            fore/back-panel
                            hp)]
                [callback
                 (λ (color-button evt)
                   (let* ([add (send (preferences:get pref-sym) get-foreground-add)]
                          [color (make-object color%
                                   (send add get-r)
                                   (send add get-g)
                                   (send add get-b))]
                          [users-choice
                           (get-color-from-user
                            (format (string-constant syntax-coloring-choose-color) example-text)
                            (send color-button get-top-level-window)
                            color)])
                     (when users-choice
                       (update-style-delta
                        (λ (delta)
                          (send delta set-delta-foreground users-choice))))))])))
    (define background-color-button
      (and (>= (get-display-depth) 8)
           background?
           (new button%
                [label (string-constant cs-background-color)]
                [parent (if background? 
                            fore/back-panel
                            hp)]
                [callback
                 (λ (color-button evt)
                   (let* ([add (send (preferences:get pref-sym) get-background-add)]
                          [color (make-object color%
                                   (send add get-r)
                                   (send add get-g)
                                   (send add get-b))]
                          [users-choice
                           (get-color-from-user
                            (format (string-constant syntax-coloring-choose-color) example-text)
                            (send color-button get-top-level-window)
                            color)])
                     (when users-choice
                       (update-style-delta
                        (λ (delta)
                          (send delta set-delta-background users-choice))))))])))
    
    (define style (send (send e get-style-list) find-named-style style-name))
    
    (send c set-line-count 1)
    (send c allow-tab-exit #t)
    
    (send e insert example-text)
    (send e set-position 0)
    
    (send slant-check set-value (or (eq? (send style get-style) 'slant)
                                    (eq? (send style get-style) 'italic)))
    (send bold-check set-value (eq? (send style get-weight) 'bold))
    (send underline-check set-value (send style get-underlined))
    (send smoothing-menu set-selection (smoothing->index (send style get-smoothing)))
    
    (send hp reflow-container)
    (when (> (send c get-height) 50)
      (send c set-line-count #f)
      (send c min-height 50)
      (send c stretchable-height #f))
    
    (preferences:add-callback
     pref-sym
     (λ (p sd)
       (send slant-check set-value (or (eq? (send style get-style) 'slant)
                                       (eq? (send style get-style) 'italic)))
       (send bold-check set-value (eq? (send sd get-weight-on) 'bold))
       (send underline-check set-value (send sd get-underlined-on))
       (send smoothing-menu set-selection (smoothing->index (send sd get-smoothing-on)))))
    (void))
  
  (define (add/mult-set m v)
    (send m set (car v) (cadr v) (caddr v)))
  
  (define (add/mult-get m)
    (let ([b1 (box 0)]
          [b2 (box 0)]
          [b3 (box 0)])
      (send m get b1 b2 b3)
      (map unbox (list b1 b2 b3))))
  
  (define style-delta-get/set
    (let ([lo3n (λ (x) (and (list? x) (= (length x) 3) (andmap number? x)))])
      (list (list (λ (x) (send x get-alignment-off))
                  (λ (x v) (send x set-alignment-off v))
                  (λ (x) (memq x '(base top center bottom))))
            
            (list (λ (x) (send x get-alignment-on))
                  (λ (x v) (send x set-alignment-on v))
                  (λ (x) (memq x '(base top center bottom))))
            
            (list (λ (x) (add/mult-get (send x get-background-add)))
                  (λ (x v) (add/mult-set (send x get-background-add) v))
                  lo3n)
            
            (list (λ (x) (add/mult-get (send x get-background-mult)))
                  (λ (x v) (add/mult-set (send x get-background-mult) v))
                  lo3n)
            
            (list (λ (x) (send x get-face))
                  (λ (x v) (send x set-face v))
                  (λ (x) (or (string? x) (not x))))
            
            (list (λ (x) (send x get-family))
                  (λ (x v) (send x set-family v))
                  (λ (x) (memq x '(base default decorative roman script swiss modern symbol system))))
            
            (list (λ (x) (add/mult-get (send x get-foreground-add)))
                  (λ (x v) (add/mult-set (send x get-foreground-add) v))
                  lo3n)
            
            (list (λ (x) (add/mult-get (send x get-foreground-mult)))
                  (λ (x v) (add/mult-set (send x get-foreground-mult) v))
                  lo3n)
            
            (list (λ (x) (send x get-size-add))
                  (λ (x v) (send x set-size-add v))
                  (λ (x) (and (integer? x) (exact? x) (<= 0 x 255))))
            
            (list (λ (x) (send x get-size-mult))
                  (λ (x v) (send x set-size-mult v))
                  (λ (x) (and (number? x) (real? x))))
            
            (list (λ (x) (send x get-style-off))
                  (λ (x v) (send x set-style-off v))
                  (λ (x) (memq x '(base normal italic slant))))
            
            (list (λ (x) (send x get-style-on))
                  (λ (x v) (send x set-style-on v))
                  (λ (x) (memq x '(base normal italic slant))))
            
            (list (λ (x) (send x get-underlined-off))
                  (λ (x v) (send x set-underlined-off v))
                  boolean?)
            
            (list (λ (x) (send x get-underlined-on))
                  (λ (x v) (send x set-underlined-on v))
                  boolean?)
            
            (list (λ (x) (send x get-weight-off))
                  (λ (x v) (send x set-weight-off v))
                  (λ (x) (memq x '(base normal bold light))))
            
            (list (λ (x) (send x get-weight-on))
                  (λ (x v) (send x set-weight-on v))
                  (λ (x) (memq x '(base normal bold light)))))))
  
  (define (marshall-style-delta style)
    (map (λ (fs) ((car fs) style)) style-delta-get/set))
  
  (define (unmarshall-style-delta info)
    (let ([style (make-object style-delta%)])
      
      (when (list? info)
        (let loop ([style-delta-get/set style-delta-get/set]
                   [info info])
          (cond
            [(null? info) (void)]
            [(null? style-delta-get/set) (void)]
            [else (let ([v (car info)]
                        [fs (car style-delta-get/set)])
                    (when ((list-ref fs 2) v)
                      ((list-ref fs 1) style v))
                    (loop (cdr style-delta-get/set)
                          (cdr info)))])))
      
      style))
  
  (define (make-style-delta color bold? underline? italic?)
    (let ((sd (make-object style-delta%)))
      (send sd set-delta-foreground color)
      (cond
        (bold?
         (send sd set-weight-on 'bold)
         (send sd set-weight-off 'base))
        (else
         (send sd set-weight-on 'base)
         (send sd set-weight-off 'bold)))
      (send sd set-underlined-on underline?)
      (send sd set-underlined-off (not underline?))
      (cond
        (italic?
         (send sd set-style-on 'italic)
         (send sd set-style-off 'base))
        (else
         (send sd set-style-on 'base)
         (send sd set-style-off 'italic)))
      sd))
  
  (define (add-background-preferences-panel)
    (preferences:add-panel
     (list (string-constant preferences-colors)
           (string-constant background-color))
     (λ (parent)
       (let ([vp (new vertical-panel% (parent parent) (alignment '(left top)))])
         (add-solid-color-config (string-constant background-color)
                                 vp
                                 'framework:basic-canvas-background)
         (add-solid-color-config (string-constant basic-gray-paren-match-color)
                                 vp
                                 'framework:paren-match-color)
         (build-text-foreground-selection-panel vp
                                                'framework:default-text-color
                                                (editor:get-default-color-style-name)
                                                (string-constant default-text-color))
         
         (build-text-foreground-selection-panel vp
                                                'framework:misspelled-text-color
                                                color:misspelled-text-color-style-name
                                                (string-constant misspelled-text-color))
         
         (let* ([choice (new choice% 
                             [label (string-constant parenthesis-color-scheme)]
                             [parent vp]
                             [choices (map (λ (x) (list-ref x 1)) 
                                           (color:get-parenthesis-colors-table))]
                             [callback
                              (λ (choice _)
                                (preferences:set 'framework:paren-color-scheme
                                                 (car (list-ref (color:get-parenthesis-colors-table)
                                                                (send choice get-selection)))))])]
                [update-choice
                 (lambda (v)
                   (send choice set-string-selection 
                         (cadr (or (assoc v (color:get-parenthesis-colors-table))
                                   (car (color:get-parenthesis-colors-table))))))])
           (preferences:add-callback 
            'framework:paren-color-scheme
            (λ (p v)
              (update-choice v)))
           (update-choice (preferences:get 'framework:paren-color-scheme)))
         
         (let ([hp (new horizontal-panel% 
                        [parent vp]
                        [alignment '(center top)])])
           (new button%
                [label (string-constant white-on-black-color-scheme)]
                [parent hp]
                [callback (λ (x y) 
                            (preferences:set 'framework:white-on-black? #t)
                            (white-on-black))])
           (new button%
                [label (string-constant black-on-white-color-scheme)]
                [parent hp]
                [callback (λ (x y) 
                            (preferences:set 'framework:white-on-black? #f)
                            (black-on-white))]))))))
  
  
  (define (build-text-foreground-selection-panel parent pref-sym style-name example-text)
    (define hp (new horizontal-panel% 
                    (parent parent)
                    (style '(border))
                    (stretchable-height #f)))
    (define e (new (class standard-style-list-text%
                     (inherit change-style get-style-list)
                     (define/augment (after-insert pos offset)
                       (inner (void) after-insert pos offset)
                       (let ([style (send (get-style-list)
                                          find-named-style
                                          style-name)])
                         (change-style style pos (+ pos offset) #f)))
                     (super-new))))
    (define c (new canvas:color%
                   (parent hp)
                   (editor e)
                   (style '(hide-hscroll
                            hide-vscroll))))
    (define color-button
      (and (>= (get-display-depth) 8)
           (make-object button%
             (string-constant cs-change-color)
             hp
             (λ (color-button evt)
               (let ([users-choice
                      (get-color-from-user
                       (format (string-constant syntax-coloring-choose-color) example-text)
                       (send color-button get-top-level-window)
                       (preferences:get pref-sym))])
                 (when users-choice
                   (preferences:set pref-sym users-choice)))))))
    (define style (send (send e get-style-list) find-named-style style-name))
    
    (send c set-line-count 1)
    (send c allow-tab-exit #t)
    
    (send e insert example-text)
    (send e set-position 0))
  
  (define (add-solid-color-config label parent pref-id)
    (letrec ([panel (new vertical-panel% (parent parent) (stretchable-height #f))]
             [hp (new horizontal-panel% (parent panel) (stretchable-height #f))]
             [msg (new message% (parent hp) (label label))]
             [canvas
              (new canvas%
                   (parent hp)
                   (paint-callback
                    (λ (c dc)
                      (draw (preferences:get pref-id)))))]
             [draw
              (λ (clr)
                (let ([dc (send canvas get-dc)])
                  (let-values ([(w h) (send canvas get-client-size)])
                    (send dc set-brush (send the-brush-list find-or-create-brush clr 'solid))
                    (send dc set-pen (send the-pen-list find-or-create-pen clr 1 'solid))
                    (send dc draw-rectangle 0 0 w h))))]
             [button
              (new button% 
                   (label (string-constant cs-change-color))
                   (parent hp)
                   (callback
                    (λ (x y)
                      (let ([color (get-color-from-user
                                    (string-constant choose-a-background-color)
                                    (send hp get-top-level-window)
                                    (preferences:get pref-id))])
                        (when color
                          (preferences:set pref-id color))))))])
      (preferences:add-callback
       pref-id
       (λ (p v) (draw v)))
      panel))
  
  ;; add-to-preferences-panel : string (vertical-panel -> void) -> void
  (define (add-to-preferences-panel panel-name func)
    (preferences:add-panel
     (list (string-constant preferences-colors) panel-name)
     (λ (parent)
       (let ([panel (new vertical-panel% (parent parent))])
         (func panel)
         panel))))
  
  ;; see docs
  (define (register-color-preference pref-name style-name color/sd 
                                     [white-on-black-color #f]
                                     [use-old-marshalling? #t]
                                     #:background [background #f])
    (let ([sd (cond 
                [(is-a? color/sd style-delta%)
                 color/sd]
                [else
                 (let ([sd (new style-delta%)])
                   (send sd set-delta-foreground color/sd)
                   sd)])])
      
      (when background
        (send sd set-delta-background background))
      
      (preferences:set-default pref-name sd (λ (x) (is-a? x style-delta%)))
      (when white-on-black-color
        (set! color-scheme-colors
              (cons (list pref-name
                          color/sd
                          (to-color white-on-black-color))
                    color-scheme-colors)))
      (preferences:set-un/marshall pref-name marshall-style-delta unmarshall-style-delta)
      (preferences:add-callback pref-name
                                (λ (sym v)
                                  (editor:set-standard-style-list-delta style-name v)))
      (editor:set-standard-style-list-delta style-name (preferences:get pref-name))))
  
  (define color-scheme-colors '())
  
  (define (set-default/color-scheme pref-sym black-on-white white-on-black)
    (let ([bw-c (to-color black-on-white)]
          [wb-c (to-color white-on-black)])
      (set! color-scheme-colors
            (cons (list pref-sym 
                        (to-color black-on-white)
                        (to-color white-on-black))
                  color-scheme-colors))
      
      (preferences:set-default pref-sym bw-c (λ (x) (is-a? x color%)))
      (preferences:set-un/marshall 
       pref-sym
       (λ (clr) (list (send clr red) (send clr green) (send clr blue) (send clr alpha)))
       (λ (lst) 
         (match lst
           [(list (? byte? red) (? byte? green) (? byte? blue))
            ;; old prefs-- before there were no alpha components to color% objects
            ;; and so only r/g/b was saved.
            (make-object color% red green blue)]
           [(list (? byte? red) (? byte? green) (? byte? blue) (? (λ (x) (and (real? x) (<= 0 x 1))) α))
            (make-object color% red green blue α)]
           [else #f])))
      (void)))
  
  (define (to-color c)
    (cond
      [(is-a? c color%) c]
      [(is-a? c style-delta%)
       (let ([m (send c get-foreground-mult)])
         (unless (and (= 0 (send m get-r))
                      (= 0 (send m get-g))
                      (= 0 (send m get-b)))
           (error 'register-color-scheme "expected a style delta with foreground-mult that is all zeros"))
         (let ([add (send c get-foreground-add)])
           (make-object color% 
             (send add get-r)
             (send add get-g)
             (send add get-b))))]
      [(string? c)
       (or (send the-color-database find-color c)
           (error 'register-color-scheme 
                  "did not find color ~s in the-color-database"
                  c))]))
  
  (define (black-on-white) (do-colorization cadr))
  (define (white-on-black) (do-colorization caddr))
  (define (do-colorization sel)
    (for-each (λ (l) 
                (let* ([p (car l)]
                       [color (sel l)]
                       [old (preferences:get p)])
                  (cond
                    [(is-a? old color%)
                     (preferences:set p color)]
                    [(is-a? old style-delta%)
                     (send old set-delta-foreground color)
                     (preferences:set p old)])))
              color-scheme-colors))
