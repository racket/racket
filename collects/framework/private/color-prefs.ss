(module color-prefs (lib "a-unit.ss")
  (require (lib "class.ss")
           (lib "unit.ss")
           (lib "etc.ss")
           (lib "mred.ss" "mred")
           (lib "string-constant.ss" "string-constants")
           "sig.ss")
  
  (import [prefix preferences: framework:preferences^]
          [prefix editor: framework:editor^]
          [prefix panel: framework:panel^]
          [prefix canvas: framework:canvas^])
  (export framework:color-prefs^)
  (init-depend framework:editor^)
  
  (define standard-style-list-text% (editor:standard-style-list-mixin text%))
  
  ;; build-color-selection-panel : (is-a?/c area-container<%>) symbol string string -> void
  ;; constructs a panel containg controls to configure the preferences panel.
  ;; BUG: style changes don't update the check boxes.
  (define build-color-selection-panel
    (opt-lambda (parent 
                 pref-sym
                 style-name
                 example-text
                 [update-style-delta
                  (λ (func)
                    (let ([delta (preferences:get pref-sym)])
                      (func delta)
                      (preferences:set pref-sym delta)))])
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
      
      (define (make-check name on off)
        (let* ([c (λ (check command)
                    (if (send check get-value)
                        (update-style-delta on)
                        (update-style-delta off)))]
               [check (make-object check-box% name hp c)])
          check))
      
      (define slant-check
        (make-check (string-constant cs-italic)
                    (λ (delta)
                      (send delta set-style-on 'slant)
                      (send delta set-style-off 'base))
                    (λ (delta)
                      (send delta set-style-on 'base)
                      (send delta set-style-off 'slant))))
      (define bold-check
        (make-check (string-constant cs-bold)
                    (λ (delta)
                      (send delta set-weight-on 'bold)
                      (send delta set-weight-off 'base))
                    (λ (delta)
                      (send delta set-weight-on 'base)
                      (send delta set-weight-off 'bold))))
      (define underline-check
        (make-check (string-constant cs-underline)
                    (λ (delta)
                      (send delta set-underlined-on #t)
                      (send delta set-underlined-off #f))
                    (λ (delta)
                      (send delta set-underlined-off #t)
                      (send delta set-underlined-on #f))))
      (define color-button
        (and (>= (get-display-depth) 8)
             (make-object button%
               (string-constant cs-change-color)
               hp
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
                        (send delta set-delta-foreground users-choice)))))))))
      (define style (send (send e get-style-list) find-named-style style-name))
      
      (send c set-line-count 1)
      (send c allow-tab-exit #t)
      
      (send e insert example-text)
      (send e set-position 0)
      
      (send slant-check set-value (eq? (send style get-style) 'slant))
      (send bold-check set-value (eq? (send style get-weight) 'bold))
      (send underline-check set-value (send style get-underlined))))
  
  (define (add/mult-set m v)
    (send m set (car v) (cadr v) (caddr v)))
  
  (define (add/mult-get m)
    (let ([b1 (box 0)]
          [b2 (box 0)]
          [b3 (box 0)])
      (send m get b1 b2 b3)
      (map unbox (list b1 b2 b3))))
  
  (define style-delta-get/set
    (list (cons (λ (x) (send x get-alignment-off))
                (λ (x v) (send x set-alignment-off v)))
          (cons (λ (x) (send x get-alignment-on))
                (λ (x v) (send x set-alignment-on v)))
          (cons (λ (x) (add/mult-get (send x get-background-add)))
                (λ (x v) (add/mult-set (send x get-background-add) v)))
          (cons (λ (x) (add/mult-get (send x get-background-mult)))
                (λ (x v) (add/mult-set (send x get-background-mult) v)))
          (cons (λ (x) (send x get-face))
                (λ (x v) (send x set-face v)))
          (cons (λ (x) (send x get-family))
                (λ (x v) (send x set-family v)))
          (cons (λ (x) (add/mult-get (send x get-foreground-add)))
                (λ (x v) (add/mult-set (send x get-foreground-add) v)))
          (cons (λ (x) (add/mult-get (send x get-foreground-mult)))
                (λ (x v) (add/mult-set (send x get-foreground-mult) v)))
          (cons (λ (x) (send x get-size-add))
                (λ (x v) (send x set-size-add v)))
          (cons (λ (x) (send x get-size-mult))
                (λ (x v) (send x set-size-mult v)))
          (cons (λ (x) (send x get-style-off))
                (λ (x v) (send x set-style-off v)))
          (cons (λ (x) (send x get-style-on))
                (λ (x v) (send x set-style-on v)))
          (cons (λ (x) (send x get-underlined-off))
                (λ (x v) (send x set-underlined-off v)))
          (cons (λ (x) (send x get-underlined-on))
                (λ (x v) (send x set-underlined-on v)))
          (cons (λ (x) (send x get-weight-off))
                (λ (x v) (send x set-weight-off v)))
          (cons (λ (x) (send x get-weight-on))
                (λ (x v) (send x set-weight-on v)))))
  
  (define (marshall-style style)
    (map (λ (fs) ((car fs) style)) style-delta-get/set))
  
  (define (unmarshall-style info)
    (let ([style (make-object style-delta%)])
      (for-each (λ (fs v) ((cdr fs) style v)) style-delta-get/set info)
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
       (let ([vp (new vertical-panel% (parent parent))])
         (add-solid-color-config (string-constant background-color)
                                 vp
                                 'framework:basic-canvas-background)
         (add-solid-color-config (string-constant paren-match-color)
                                 vp
                                 'framework:paren-match-color)
         (build-text-foreground-selection-panel vp
                                                'framework:default-text-color
                                                (editor:get-default-color-style-name)
                                                (string-constant default-text-color))))))
  
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
  (define (register-color-pref pref-name style-name color)
    (let ([sd (new style-delta%)])
      (send sd set-delta-foreground color)
      (preferences:set-default pref-name sd (λ (x) (is-a? x style-delta%))))
    (preferences:set-un/marshall pref-name marshall-style unmarshall-style)
    (preferences:add-callback pref-name
                              (λ (sym v)
                                (editor:set-standard-style-list-delta style-name v)))
    (editor:set-standard-style-list-delta style-name (preferences:get pref-name))))
