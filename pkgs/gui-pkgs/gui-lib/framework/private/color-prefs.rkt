#lang racket/unit
  (require racket/class
           racket/gui/base
           string-constants
           racket/match
           racket/contract/base
           racket/set
           setup/getinfo
           setup/collects
           string-constants
           racket/pretty
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
  (define (build-color-selection-panel parent pref-sym style-name example-text 
                                       #:background? [background? #f])
    (define (get-from-pref-sym)
      (if (set-member? known-style-names pref-sym)
          (lookup-in-color-scheme pref-sym)
          (preferences:get pref-sym)))
    (define (set-via-pref-sym delta) 
      (if (set-member? known-style-names pref-sym)
          (set-in-color-scheme pref-sym delta)
          (preferences:set pref-sym delta)))
    (define (add-pref-sym-callback f) 
      (if (set-member? known-style-names pref-sym)
          (register-color-scheme-entry-change-callback pref-sym f)
          (preferences:add-callback pref-sym (λ (p sd) (f sd)))))
    (define (update-style-delta func)
      (let ([working-delta (new style-delta%)])
        (send working-delta copy (get-from-pref-sym))
        (func working-delta)
        (set-via-pref-sym working-delta)))
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
                   (let* ([add (send (get-from-pref-sym) get-foreground-add)]
                          [color (make-object color%
                                   (send add get-r)
                                   (send add get-g)
                                   (send add get-b))]
                          [users-choice
                           (get-color-from-user
                            (format (string-constant syntax-coloring-choose-color) example-text)
                            (send color-button get-top-level-window)
                            color
                            '(alpha))])
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
                   (let* ([add (send (get-from-pref-sym) get-background-add)]
                          [color (make-object color%
                                   (send add get-r)
                                   (send add get-g)
                                   (send add get-b))]
                          [users-choice
                           (get-color-from-user
                            (format (string-constant syntax-coloring-choose-color) example-text)
                            (send color-button get-top-level-window)
                            color
                            '(alpha))])
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
    
    (add-pref-sym-callback
     (λ (sd)
       (send slant-check set-value (or (equal? (send style get-style) 'slant)
                                       (equal? (send style get-style) 'italic)))
       (send bold-check set-value (equal? (send sd get-weight-on) 'bold))
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
  
  (define (make-style-delta color bold? underline? italic? #:background [background #f])
    (define sd (make-object style-delta%))
    (send sd set-delta-foreground color)
    (cond
      [bold?
       (send sd set-weight-on 'bold)
       (send sd set-weight-off 'base)]
      [else
       (send sd set-weight-on 'base)
       (send sd set-weight-off 'bold)])
    (send sd set-underlined-on underline?)
    (send sd set-underlined-off (not underline?))
    (cond
      [italic?
       (send sd set-style-on 'italic)
       (send sd set-style-off 'base)]
      [else
       (send sd set-style-on 'base)
       (send sd set-style-off 'italic)])
    (when background
      (send sd set-delta-background background))
    sd)
  
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
           (update-choice (preferences:get 'framework:paren-color-scheme)))))))
    
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
                       (lookup-in-color-scheme pref-sym)
                       '(alpha))])
                 (when users-choice
                   (set-in-color-scheme pref-sym users-choice)))))))
    (define style (send (send e get-style-list) find-named-style style-name))
    
    (send c set-line-count 1)
    (send c allow-tab-exit #t)
    
    (send e insert example-text)
    (send e set-position 0))
  
  (define (add-solid-color-config label parent pref-id)
    (define panel (new vertical-panel% (parent parent) (stretchable-height #f)))
    (define hp (new horizontal-panel% (parent panel) (stretchable-height #f)))
    (define msg (new message% (parent hp) (label label)))
    (define canvas
      (new canvas%
           (parent hp)
           (paint-callback
            (λ (c dc)
              (draw (lookup-in-color-scheme pref-id))))))
    (define (draw clr)
      (define dc (send canvas get-dc))
      (define-values (w h) (send canvas get-client-size))
      (send dc set-pen (send the-pen-list find-or-create-pen "black" 1 'transparent))
      (send dc set-brush (if (preferences:get 'framework:white-on-black?)
                             "black"
                             "white")
            'solid)
      (send dc draw-rectangle 0 0 w h)
      (send dc set-brush (send the-brush-list find-or-create-brush clr 'solid))
      (send dc draw-rectangle 0 0 w h))
    (define button
      (new button% 
           (label (string-constant cs-change-color))
           (parent hp)
           (callback
            (λ (x y)
              (define color (get-color-from-user
                             (string-constant choose-a-background-color)
                             (send hp get-top-level-window)
                             (lookup-in-color-scheme pref-id)
                             '(alpha)))
              (when color
                (set-in-color-scheme pref-id color))))))
    (register-color-scheme-entry-change-callback
     pref-id
     (λ (v) 
       ;; the pref should be updated on the next event callback
       (queue-callback (λ () (send canvas refresh)))))
    panel)

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
       unmarshall-color)
      (void)))
  
(define (unmarshall-color lst)
  (match lst
    [(list (? byte? red) (? byte? green) (? byte? blue))
     ;; old prefs-- before there were no alpha components to color% objects
     ;; and so only r/g/b was saved.
     (make-object color% red green blue)]
    [(list (? byte? red) (? byte? green) (? byte? blue) (? (between/c 0 1) α))
     (make-object color% red green blue α)]
    [else #f]))

  (define (to-color c)
    (cond
      [(is-a? c color%) c]
      [(is-a? c style-delta%)
       (let ([m (send c get-foreground-mult)])
         (unless (and (= 0 (send m get-r))
                      (= 0 (send m get-g))
                      (= 0 (send m get-b)))
           (error 'register-color-scheme
                  "expected a style delta with foreground-mult that is all zeros"))
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


;                                                                                                 
;                                                                                                 
;                                                                                                 
;                                                                                                 
;                  ;;;                                 ;;;                                        
;                  ;;;                                 ;;;                                        
;    ;;;     ;;;   ;;;   ;;;   ;;; ;;    ;;;;    ;;;   ;;; ;;    ;;;;  ;;; ;; ;;;    ;;;;   ;;;;  
;   ;;;;;   ;;;;;  ;;;  ;;;;;  ;;;;;    ;;; ;;  ;;;;;  ;;;;;;;  ;; ;;; ;;;;;;;;;;;  ;; ;;; ;;; ;; 
;  ;;;  ;; ;;; ;;; ;;; ;;; ;;; ;;;      ;;;    ;;;  ;; ;;; ;;; ;;; ;;; ;;; ;;; ;;; ;;; ;;; ;;;    
;  ;;;     ;;; ;;; ;;; ;;; ;;; ;;;       ;;;;  ;;;     ;;; ;;; ;;;;;;; ;;; ;;; ;;; ;;;;;;;  ;;;;  
;  ;;;  ;; ;;; ;;; ;;; ;;; ;;; ;;;         ;;; ;;;  ;; ;;; ;;; ;;;     ;;; ;;; ;;; ;;;        ;;; 
;   ;;;;;   ;;;;;  ;;;  ;;;;;  ;;;      ;; ;;;  ;;;;;  ;;; ;;;  ;;;;;; ;;; ;;; ;;;  ;;;;;; ;; ;;; 
;    ;;;     ;;;   ;;;   ;;;   ;;;       ;;;;    ;;;   ;;; ;;;   ;;;;  ;;; ;;; ;;;   ;;;;   ;;;;  
;                                                                                                 
;                                                                                                 
;                                                                                                 
;                                                                                                 

(define default-example
  (string-append
   "#lang racket   ; draw a graph of\n"
   "(require plot) ; cos and log\n"
   "(plot #:label \"y = cos(x) & y = log(x)\"\n"
   "      (list (function cos -5 5) (function log -5 5)))\n"
   "\"an unclosed string is an error"))

(struct color-scheme (name button-label white-on-black-base? mapping example) #:transparent)
(define black-on-white-color-scheme-name 'classic)
(define white-on-black-color-scheme-name 'white-on-black)
(define known-color-schemes
  ;; note:first item in this list must be the black-on-white color scheme
  ;; and the second must the white-on-black color scheme
  (list (color-scheme black-on-white-color-scheme-name
                      (string-constant classic-color-scheme)
                      #f (make-hash) default-example)
        (color-scheme white-on-black-color-scheme-name
                      (string-constant white-on-black-color-scheme)
                      #t (make-hash) default-example)))

(define color-change-callbacks (make-hash))

(define known-color-names (set))
(define known-style-names (set))

(define (get-color-scheme-names) (values known-color-names known-style-names))

(define-logger color-scheme)

(define (register-info-based-color-schemes)
  (log-color-scheme-info
   "color-names: ~a\nstyle-names:\n~a\n" 
   (sort (set->list known-color-names) symbol<?)
   (sort (set->list known-style-names) symbol<?))
  (define preferred-color-scheme (preferences:get 'framework:color-scheme))
  (for ([dir (in-list (find-relevant-directories '(framework:color-schemes)))])
    (define info (get-info/full dir))
    (when info
      (define cs-info (info 'framework:color-schemes))
      (cond
        [(info-file-result-check? cs-info)
         (for ([one-scheme (in-list cs-info)])
           (define name (hash-ref one-scheme 'name 
                                  (λ ()
                                    (define d (path->module-path dir))
                                    (if (path-string? d)
                                        (format "~a" d)
                                        (format "~s" d)))))
           (define white-on-black-base? (hash-ref one-scheme 'white-on-black-base? #f))
           (define mapping (hash-ref one-scheme 'colors '()))
           (define example (hash-ref one-scheme 'example default-example))
           (register-color-scheme (if (symbol? name)
                                      (if (string-constant? name)
                                          (dynamic-string-constant name)
                                          (symbol->string name))
                                      name)
                                  white-on-black-base?
                                  mapping
                                  example))]
        [else
         (when cs-info
           (log-color-scheme-warning
            "expected something matching:\n~a\nfor framework:color-schemes in ~a, got\n~a"
            (pretty-format (contract-name info-file-result-check?))
            dir
            (pretty-format cs-info)))])))
  ;; the color-scheme saved in the user's preferences may not be known
  ;; until after the code above executes, which would mean that the 
  ;; color scheme in effect up to that point may be wrong. So fix that here:
  (set-current-color-scheme preferred-color-scheme #t))


;; register-color-scheme : string boolean? (listof (cons/c symbol? (listof props)) -> void
;; props = (or/c 'bold 'italic 'underline 
;;              
;; called based on the contents of info.rkt files
(define (register-color-scheme scheme-name white-on-black-base? mapping example)
  (define (good-line? line)
    (or (set-member? known-color-names (car line))
        (set-member? known-style-names (car line))))
  (for ([x (in-list mapping)])
    (unless (good-line? x)
      (log-color-scheme-warning "unknown style/color name: ~s" x)))
  (set! known-color-schemes
        (append known-color-schemes
                (list
                 (color-scheme
                  (if (symbol? scheme-name)
                      scheme-name
                      (string->symbol scheme-name))
                  (if (symbol? scheme-name)
                      (dynamic-string-constant scheme-name)
                      scheme-name)
                  white-on-black-base?
                  (make-hash
                   (for/list ([line (in-list mapping)]
                              #:when (good-line? line))
                     (define name (car line))
                     (cons name
                           (cond
                             [(set-member? known-color-names name)
                              (props->color (cdr line))]
                             [(set-member? known-style-names name)
                              (props->style-delta (cdr line))]))))
                  example)))))

(define valid-props? 
  (listof (or/c 'bold 'italic 'underline
                (vector/c byte? byte? byte? #:flat? #t)
                (vector/c byte? byte? byte? (between/c 0.0 1.0) #:flat? #t))))

(define (valid-key-values? h)
  (for/or ([(k v) (in-hash h)])
    (cond
      [(equal? k 'name) (or (string? v) (symbol? v))]
      [(equal? k 'white-on-black-base?) (boolean? v)]
      [(equal? k 'colors) ((listof (cons/c symbol? valid-props?)) v)]
      [(equal? k 'example) (string? v)]
      [else 
       ;; don't care about other keys, as they'll be ignored
       #t])))

(define info-file-result-check?
  (listof (and/c hash?
                 immutable?
                 valid-key-values?)))

(define (props->color line)
  (or (for/or ([v (in-list line)])
        (and (vector? v)
             (vec->color v)))
      (vec->color #'(0 0 0))))

(define (props->style-delta line)
  (define sd (new style-delta%))
  (for ([prop (in-list line)])
    (match prop
      [`bold (send sd set-delta 'change-bold)]
      [`italic (send sd set-delta 'change-italic)]
      [`underline (send sd set-delta 'change-underline #t)]
      [else (send sd set-delta-foreground (vec->color prop))]))
  sd)

(define (vec->color v)
  (make-object color% 
    (vector-ref v 0)
    (vector-ref v 1)
    (vector-ref v 2)
    (if (= (vector-length v) 4)
        (vector-ref v 3)
        1.0)))

;; returns the user's preferred color, wrt to the current color scheme
(define (lookup-in-color-scheme color-name)
  (lookup-in-color-scheme/given-mapping 
   color-name
   (preferences:get (color-scheme-entry-name->pref-name color-name))
   (get-current-color-scheme)))

(define (lookup-in-color-scheme/given-mapping color-name table a-color-scheme)
  (cond
    ;; check if the user adjusted the color
    [(hash-ref table (color-scheme-name a-color-scheme) #f)
     =>
     values]
    ;; check if the color scheme has that mapping
    [(hash-ref (color-scheme-mapping a-color-scheme)
               color-name
               #f)
     => values]
    [else
     ;; fall back to either the white-on-black or the black-on-white color scheme
     (define fallback-color-scheme
       (lookup-color-scheme
        (if (color-scheme-white-on-black-base? a-color-scheme)
            white-on-black-color-scheme-name
            black-on-white-color-scheme-name)))
     (hash-ref (color-scheme-mapping fallback-color-scheme)
               color-name)]))

;; set-color : symbol (or/c string? (is-a?/c color%) (is-a?/c style-delta%)) -> void
(define (set-in-color-scheme color-name clr/sd)
  (define table (preferences:get (color-scheme-entry-name->pref-name color-name)))
  (define current-color-scheme (get-current-color-scheme))
  (define scheme-name (color-scheme-name current-color-scheme))
  (define new-table
    (cond
      [(set-member? known-style-names color-name)
       ;; it would be good to be able to use hash-remove here when 
       (hash-set table scheme-name clr/sd)]
      [else
       (define color (->color-object clr/sd))
       (define default 
         (hash-ref (color-scheme-mapping current-color-scheme)
                   color-name
                   #f))
       (cond
         [(and default (same-color? color default))
          (hash-remove table scheme-name)]
         [else
          (hash-set table scheme-name color)])]))
  (preferences:set (color-scheme-entry-name->pref-name color-name) new-table))

(define (->color-object clr)
  (if (string? clr)
      (send the-color-database find-color clr)
      clr))

(define (same-color? c1 c2)
  (and (= (send c1 red) (send c2 red))
       (= (send c1 green) (send c2 green))
       (= (send c1 blue) (send c2 blue))
       (= (send c1 alpha) (send c2 alpha))))

(define (get-current-color-scheme)
  ;; if pref not recognized, return white-on-black color scheme
  ;; so that if some color scheme goes away, we have
  ;; some reasonable backup plan (and, if it comes back
  ;; we don't lose the prefs)
  (define pref-val (preferences:get 'framework:color-scheme))
  (define found-color-scheme (lookup-color-scheme pref-val))
  (cond
    [found-color-scheme found-color-scheme]
    [else (car known-color-schemes)]))

(define (get-current-color-scheme-name)
  (color-scheme-name (get-current-color-scheme)))

;; string -> (or/c #f color-scheme?)
(define (lookup-color-scheme name)
  (for/or ([known-color-scheme (in-list known-color-schemes)])
    (and (equal? name (color-scheme-name known-color-scheme))
         known-color-scheme)))

(define (set-current-color-scheme name [avoid-shortcircuit? #f])
  (define color-scheme
    (or (for/or ([known-color-scheme (in-list known-color-schemes)])
          (and (equal? name (color-scheme-name known-color-scheme))
               known-color-scheme))
        (car known-color-schemes)))
  (when (or avoid-shortcircuit?
            (not (equal? (color-scheme-name color-scheme)
                         (color-scheme-name (get-current-color-scheme)))))
    (preferences:set 'framework:color-scheme (color-scheme-name color-scheme))
    (define old-wob (preferences:get 'framework:white-on-black?))
    (define new-wob (color-scheme-white-on-black-base? color-scheme))
    (unless (equal? old-wob new-wob)
      (preferences:set 'framework:white-on-black? new-wob)
      (if new-wob
          (white-on-black)
          (black-on-white)))
    (for ([(color-name fns) (in-hash color-change-callbacks)])
      (for ([fn/b (in-list fns)])
        (define fn (if (weak-box? fn/b) (weak-box-value fn/b) fn/b))
        (when fn
          (fn (lookup-in-color-scheme color-name)))))))

(define (get-available-color-schemes) 
  (for/list ([(name a-color-scheme) (in-hash known-color-schemes)])
    name))

(define (register-color-scheme-entry-change-callback color fn [weak? #f])
  (define wb/f (if weak? (make-weak-box fn) fn))
  ;; so we know which callbacks to call when a color scheme change happens
  (hash-set! color-change-callbacks 
             color
             (cons wb/f
                   (remove-gones (hash-ref color-change-callbacks color '()))))
  ;; so that individual color changes in a given scheme get callbacks
  (define remover
    (preferences:add-callback
     (color-scheme-entry-name->pref-name color)
     (λ (pref ht)
       (define fn
         (cond
           [(weak-box? wb/f)
            (define fn (weak-box-value wb/f))
            (unless fn (remover))
            fn]
           [else wb/f]))
       (when fn
         (fn (lookup-in-color-scheme/given-mapping 
              color
              ht
              (get-current-color-scheme)))))))
  (void))

(define (remove-gones lst)
  (for/list ([x (in-list lst)]
             #:when (or (not (weak-box? x))
                        (weak-box-value x)))
    x))

(define (known-color-scheme-name? n) 
  (or (set-member? known-color-names n) 
      (set-member? known-style-names n)))

(define (color-scheme-style-name? n)
  (set-member? known-style-names n))

(define (color-scheme-entry-name->pref-name sym)
  (string->symbol (format "color-scheme-entry:~a" sym)))

(define name->style-name (make-hash))

(define (add-color-scheme-entry name _b-o-w-color _w-o-b-color 
                                #:style [style-name #f]
                                #:bold? [bold? #f]
                                #:underline? [underline? #f]
                                #:italic? [italic? #f]
                                #:background [background #f])
  (define b-o-w-color (->color-object _b-o-w-color))
  (define w-o-b-color (->color-object _w-o-b-color))
  (cond
    [style-name
     (set! known-style-names (set-add known-style-names name))
     (hash-set! name->style-name name style-name)]
    [else
     (set! known-color-names (set-add known-color-names name))])
  (define (update-color scheme-name color)
    (hash-set! (color-scheme-mapping (lookup-color-scheme scheme-name))
               name
               (if style-name
                   (make-style-delta color bold? underline? italic? #:background background)
                   color)))
  (update-color white-on-black-color-scheme-name w-o-b-color)
  (update-color black-on-white-color-scheme-name b-o-w-color)
  (preferences:set-default (color-scheme-entry-name->pref-name name)
                           (hash)
                           (hash/c symbol?
                                   (if style-name 
                                       (is-a?/c style-delta%)
                                       (is-a?/c color%))
                                   #:immutable #t))
  (preferences:set-un/marshall 
   (color-scheme-entry-name->pref-name name)
   (λ (h)
     (for/hash ([(k v) (in-hash h)])
       (values k
               (if style-name
                   (marshall-style-delta v)
                   (vector (send v red) (send v green) (send v blue) (send v alpha))))))
   (λ (val)
     (cond
       [(and (list? val) (= (length val) (length style-delta-get/set)))
        ;; old style prefs; check to see if this user
        ;; was using the white on black or black on white and 
        ;; build the corresponding new pref
        (hash (if (preferences:get 'framework:white-on-black?)
                  white-on-black-color-scheme-name
                  black-on-white-color-scheme-name)
              (unmarshall-style-delta val))]
       [(unmarshall-color val)
        => 
        (λ (clr)
          ;; old color prefs; as above
          (hash (if (preferences:get 'framework:white-on-black?)
                    white-on-black-color-scheme-name
                    black-on-white-color-scheme-name)
                clr))]
       [(hash? val)
        ;; this may return a bogus hash, but the preferesnces system will check
        ;; and revert this to the default pref in that case
        (for/hash ([(k v) (in-hash val)])
          (values 
           k 
           (if style-name
               (unmarshall-style-delta v)
               (and (vector? v)
                    (= (vector-length v) 4)
                    (make-object color% 
                      (vector-ref v 0) (vector-ref v 1) 
                      (vector-ref v 2) (vector-ref v 3))))))]
       [else #f])))
  
  (when style-name
    (register-color-scheme-entry-change-callback
     name
     (λ (sd)
       (editor:set-standard-style-list-delta style-name sd)))
    (define init-value (lookup-in-color-scheme name))
    (editor:set-standard-style-list-delta style-name init-value)))

(define (add-color-scheme-preferences-panel #:extras [extras void])
  (preferences:add-panel
   (list (string-constant preferences-colors)
         (string-constant color-schemes))
   (λ (parent)
     (define vp 
       (new vertical-panel% 
            [parent parent]
            [style '(auto-vscroll)]))
     (extras vp)
     (define buttons
       (for/list ([color-scheme (in-list known-color-schemes)])
         (define hp (new horizontal-panel% 
                         [parent vp]
                         [alignment '(left top)]
                         [stretchable-height #t]))
         (define t (new racket:text%))
         (define str (color-scheme-example color-scheme))
         (send t insert str)
         (define ec (new editor-canvas% 
                         [parent hp]
                         [style '(auto-hscroll no-vscroll)]
                         [editor t]))
         (define (update-colors defaults?)
           (define bkg-name 'framework:basic-canvas-background)
           (send ec set-canvas-background
                 (lookup-in-color-scheme/given-mapping 
                  bkg-name
                  (if defaults?
                      (hash)
                      (preferences:get (color-scheme-entry-name->pref-name bkg-name)))
                  color-scheme))
           (send t set-style-list (color-scheme->style-list color-scheme defaults?)))
         (send ec set-line-count (+ 1 (for/sum ([c (in-string str)])
                                        (if (equal? c #\newline)
                                            1
                                            0))))
         (define bp (new vertical-panel% [parent hp] 
                         [stretchable-height #f]
                         [stretchable-width #f]))
         (define defaults? #f)
         (define btn
           (new button%
                [label (color-scheme-button-label color-scheme)]
                [parent bp]
                [callback (λ (x y) 
                            (set-current-color-scheme
                             (color-scheme-name color-scheme))
                            (when (and default-checkbox
                                       (send default-checkbox get-value))
                              (revert-to-color-scheme-defaults color-scheme)))]))
         (define default-checkbox
           (new check-box%
                [stretchable-width #t]
                [label "Revert to\ndefault colors"]
                [parent bp]
                [callback
                 (λ (x y)
                   (update-colors (send default-checkbox get-value)))]))
         (update-colors #f)
         btn))
     (define wid (apply max (map (λ (x) (send x get-width)) buttons)))
     (for ([b (in-list buttons)])
       (send b min-width wid))
     (void))))

(define (revert-to-color-scheme-defaults color-scheme)
  (define cs-name (color-scheme-name color-scheme))
  (for ([name (in-set (set-union known-style-names known-color-names))])
    (define pref-sym (color-scheme-entry-name->pref-name name))
    (define pref-hash (preferences:get pref-sym))
    (when (hash-ref pref-hash cs-name #f)
      (preferences:set pref-sym (hash-remove pref-hash cs-name)))))

(define (color-scheme->style-list color-scheme defaults?)
  (define style-list (new style-list%))
  
  (define standard-delta (make-object style-delta% 'change-normal))
  (send standard-delta set-delta 'change-family 'modern)
  (send standard-delta set-size-mult 0)
  (send standard-delta set-size-add (editor:get-current-preferred-font-size))
  (send standard-delta set-delta-face (preferences:get 'framework:standard-style-list:font-name))
  (send style-list new-named-style "Standard"
        (send style-list find-or-create-style
              (send style-list basic-style)
              standard-delta))
  (for ([name (in-set known-style-names)])
    (define pref-hash (preferences:get (color-scheme-entry-name->pref-name name)))
    (define delta
      (lookup-in-color-scheme/given-mapping
       name 
       (if defaults? (hash) pref-hash)
       color-scheme))
    (send style-list new-named-style
          (hash-ref name->style-name name)
          (send style-list find-or-create-style
                (send style-list find-named-style "Standard")
                delta)))
  style-list)
