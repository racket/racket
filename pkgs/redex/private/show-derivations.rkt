#lang racket/base
(require racket/class
         racket/gui/base
         racket/match
         racket/pretty
         framework
         "size-snip.rkt"
         "judgment-form.rkt"
         "traces.rkt")

(provide show-derivations
         derivation/ps)

(define sub-derivation-horizontal-gap 20)
(define sub-derivation-vertical-gap 10) ;; must be even

(define (derivation/ps derivation filename
                       #:pp [pp default-pretty-printer]
                       #:racket-colors? [racket-colors? #f]
                       #:post-process [post-process void])
  (define-values (ec pb)
    (parameterize ([actually-show-window #f])
      (show-derivations (list derivation)
                        #:pp pp
                        #:racket-colors? racket-colors?)))
  (post-process pb)
  (print-to-ps pb ec filename))

(define actually-show-window (make-parameter #t))

(define (show-derivations derivations
                          #:pp [pp default-pretty-printer]
                          #:racket-colors? [racket-colors? #f]
                          #:init-derivation [init-derivation 0])
  (define init-cw (initial-char-width))
  (define f (new (class deriv-frame%
                   (define size-callback-queued? #f)
                   (define/override (on-size w h)
                     (unless size-callback-queued?
                       (set! size-callback-queued? #t)
                       (queue-callback
                        (λ ()
                          (set! size-callback-queued? #f)
                          (send pb begin-edit-sequence)
                          (send pb re-run-layout)
                          (send pb end-edit-sequence))
                        #f))
                     (super on-size w h))
                   (super-new [label "PLT Redex Judgment Form Derivations"]
                              [width 400]
                              [height 400]))))
  (define ac (send f get-area-container))
  (define pb #f)
  (define current-derivation #f)
  (define ec (new editor-canvas% 
                  [parent ac]))
  (send f reflow-container)
    
  (define (show-derivation i)
    (set! current-derivation i)
    (set! pb (new derivation-pb%))
    (send ec set-editor pb)
    (send f reflow-container)
    (send pb begin-edit-sequence)
    (fill-derivation-pb pb (list-ref derivations i) pp racket-colors? 
                        (if char-width-slider
                            (send char-width-slider get-value)
                            init-cw))
    (send which-msg set-label (ith-label i))
    (send pb end-edit-sequence))
  
  (define controls-panel (new vertical-panel% [parent ac] [stretchable-height #f]))
  (define which-derivation-panel (new horizontal-panel% [parent ac] [stretchable-height #f] [alignment '(right center)]))
  
  (define (next/prev-derivation dir label)
    (new button%
         [label label]
         [parent which-derivation-panel]
         [callback
          (λ (x y)
            (show-derivation (modulo (+ current-derivation dir)
                                     (length derivations))))]))
  (next/prev-derivation -1 "Prev Derivation")
  (define (ith-label i)
    (format "~a / ~a" (+ i 1) (length derivations)))
  (define which-msg
    (new message% 
         [label (ith-label (- (length derivations) 1))]
         [parent which-derivation-panel]))
  (next/prev-derivation +1 "Next Derivation")
  (when (<= (length derivations) 1)
    (send ac change-children
          (λ (l) (remq which-derivation-panel l))))
  
  (define (set-all-cws cw)
    (when pb
      (let loop ([snip (send pb find-first-snip)])
        (when snip
          (when (is-a? snip deriv-editor-snip%)
            (send snip set-char-width cw))
          (loop (send snip next))))))
  
  (define char-width-slider 
    (and (number? init-cw)
         (new slider%
              [parent controls-panel]
              [min-value 2]
              [max-value 100]
              [init-value init-cw]
              [label "Pretty Print Width"]
              [callback
               (λ (_1 _2)
                 (when pb
                   (send pb begin-edit-sequence)
                   (set-all-cws (send char-width-slider get-value))
                   (send pb re-run-layout)
                   (send pb end-edit-sequence)))])))
  (show-derivation 0)
  (cond
    [(actually-show-window)
     (send f show #t)]
    [else
     (values ec pb)]))

(define deriv-frame%
  (frame:standard-menus-mixin (frame:basic-mixin frame%)))

(define (fill-derivation-pb pb derivation pp racket-colors? cw)
  (define top-snip
    (let loop ([derivation derivation])
      (define children
        (reverse
         (for/fold ([children '()]) ([sub (in-list (derivation-subs derivation))])
           (define child (loop sub))
           (cons child children))))
      (define line-snip (new line-snip%))
      (define name-snip (and (derivation-name derivation)
                             (make-object string-snip% 
                               (format " [~a]" (derivation-name derivation)))))
      (define snip (make-snip (derivation-term derivation) 
                              children
                              pp
                              racket-colors?
                              (get-user-char-width 
                               cw
                               (derivation-term derivation))
                              line-snip
                              name-snip))
      (send snip set-derivation-children children)
      (send pb insert snip)
      (send pb insert line-snip)
      (when name-snip (send pb insert name-snip))
      snip))
  (send pb set-top-snip top-snip)
  (send pb re-run-layout))

(define derivation-pb%
  (class pasteboard%
    
    (define top-snip #f)
    (define/public (set-top-snip ts) (set! top-snip ts))
    (define/public (get-top-snip) top-snip)
    
    (define/public (re-run-layout)
      (define table (make-hash))
      (send top-snip resize-derivation this table)
      (define admin (send this get-admin))
      (define-values (init-x init-y)
        (cond
          [admin
           (define bw (box 0))
           (define bh (box 0))
           (send admin get-view #f #f bw bh)
           (match-define (cons derivation-width derivation-height) (hash-ref table top-snip))
           (values (max 0 (- (/ (unbox bw) 2) (/ derivation-width 2)))
                   (max 0 (- (/ (unbox bh) 2) (/ derivation-height 2))))]
          [else
           (values 0 0)]))
      (send top-snip layout-derivation table this init-x init-y))
    
    (define/augment (can-interactive-resize? evt) #f)
    (define/augment (can-interactive-move? evt) #f)
    (define/augment (can-select? snip on?) (not on?))
    
    (inherit get-focus-snip)
    
    (super-new)
    
    (inherit set-keymap)
    (set-keymap pb-km)))

(define pb-km (new keymap%))
(send pb-km add-function "set-focus"
      (λ (pb evt)
        (define-values (x y) (send pb dc-location-to-editor-location
                                   (send evt get-x)
                                   (send evt get-y)))
        (define snp (send pb find-snip x y))
        (cond
          [(not snp)
           (send pb set-caret-owner #f)]
          [(is-a? snp deriv-editor-snip%)
           (send pb set-caret-owner snp)])))
(send pb-km map-function "leftbutton" "set-focus")

(define deriv-text%
  (class size-text%
    (inherit get-admin)
    (define/override (on-focus on?)
      (define admin (get-admin))
      (when (is-a? admin editor-snip-editor-admin<%>)
        (define snip (send admin get-snip))
        (send snip show-border on?)))
    (super-new)))

(define (make-snip expr children pp code-colors? cw line-snip name-snip)
  (let* ([text (new deriv-text%)]
         [es (instantiate deriv-editor-snip% ()
               [char-width cw]
               [editor text]
               [pp pp]
               [expr expr]
               [with-border? #f]
               [line-snip line-snip]
               [name-snip name-snip])])
    (send text set-autowrap-bitmap #f)
    (send text set-max-width 'none)
    (send text freeze-colorer)
    (unless code-colors?
      (send text stop-colorer #t))
    (send es format-expr)
    es))

(define deriv-editor-snip%
  (class* size-editor-snip% ()
    (define derivation-children '())
    (define/public (set-derivation-children c) (set! derivation-children c))
    (init-field line-snip)
    (init-field name-snip)
    
    (define/public (resize-derivation pb table)
      (let loop ([derivation derivation])
        (define-values (children-width children-height)
          (for/fold ([width 0]
                     [height 0])
            ([child (in-list derivation-children)])
            (define-values (this-w this-h) (send child resize-derivation pb table))
            (values (+ width this-w)
                    (max height this-h))))
        (define sub-derivation-width
          (if (null? derivation-children)
              0
              (+ children-width (* (- (length derivation-children)
                                      1)
                                   sub-derivation-horizontal-gap))))
        (define name-width (if name-snip
                               (find-snip-width pb name-snip)
                               0))
        (define derivation-width
          (+ (max sub-derivation-width
                  (find-snip-width pb this))
             name-width))
        (define derivation-height
          (+ children-height
             sub-derivation-vertical-gap
             (find-snip-height pb this)))
        (hash-set! table this (cons derivation-width derivation-height))
        (values derivation-width derivation-height)))
    
    (define/public (layout-derivation table pb dx dy)
      (match-define (cons derivation-width derivation-height) (hash-ref table this))
      (define my-height (find-snip-height pb this))
      (define my-width (find-snip-width pb this))
      (define name-snip-width (if name-snip
                                  (find-snip-width pb name-snip)
                                  0))
      (define my-x (+ dx (- (/ (- derivation-width name-snip-width) 2) (/ my-width 2))))
      (define my-y (+ dy derivation-height (- my-height)))
      (define children-width
        (for/sum ([child (in-list derivation-children)])
          (car (hash-ref table child))))
      (define start-dx (+ dx (/ (- (- derivation-width name-snip-width) children-width) 2)))
      (send pb move-to this my-x my-y)
      (send pb move-to line-snip dx (- my-y (/ sub-derivation-vertical-gap 2)))
      (send line-snip set-width (- derivation-width name-snip-width))
      (when name-snip
        (define name-snip-height (find-snip-height pb name-snip))
        (send pb move-to name-snip 
              (+ dx derivation-width (- name-snip-width))
              (- my-y (/ sub-derivation-vertical-gap 2) (/ name-snip-height 2))))
      (for/fold ([dx start-dx]) ([snip (in-list derivation-children)])
        (define that-ones-width (car (hash-ref table snip)))
        (define that-ones-height (cdr (hash-ref table snip)))
        (send snip layout-derivation table
              pb
              dx
              (+ dy (- derivation-height that-ones-height my-height sub-derivation-vertical-gap)))
        (+ dx that-ones-width sub-derivation-horizontal-gap)))
    
    (super-new)))

(define line-snip%
  (class snip%
    (inherit get-admin)
    (define width 10)
    (define/public (set-width w) 
      (unless (equal? w width)
        (define admin (get-admin))
        (set! width w) 
        (when admin 
          (send admin resized this #f)
          (send admin needs-update this 0 0 w 1))))
    (define/override (copy)
      (define c (new line-snip%))
      (send c set-width width)
      c)
    (define/override (draw dc x y left top right bottom dx dy draw-caret)
      (define old-smoothing (send dc get-smoothing))
      (define old-pen (send dc get-pen))
      (send dc set-pen "black" 1 'transparent)
      (send dc set-brush "black" 'solid)
      (send dc set-smoothing 'aligned)
      (send dc draw-rectangle x y width 1)
      (send dc set-smoothing old-smoothing)
      (send dc set-pen old-pen))
    (define/override (get-extent dc x y wb hb db sb lb rb)
      (super get-extent dc x y wb hb db sb lb rb)
      (set-box/f wb width)
      (set-box/f hb 1))
    (inherit set-snipclass)
    (super-new)
    (set-snipclass line-snipclass)))

(define (set-box/f b v) (when (box? b) (set-box! b v)))

(define line-snipclass (new snip-class%))
(send line-snipclass set-classname "redex:derivation-line")
(send line-snipclass set-version 1)
(send (get-the-snip-class-list) add line-snipclass)
