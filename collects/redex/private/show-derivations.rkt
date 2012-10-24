#lang racket/base
(require racket/class
         racket/gui/base
         mrlib/graph
         racket/match
         racket/pretty
         "size-snip.rkt"
         "judgment-form.rkt")

(provide show-derivations)

(define sub-derivation-horizontal-gap 20)
(define sub-derivation-vertical-gap 10) ;; must be even

(define (show-derivations derivations
                          #:pp [pp default-pretty-printer]
                          #:racket-colors? [racket-colors? #f])
  (define cw (initial-char-width))
  (define f (new frame% [label "PLT Redex Judgment Form Derivations"] [width 400] [height 400]))
  (define pb (new derivation-pb%))
  (define ec (new editor-canvas% 
                  [parent f]
                  [editor pb]))
  (send f reflow-container)
  (define top-snip (fill-derivation-pb pb (car derivations) pp racket-colors? cw))
  (define controls-panel (new vertical-panel% [parent f] [stretchable-height #f]))
  
  (define (set-all-cws cw)
    (let loop ([snip (send pb find-first-snip)])
      (when snip
        (when (is-a? snip graph-editor-snip%)
          (send snip set-char-width cw))
        (loop (send snip next)))))
  
  (define char-width-slider 
    (and (number? cw)
         (new slider%
              [parent controls-panel]
              [min-value 10]
              [max-value 100]
              [init-value cw]
              [label "Pretty Print Width"]
              [callback
               (λ (_1 _2)
                 (send pb begin-edit-sequence)
                 (set-all-cws (send char-width-slider get-value))
                 (send top-snip relayout-derivation pb)
                 (send pb end-edit-sequence))]))) 
  (send f show #t))

(define (fill-derivation-pb pb derivation pp racket-colors? cw)
  (define top-snip
    (let loop ([derivation derivation])
      (define children
        (for/fold ([children '()]) ([sub (in-list (derivation-subs derivation))])
          (define child (loop sub))
          (cons child children)))
      (define line-snip (new line-snip%))
      (define snip (make-snip (derivation-term derivation) 
                              children
                              pp
                              racket-colors?
                              (get-user-char-width 
                               cw
                               (derivation-term derivation))
                              line-snip))
      (send snip set-derivation-children children)
      (send pb insert snip)
      (send pb insert line-snip)
      snip))
  (send top-snip relayout-derivation pb)
  top-snip)

(define derivation-pb%
  (resizing-pasteboard-mixin
   (graph-pasteboard-mixin
    pasteboard%)))

(define (make-snip expr children pp code-colors? cw line-snip)
  (let* ([text (new size-text%)]
         [es (instantiate graph-editor-snip% ()
               [char-width cw]
               [editor text]
               [pp pp]
               [expr expr]
               [with-border? #f]
               [line-snip line-snip])])
    (send text set-autowrap-bitmap #f)
    (send text set-max-width 'none)
    (send text freeze-colorer)
    (unless code-colors?
      (send text stop-colorer #t))
    (send es format-expr)
    es))

(define graph-editor-snip%
  (class* (graph-snip-mixin size-editor-snip%) (reflowing-snip<%>)
    (define derivation-children '())
    (define/public (set-derivation-children c) (set! derivation-children c))
    (init-field line-snip)
    
    (define/public (relayout-derivation pb)
      (define table (make-hash))
      (resize-derivation pb table)
      (layout-derivation table pb 0 0))
    
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
        (define derivation-width
          (max sub-derivation-width
               (find-snip-width pb this)))
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
      (define my-x (+ dx (- (/ derivation-width 2) (/ my-width 2))))
      (define my-y (+ dy derivation-height (- my-height)))
      (define children-width
        (for/sum ([child (in-list derivation-children)])
          (car (hash-ref table child))))
      (define start-dx (+ dx (/ (- derivation-width children-width) 2)))
      (send pb move-to this my-x my-y)
      (send pb move-to line-snip dx (- my-y (/ sub-derivation-vertical-gap 2)))
      (send line-snip set-width derivation-width)
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
    (define width 10)
    (define/public (set-width w) (set! width w))
    (define/override (draw dc x y  left top right bottom dx dy draw-caret)
      (send dc draw-line x y (+ x width) y))
    (define/override (get-extent dc x y wb hb db sb lb rb)
      (set-box/f wb width)
      (set-box/f hb 1)
      (set-box/f db 0)
      (set-box/f sb 0)
      (set-box/f lb 0)
      (set-box/f rb 0))
    (inherit set-snipclass)
    (super-new)
    (set-snipclass line-snipclass)))

(define (set-box/f b v) (when (box? b) (set-box! b v)))

(define line-snipclass (new snip-class%))
(send line-snipclass set-classname "redex:derivation-line")
(send line-snipclass set-version 1)
(send (get-the-snip-class-list) add line-snipclass)
