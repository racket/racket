#lang racket/base

;; Functions that create legend entries and lists of legend entries.

(require racket/class racket/match racket/list racket/string racket/sequence racket/contract
         "format.rkt"
         "draw.rkt"
         "utils.rkt"
         "area.rkt")

(provide (all-defined-out))

;; ===================================================================================================
;; Line legends

(define (line-legend-entry label color width style)
  (legend-entry label (λ (plot-area x-min x-max y-min y-max)
                        (define y (* 1/2 (+ y-min y-max)))
                        (send plot-area set-pen color width style)
                        (send plot-area set-alpha 1)
                        (send plot-area draw-line (vector x-min y) (vector x-max y)))))

(define (line-legend-entries label zs colors widths styles)
  (define z-min (first zs))
  (define z-max (last zs))
  (define digits (digits-for-range z-min z-max))
  (define hash
    (for/fold ([hash  empty]) ([z      (in-list zs)]
                               [color  (in-cycle (maybe-apply/list colors zs))]
                               [width  (in-cycle (maybe-apply/list widths zs))]
                               [style  (in-cycle (maybe-apply/list styles zs))])
      (define entry-label (real->string/trunc z digits))
      (assoc-cons hash (list color width style) entry-label)))
  
  (reverse
   (for/list ([entry  (in-list hash)])
     (match-define (cons args vs) entry)
     (apply line-legend-entry
            (if (= 1 (length vs))
                (format "~a = ~a" label (first vs))
                (format "~a ∈ {~a}" label (string-join (reverse vs) ",")))
            args))))

;; ===================================================================================================
;; Rectangle legends

(define (rectangle-legend-entry label fill-color fill-style line-color line-width line-style)
  (legend-entry label (λ (plot-area x-min x-max y-min y-max)
                        (send plot-area set-brush fill-color fill-style)
                        (send plot-area set-pen line-color line-width line-style)
                        (send plot-area set-alpha 1)
                        (send plot-area draw-rectangle (vector x-min y-min) (vector x-max y-max)))))

(define (rectangle-legend-entries label zs fill-colors fill-styles line-colors line-widths line-styles)
  (define z-min (first zs))
  (define z-max (last zs))
  (define digits (digits-for-range z-min z-max))
  (define hash
    (for/fold ([hash  empty]) ([z           (in-list zs)]
                               [fill-color  (in-cycle (maybe-apply/list fill-colors zs))]
                               [fill-style  (in-cycle (maybe-apply/list fill-styles zs))]
                               [line-color  (in-cycle (maybe-apply/list line-colors zs))]
                               [line-width  (in-cycle (maybe-apply/list line-widths zs))]
                               [line-style  (in-cycle (maybe-apply/list line-styles zs))])
      (define entry-label (real->string/trunc z digits))
      (assoc-cons hash (list fill-color fill-style line-color line-width line-style) entry-label)))
  
  (reverse
   (for/list ([entry  (in-list hash)])
     (match-define (cons (list fill-color fill-style line-color line-width line-style) vs) entry)
     (rectangle-legend-entry (if (= 1 (length vs))
                                 (format "~a = ~a" label (first vs))
                                 (format "~a ∈ {~a}" label (string-join (reverse vs) ",")))
                             fill-color fill-style line-color line-width line-style))))

;; ===================================================================================================
;; Interval legends

(define (interval-legend-entry label fill-color fill-style line-color line-width line-style
                               line1-color line1-width line1-style
                               line2-color line2-width line2-style)
  (legend-entry label (λ (plot-area x-min x-max y-min y-max)
                        (send plot-area set-alpha 1)
                        ;; rectangle
                        (send plot-area set-pen line-color line-width line-style)
                        (send plot-area set-brush fill-color fill-style)
                        (send plot-area draw-rectangle (vector x-min y-min) (vector x-max y-max))
                        ;; bottom line
                        (send plot-area set-pen line1-color line1-width line1-style)
                        (send plot-area draw-line (vector x-min y-max) (vector x-max y-max))
                        ;; top line
                        (send plot-area set-pen line2-color line2-width line2-style)
                        (send plot-area draw-line (vector x-min y-min) (vector x-max y-min)))))

(define (interval-legend-entries label zs fill-colors fill-styles line-colors line-widths line-styles
                                 line1-colors line1-widths line1-styles
                                 line2-colors line2-widths line2-styles)
  (define z-min (first zs))
  (define z-max (last zs))
  (define digits (digits-for-range z-min z-max))
  (define hash
    (for/fold ([hash  empty]) ([za           (in-list zs)]
                               [zb           (in-list (rest zs))]
                               [fill-color   (in-cycle (maybe-apply/list fill-colors zs))]
                               [fill-style   (in-cycle (maybe-apply/list fill-styles zs))]
                               [line-color   (in-cycle (maybe-apply/list line-colors zs))]
                               [line-width   (in-cycle (maybe-apply/list line-widths zs))]
                               [line-style   (in-cycle (maybe-apply/list line-styles zs))]
                               [line1-color  (in-cycle (maybe-apply/list line1-colors zs))]
                               [line1-width  (in-cycle (maybe-apply/list line1-widths zs))]
                               [line1-style  (in-cycle (maybe-apply/list line1-styles zs))]
                               [line2-color  (in-cycle (maybe-apply/list line2-colors zs))]
                               [line2-width  (in-cycle (maybe-apply/list line2-widths zs))]
                               [line2-style  (in-cycle (maybe-apply/list line2-styles zs))])
      (define entry-label
        (format "[~a,~a]" (real->string/trunc za digits) (real->string/trunc zb digits)))
      (assoc-cons hash
                  (list fill-color fill-style line-color line-width line-style
                        line1-color line1-width line1-style
                        line2-color line2-width line2-style)
                  entry-label)))
  
  (reverse
   (for/list ([entry  (in-list hash)])
     (match-define (cons (list fill-color fill-style line-color line-width line-style
                               line1-color line1-width line1-style
                               line2-color line2-width line2-style)
                         vs)
       entry)
     (interval-legend-entry (format "~a ∈ ~a" label (string-join (reverse vs) " ∪ "))
                            fill-color fill-style line-color line-width line-style
                            line1-color line1-width line1-style
                            line2-color line2-width line2-style))))

(define (contour-intervals-legend-entries label z-min z-max zs
                                          fill-colors fill-styles line-colors line-widths line-styles
                                          contour-colors contour-widths contour-styles)
  (define interval-zs (append (list z-min) zs (list z-max)))
  
  (define ccs (append (list 0)
                      (sequence-take (in-cycle (maybe-apply/list contour-colors zs)) 0 (length zs))
                      (list 0)))
  (define cws (append (list 0)
                      (sequence-take (in-cycle (maybe-apply/list contour-widths zs)) 0 (length zs))
                      (list 0)))
  (define css (append '(transparent)
                      (sequence-take (in-cycle (maybe-apply/list contour-styles zs)) 0 (length zs))
                      '(transparent)))
  
  (interval-legend-entries label interval-zs
                           fill-colors fill-styles line-colors line-widths line-styles
                           ccs cws css (rest ccs) (rest cws) (rest css)))

;; ===================================================================================================
;; Point legends

(define (point-legend-entry label symbol color size line-width)
  (legend-entry label (λ (plot-area x-min x-max y-min y-max)
                        (send plot-area set-pen color line-width 'solid)
                        (send plot-area set-alpha 1)
                        (send plot-area draw-glyphs
                              (list (vector (* 1/2 (+ x-min x-max)) (* 1/2 (+ y-min y-max))))
                              symbol size))))

(define (vector-field-legend-entry label color line-width line-style)
  (legend-entry label (λ (plot-area x-min x-max y-min y-max)
                        (send plot-area set-pen color line-width line-style)
                        (send plot-area set-alpha 1)
                        (send plot-area draw-arrow-glyph
                              (vector (* 1/2 (+ x-min x-max)) (* 1/2 (+ y-min y-max)))
                              (* 1/4 (- x-max x-min)) 0))))
