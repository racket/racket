#lang racket/base

;; Functions that create legend entries and lists of legend entries.

(require racket/class racket/match racket/list racket/string racket/sequence racket/contract
         "contract.rkt"
         "contract-doc.rkt"
         "format.rkt"
         "draw.rkt"
         "utils.rkt")

(provide (all-defined-out))

(struct legend-entry (label draw) #:transparent)

;; ===================================================================================================
;; Line legends

(defproc (line-legend-entry [label string?]
                            [color plot-color/c] [width (>=/c 0)] [style plot-pen-style/c]
                            ) legend-entry?
  (legend-entry label (λ (pd x-min x-max y-min y-max)
                        (define y (* 1/2 (+ y-min y-max)))
                        (send pd set-pen color width style)
                        (send pd set-alpha 1)
                        (send pd draw-line (vector x-min y) (vector x-max y)))))

(defproc (line-legend-entries [label string?] [zs (listof real?)] [z-labels (listof string?)]
                              [colors plot-colors/c] [widths pen-widths/c] [styles plot-pen-styles/c]
                              ) (listof legend-entry?)
  (define hash
    (for/fold ([hash  empty]) ([z        (in-list zs)]
                               [z-label  (in-list z-labels)]
                               [color    (in-cycle (maybe-apply/list colors zs))]
                               [width    (in-cycle (maybe-apply/list widths zs))]
                               [style    (in-cycle (maybe-apply/list styles zs))])
      (assoc-cons hash (list color width style) z-label)))
  
  (reverse
   (for/list ([entry  (in-list hash)])
     (match-define (cons args vs) entry)
     (apply line-legend-entry
            (cond [(= 1 (length vs))  (format "~a = ~a" label (first vs))]
                  [else  (format "~a ∈ {~a}" label (string-join (reverse vs) ","))])
            args))))

;; ===================================================================================================
;; Rectangle legends

(defproc (rectangle-legend-entry [label string?]
                                 [fill-color plot-color/c] [fill-style plot-brush-style/c]
                                 [line-color plot-color/c] [line-width (>=/c 0)]
                                 [line-style plot-pen-style/c]) legend-entry?
  (legend-entry label (λ (pd x-min x-max y-min y-max)
                        (send pd set-brush fill-color fill-style)
                        (send pd set-pen line-color line-width line-style)
                        (send pd set-alpha 1)
                        (send pd draw-rectangle (vector x-min y-min) (vector x-max y-max)))))

(defproc (rectangle-legend-entries [label string?] [zs (listof real?)]
                                   [fill-colors plot-colors/c] [fill-styles plot-brush-styles/c]
                                   [line-colors plot-colors/c] [line-widths pen-widths/c]
                                   [line-styles plot-pen-styles/c]) (listof legend-entry?)
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
      (define entry-label (real->plot-label z digits))
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

(defproc (interval-legend-entry
          [label string?]
          [fill-color plot-color/c] [fill-style plot-brush-style/c]
          [line-color plot-color/c] [line-width (>=/c 0)] [line-style plot-pen-style/c]
          [line1-color plot-color/c] [line1-width (>=/c 0)] [line1-style plot-pen-style/c]
          [line2-color plot-color/c] [line2-width (>=/c 0)] [line2-style plot-pen-style/c]
          ) legend-entry?
  (legend-entry label (λ (pd x-min x-max y-min y-max)
                        (send pd set-alpha 1)
                        ;; rectangle
                        (send pd set-pen line-color line-width line-style)
                        (send pd set-brush fill-color fill-style)
                        (send pd draw-rectangle (vector x-min y-min) (vector x-max y-max))
                        ;; bottom line
                        (send pd set-pen line1-color line1-width line1-style)
                        (send pd draw-line (vector x-min y-max) (vector x-max y-max))
                        ;; top line
                        (send pd set-pen line2-color line2-width line2-style)
                        (send pd draw-line (vector x-min y-min) (vector x-max y-min)))))

(defproc (interval-legend-entries
          [label string?] [zs (listof real?)] [z-labels (listof string?)]
          [fill-colors plot-colors/c] [fill-styles plot-brush-styles/c]
          [line-colors plot-colors/c] [line-widths pen-widths/c] [line-styles plot-pen-styles/c]
          [line1-colors plot-colors/c] [line1-widths pen-widths/c] [line1-styles plot-pen-styles/c]
          [line2-colors plot-colors/c] [line2-widths pen-widths/c] [line2-styles plot-pen-styles/c]
          ) (listof legend-entry?)
  (define hash
    (for/fold ([hash  empty]) ([za           (in-list zs)]
                               [zb           (in-list (rest zs))]
                               [la           (in-list z-labels)]
                               [lb           (in-list (rest z-labels))]
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
      (define entry-label (format "[~a,~a]" la lb))
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

(defproc (contour-intervals-legend-entries
          [label string?] [zs (listof real?)] [z-labels (listof string?)]
          [fill-colors plot-colors/c] [fill-styles plot-brush-styles/c]
          [line-colors plot-colors/c] [line-widths pen-widths/c] [line-styles plot-pen-styles/c]
          [contour-colors plot-colors/c] [contour-widths pen-widths/c]
          [contour-styles plot-pen-styles/c]) (listof legend-entry?)
  (define n (- (length zs) 2))
  (define ccs (append (list 0)
                      (sequence-take (in-cycle (maybe-apply/list contour-colors zs)) 0 n)
                      (list 0)))
  (define cws (append (list 0)
                      (sequence-take (in-cycle (maybe-apply/list contour-widths zs)) 0 n)
                      (list 0)))
  (define css (append '(transparent)
                      (sequence-take (in-cycle (maybe-apply/list contour-styles zs)) 0 n)
                      '(transparent)))
  
  (interval-legend-entries label zs z-labels
                           fill-colors fill-styles line-colors line-widths line-styles
                           ccs cws css (rest ccs) (rest cws) (rest css)))

;; ===================================================================================================
;; Point legends

(defproc (point-legend-entry [label string?] [sym point-sym/c]
                             [color plot-color/c] [size (>=/c 0)] [line-width (>=/c 0)]) legend-entry?
  (legend-entry label (λ (pd x-min x-max y-min y-max)
                        (send pd set-pen color line-width 'solid)
                        (send pd set-alpha 1)
                        (send pd draw-glyphs
                              (list (vector (* 1/2 (+ x-min x-max)) (* 1/2 (+ y-min y-max))))
                              sym size))))

(defproc (vector-field-legend-entry [label string?] [color plot-color/c]
                                    [line-width (>=/c 0)] [line-style plot-pen-style/c]
                                    ) legend-entry?
  (legend-entry label (λ (pd x-min x-max y-min y-max)
                        (send pd set-pen color line-width line-style)
                        (send pd set-alpha 1)
                        (send pd draw-arrow-glyph
                              (vector (* 1/2 (+ x-min x-max)) (* 1/2 (+ y-min y-max)))
                              (* 1/4 (- x-max x-min)) 0))))
