#lang racket/base

;; Functions that create legend entries and lists of legend entries.

(require racket/class racket/match racket/list racket/string racket/sequence racket/contract
         unstable/latent-contract/defthing
         "math.rkt"
         "contract.rkt"
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
  (legend-entry label (λ (pd x-size y-size)
                        (define y (* 1/2 y-size))
                        (send pd set-pen color width style)
                        (send pd set-alpha 1)
                        (send pd draw-line (vector 0 y) (vector x-size y)))))

(defproc (line-legend-entries [label string?] [zs (listof real?)] [z-labels (listof string?)]
                              [colors (plot-colors/c (listof real?))]
                              [widths (pen-widths/c (listof real?))]
                              [styles (plot-pen-styles/c (listof real?))]
                              ) (listof legend-entry?)
  (define hash
    (for/fold ([hash  empty]) ([z        (in-list zs)]
                               [z-label  (in-list z-labels)]
                               [color    (in-cycle (maybe-apply colors zs))]
                               [width    (in-cycle (maybe-apply widths zs))]
                               [style    (in-cycle (maybe-apply styles zs))])
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
                                 [color plot-color/c] [style plot-brush-style/c]
                                 [line-color plot-color/c] [line-width (>=/c 0)]
                                 [line-style plot-pen-style/c]) legend-entry?
  (legend-entry label (λ (pd x-size y-size)
                        (send pd set-brush color style)
                        (send pd set-pen line-color line-width line-style)
                        (send pd set-alpha 1)
                        (send pd draw-rect (vector (ivl 0 x-size) (ivl 0 y-size))))))

(defproc (rectangle-legend-entries [label string?] [zs (listof real?)]
                                   [colors (plot-colors/c (listof real?))]
                                   [styles (plot-brush-styles/c (listof real?))]
                                   [line-colors (plot-colors/c (listof real?))]
                                   [line-widths (pen-widths/c (listof real?))]
                                   [line-styles (plot-pen-styles/c (listof real?))]
                                   ) (listof legend-entry?)
  (define z-min (first zs))
  (define z-max (last zs))
  (define digits (digits-for-range z-min z-max))
  (define hash
    (for/fold ([hash  empty]) ([z           (in-list zs)]
                               [color       (in-cycle (maybe-apply colors zs))]
                               [style       (in-cycle (maybe-apply styles zs))]
                               [line-color  (in-cycle (maybe-apply line-colors zs))]
                               [line-width  (in-cycle (maybe-apply line-widths zs))]
                               [line-style  (in-cycle (maybe-apply line-styles zs))])
      (define entry-label (real->plot-label z digits))
      (assoc-cons hash (list color style line-color line-width line-style) entry-label)))
  
  (reverse
   (for/list ([entry  (in-list hash)])
     (match-define (cons (list color style line-color line-width line-style) vs) entry)
     (rectangle-legend-entry (if (= 1 (length vs))
                                 (format "~a = ~a" label (first vs))
                                 (format "~a ∈ {~a}" label (string-join (reverse vs) ",")))
                             color style line-color line-width line-style))))

;; ===================================================================================================
;; Interval legends

(defproc (interval-legend-entry
          [label string?]
          [color plot-color/c] [style plot-brush-style/c]
          [line-color plot-color/c] [line-width (>=/c 0)] [line-style plot-pen-style/c]
          [line1-color plot-color/c] [line1-width (>=/c 0)] [line1-style plot-pen-style/c]
          [line2-color plot-color/c] [line2-width (>=/c 0)] [line2-style plot-pen-style/c]
          ) legend-entry?
  (legend-entry label (λ (pd x-size y-size)
                        (send pd set-alpha 1)
                        ;; rectangle
                        (send pd set-pen line-color line-width line-style)
                        (send pd set-brush color style)
                        (send pd draw-rect (vector (ivl 0 x-size) (ivl 0 y-size)))
                        ;; bottom line
                        (send pd set-pen line1-color line1-width line1-style)
                        (send pd draw-line (vector 0 y-size) (vector x-size y-size))
                        ;; top line
                        (send pd set-pen line2-color line2-width line2-style)
                        (send pd draw-line (vector 0 0) (vector x-size 0)))))

(defproc (interval-legend-entries [label string?] [ivls (listof ivl?)] [ivl-labels (listof string?)]
                                  [colors (plot-colors/c (listof ivl?))]
                                  [styles (plot-brush-styles/c (listof ivl?))]
                                  [line-colors (plot-colors/c (listof ivl?))]
                                  [line-widths (pen-widths/c (listof ivl?))]
                                  [line-styles (plot-pen-styles/c (listof ivl?))]
                                  [line1-colors (plot-colors/c (listof ivl?))]
                                  [line1-widths (pen-widths/c (listof ivl?))]
                                  [line1-styles (plot-pen-styles/c (listof ivl?))]
                                  [line2-colors (plot-colors/c (listof ivl?))]
                                  [line2-widths (pen-widths/c (listof ivl?))]
                                  [line2-styles (plot-pen-styles/c (listof ivl?))]
                                  ) (listof legend-entry?)
  (define hash
    (for/fold ([hash  empty]) ([ivl-label    (in-list ivl-labels)]
                               [color        (in-cycle (maybe-apply colors ivls))]
                               [style        (in-cycle (maybe-apply styles ivls))]
                               [line-color   (in-cycle (maybe-apply line-colors ivls))]
                               [line-width   (in-cycle (maybe-apply line-widths ivls))]
                               [line-style   (in-cycle (maybe-apply line-styles ivls))]
                               [line1-color  (in-cycle (maybe-apply line1-colors ivls))]
                               [line1-width  (in-cycle (maybe-apply line1-widths ivls))]
                               [line1-style  (in-cycle (maybe-apply line1-styles ivls))]
                               [line2-color  (in-cycle (maybe-apply line2-colors ivls))]
                               [line2-width  (in-cycle (maybe-apply line2-widths ivls))]
                               [line2-style  (in-cycle (maybe-apply line2-styles ivls))])
      (assoc-cons hash
                  (list color style line-color line-width line-style
                        line1-color line1-width line1-style
                        line2-color line2-width line2-style)
                  ivl-label)))
  
  (reverse
   (for/list ([entry  (in-list hash)])
     (match-define (cons (list color style line-color line-width line-style
                               line1-color line1-width line1-style
                               line2-color line2-width line2-style)
                         ivl-labels)
       entry)
     (interval-legend-entry (format "~a ∈ ~a" label (string-join (reverse ivl-labels) " ∪ "))
                            color style line-color line-width line-style
                            line1-color line1-width line1-style
                            line2-color line2-width line2-style))))

;; ===================================================================================================
;; Point legends

(defproc (point-legend-entry [label string?] [sym point-sym/c]
                             [color plot-color/c] [fill-color plot-color/c]
                             [size (>=/c 0)] [line-width (>=/c 0)]) legend-entry?
  (legend-entry label (λ (pd x-size y-size)
                        (send pd set-pen color line-width 'solid)
                        (send pd set-brush fill-color 'solid)
                        (send pd set-alpha 1)
                        (send pd draw-glyphs (list (vector (* 1/2 x-size) (* 1/2 y-size))) sym size))))

(defproc (arrow-legend-entry [label string?] [color plot-color/c]
                             [line-width (>=/c 0)] [line-style plot-pen-style/c]
                             ) legend-entry?
  (legend-entry label (λ (pd x-size y-size)
                        (send pd set-pen color line-width line-style)
                        (send pd set-alpha 1)
                        (send pd draw-arrow-glyph
                              (vector (* 1/2 x-size) (* 1/2 y-size))
                              (* 1/4 x-size) 0))))
