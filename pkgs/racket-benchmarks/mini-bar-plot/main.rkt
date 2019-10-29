#lang racket/base
(require pict
         racket/draw
         racket/list
         racket/format)

(provide mini-bar-plot
         
         current-h-plot-sep
         current-v-plot-sep
         current-tt
         current-t
         
         generate-colors)

(define current-h-plot-sep (make-parameter 24))
(define (current-v-plot-sep) (* 2 (current-h-plot-sep)))

(define (default-t s) (text s 'swiss 32))
(define (default-tt s) (text s '(bold . modern) 32))

(define current-t (make-parameter default-t))
(define current-tt (make-parameter default-tt))

(define (mini-bar-plot names
                       timess
                       #:colors [colors (generate-colors names)]
                       #:t [t (current-t)]
                       #:tt [tt (current-tt)]
                       #:measurement-t [measurement-t tt]
                       #:bar-t [bar-t t]
                       #:h-plot-sep [h-plot-sep (current-h-plot-sep)]
                       #:v-plot-sep [v-plot-sep (* 2 h-plot-sep)]
                       #:columns [columns 1]
                       #:bar-sep [bar-sep 0]
                       #:bar-text-scale [bar-text-scale 0.5]
                       #:vertical-bars? [vertical-bars? #f]
                       #:width [W 200]
                       #:height [H (* 2 bar-text-scale (if vertical-bars? 70 30))]
                       #:normalize-max? [normalize-max? #f]
                       #:pin-max? [pin-max? #f]
                       #:max [starting-max 0]
                       #:sort-ratio [sort-ratio #f]
                       #:key->pict [key->pict (lambda (k) #f)]
                       #:reverse? [reverse? #f]
                       #:ghost-label [ghost-label #f]
                       #:details [details (map (lambda (v) #hasheq()) timess)]
                       #:detail-spec [detail-spec #f]
                       #:display-scale [display-scale 1]
                       #:decimal-places [decimal-places 0]
                       #:fill-vertical? [fill-vertical? #f]
                       #:widen? [widen? vertical-bars?]
                       #:pad-left [pad-left 0]
                       #:prefix [prefix ""]
                       #:suffix [suffix ""])
  (define keys ((if reverse? reverse values)
                (sort (hash-keys (car timess))
                      (lambda (a b)
                        (cond
                          [sort-ratio
                           (define (ratio a)
                             (/ (hash-ref (list-ref timess (car sort-ratio)) a)
                                (hash-ref (list-ref timess (cdr sort-ratio)) a)))
                           (< (ratio a) (ratio b))]
                          [else (symbol<? a b)])))))
  (define base-max (if normalize-max?
                       (for*/fold ([mx starting-max]) ([times (in-list timess)]
                                                       [t (in-hash-values times)])
                         (max mx t))
                       starting-max))
  (define (overlay-detail bar total detail key)
    (cond
      [(not detail-spec) bar]
      [else
       (let loop ([bar bar] [delta 0] [spec detail-spec])
         (cond
           [(null? spec) bar]
           [else
            (define t (hash-ref (hash-ref detail key) (caar spec) 0))
            (define dbar (colorize (filled-rectangle (* (pict-width bar) (/ t total))
                                                     H)
                                   (cdar spec)))
            (loop (rb-superimpose bar
                                  (inset dbar 0 0 delta 0))
                  (+ delta (pict-width dbar))
                  (cdr spec))]))]))
  (define (widen p) (if widen?
                        (let ([a (* 2/3 (pict-width p))])
                          (inset p (+ a pad-left) 0 a 0))
                        (if pad-left
                            (inset p pad-left 0 0 0)
                            p)))
  (define plots (for/list ([key (in-list keys)])
                  (define max-time (if pin-max?
                                       base-max
                                       (for/fold ([mx base-max]) ([times (in-list timess)])
                                         (max mx (hash-ref times key)))))
                  (vl-append
                   2
                   (or (key->pict key)
                       (measurement-t (~a key)))
                   (widen
                    (apply
                     (if vertical-bars? hb-append vr-append)
                     bar-sep
                     (for/list ([name (in-list names)]
                                [color (in-list colors)]
                                [times (in-list timess)]
                                [detail (in-list details)])
                       (define time (hash-ref times key))
                       ((if vertical-bars? (lambda (sep a b) (vc-append sep b a)) hc-append)
                        5
                        (let ([l (colorize (if (pict? name) name (t (~a name))) color)])
                          (let ([p (if ghost-label
                                       (if vertical-bars?
                                           (ctl-superimpose l (ghost ghost-label))
                                           (rtl-superimpose l (ghost ghost-label)))
                                       l)])
                            (if vertical-bars?
                                (let ([p2 (scale p (min 1 (/ H (pict-width p))))])
                                  (cc-superimpose p2 (blank 0 (pict-height p))))
                                p)))
                        (let ([lbl (scale (let ([tp (t (format "~a~a~a"
                                                             prefix
                                                             (~r (* time display-scale) #:precision decimal-places)
                                                             suffix))])
                                            tp)
                                          0.5)]
                              [bar (let ([bar (overlay-detail
                                               (colorize (if vertical-bars?
                                                             (filled-rectangle H (* W (/ time (max 1 max-time))))
                                                             (filled-rectangle (* W (/ time (max 1 max-time))) H))
                                                         color)
                                               time detail key)])
                                     (if (and pin-max? (time . > . max-time))
                                         (if vertical-bars?
                                             (inset bar 0 (- W (pict-height bar)) 0 0)
                                             (inset bar 0 0 (- W (pict-width bar)) 0))
                                         bar))])
                          (define labelled-bar
                             ((if vertical-bars? cb-superimpose lb-superimpose)
                              bar
                              (inset (colorize lbl "white")
                                     4)))
                          ((if vertical-bars? cb-superimpose lt-superimpose)
                           (pin-under (clip (refocus labelled-bar bar))
                                      lbl lt-find
                                      (colorize lbl color))
                           (if vertical-bars?
                               (blank H (if fill-vertical? W 0))
                               (blank W H)))))))))))
  (define (pad plots)
    (append plots
            (let ([n (remainder (length plots) columns)])
              (if (zero? n)
                  null
                  (make-list (- columns n) (blank))))))
  (table columns
         (pad plots)
         cc-superimpose cc-superimpose
         h-plot-sep v-plot-sep))

(define (generate-colors names #:used [used '()])
  (define nice-colors (for/list ([c '("red" "blue" "forestgreen" "purple" "orange")]
                                 #:unless (member c used))
                        c))
  (define n (length names))
  (define nice-n (length nice-colors))
  (if (n . <= . nice-n)
      (take nice-colors n)
      (append nice-colors
              (for ([i (in-range nice-n n)])
                (make-color (modulo (* i 101) 256)
                            (modulo (* i 203) 256)
                            (modulo (* i 53) 256))))))
