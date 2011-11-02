#lang racket

(require plot plot/utils plot/common/contract-doc)

(struct label-params (score z z-ivl str v) #:transparent)

(define (dnorm x m s^2)
  (* (/ 1 (sqrt (* 2 pi s^2))) (exp (* -1/2 (/ (sqr (- x m)) s^2)))))

(define ((contour-labels-render-proc f g levels samples color size family alpha) area)
  (let/ec return
    (define-values (x-min x-max y-min y-max) (send area get-bounds))
    (match-define (list xs ys zss) (g x-min x-max samples y-min y-max samples))
    
    (define-values (z-min z-max)
      (let ([zs  (filter regular? (2d-sample->list zss))])
        (when (empty? zs) (return empty))
        (values (apply min* zs) (apply max* zs))))
    
    (define z-ticks (contour-ticks z-min z-max levels #f))
    (define zs (map pre-tick-value z-ticks))
    
    (send area set-text-foreground color)
    (send area set-font size family)
    (send area set-alpha alpha)
    (define labels
      (append*
       (for/list ([z-tick  (in-list z-ticks)])
        (match-define (tick z major? label) z-tick)
        (for/list ([ya  (in-list ys)]
                   [yb  (in-list (rest ys))]
                   [zs0  (in-vector zss)]
                   [zs1  (in-vector zss 1)]
                   #:when #t
                   [xa  (in-list xs)]
                   [xb  (in-list (rest xs))]
                   [z1  (in-vector zs0)]
                   [z2  (in-vector zs0 1)]
                   [z3  (in-vector zs1 1)]
                   [z4  (in-vector zs1)]
                   #:when #t
                   [line  (in-list (heights->lines xa xb ya yb z z1 z2 z3 z4))])
          (match-define (vector x y _) (vcenter line))
          (label-params 0 z
                        (ivl (min* z1 z2 z3 z4) (max* z1 z2 z3 z4))
                        label (send area plot->dc (vector x y)))))))
    
    (match-define (vector dc-x-min dc-y-min) (send area plot->dc (vector x-min y-min)))
    (match-define (vector dc-x-max dc-y-max) (send area plot->dc (vector x-max y-max)))
    
    (define x-sigma (/ (plot-width) samples))
    (define y-sigma (/ (plot-height) samples))
    (define z-sigma (/ (- z-max z-min) (length z-ticks)))
    
    (define new-labels
      (for/fold ([labels labels]) ([keep  (in-list (list 4))])
        (define new-labels
          (for/list ([l1  (in-list labels)])
            (match-define (label-params s1 z1 (ivl z1-min z1-max) str1 (vector x1 y1)) l1)
            (define new-score
              (apply + (for/list ([l2  (in-list labels)])
                         (match-define (label-params s2 z2 _ str2 (vector x2 y2)) l2)
                         (* (exp (* -1/2 (sqr (/ (- s1 s2) 1))))
                            (exp (* -1/2 (sqr (/ (- x1 x2) x-sigma))))
                            (exp (* -1/2 (sqr (/ (- y1 y2) y-sigma))))
                            (exp (* -1/2 (sqr (/ (- z1 z2) z-sigma))))
                            ))))
            (label-params new-score z1 (ivl z1-min z1-max) str1 (vector x1 y1))))
        (append*
         (for/list ([z  (in-list zs)])
           (define z-labels (sort (filter (λ (l) (= z (label-params-z l))) new-labels)
                                  > #:key label-params-score))
           #;(define keep (min 4 (length z-labels) (round (* 1/8 (length z-labels)))))
           (take z-labels (min keep (length z-labels)))))))
    
    (for ([label  (in-list new-labels)])
      (match-define (label-params score z _ str (vector x y)) label)
      (send area draw-text #;(real->plot-label score 3) str (vector x y) 'center 0 #:outline? #t))
    
    empty))

(defproc (contour-labels
          [f (real? real? . -> . real?)]
          [x-min (or/c real? #f) #f] [x-max (or/c real? #f) #f]
          [y-min (or/c real? #f) #f] [y-max (or/c real? #f) #f]
          [#:levels levels (or/c 'auto exact-positive-integer? (listof real?)) (contour-levels)]
          [#:samples samples (and/c exact-integer? (>=/c 2)) (contour-samples)]
          [#:color color plot-color/c (plot-foreground)]
          [#:size size (>=/c 0) (plot-font-size)]
          [#:family family font-family/c (plot-font-family)]
          [#:alpha alpha (real-in 0 1) (label-alpha)]
          ) renderer2d?
  (define g (2d-function->sampler f))
  (renderer2d (vector (ivl x-min x-max) (ivl y-min y-max)) #f #f
              (contour-labels-render-proc f g levels samples color size family alpha)))

;(contour-samples 11)
;(plot-z-max-ticks 50)
#;
(parameterize (#;[plot-x-transform  log-transform]
               #;[plot-y-transform  log-transform])
  (plot (list (contours (λ (x y) (sqrt (+ (sqr x) (sqr y)))) -1 4 -1 4)
              (contour-labels (λ (x y) (sqrt (+ (sqr x) (sqr y))))))))
