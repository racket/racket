#lang racket/base

(require racket/flonum racket/list racket/promise racket/math racket/contract
         plot/custom plot/utils
         "../common/contract-doc.rkt"
         "../common/utils.rkt"
         "line.rkt")

(provide kde density)

(define (factorial n)
  (if (zero? n) 1 (* n (factorial (sub1 n)))))

;; make-kde/windowed : (vectorof flonum) flonum flonum flonum -> (listof flonum) -> (listof flonum)
;; (can assume that xs is sorted)
;; Make a naive KDE, but uses windows to keep from adding Gaussians more than max-dist away
(define ((make-kde/windowed xs h max-dist q) ys)
  (define-values (_i ps)
    (for/fold ([i 0] [ps empty]) ([y  (in-list ys)])
      (define new-i (vector-find-index (λ (x) ((flabs (fl- x y)) . fl<= . max-dist)) xs i))
      (cond [new-i
             (define new-j (vector-find-index (λ (x) ((flabs (fl- x y)) . fl> . max-dist)) xs new-i))
             (define p (apply + (for/list ([x  (in-vector xs new-i new-j)])
                                  (define z (fl/ (fl- x y) h))
                                  (fl* q (flexp (fl- 0.0 (fl* z z)))))))
             (values new-i (cons p ps))]
            [else
             (values 0 (cons 0.0 ps))])))
  (reverse ps))

;; make-kde/fast-gauss : natural (vectorof flonum) flonum flonum flonum (listof flonum)
;;                       -> (listof flonum) -> (listof flonum)
;; (can assume that xs is sorted)
;; Make a KDE using the Improved Fast Gauss Transform
;; Using the algorithm published in:
;;   Changjiang Yang, Ramani Duraiswami, Nail A. Gumerov and Larry Davis
;;   "Improved Fast Gauss Transform and Efficient Kernel Density Estimation"
;;   Proceedings of the Ninth IEEE International Conference on Computer Vision (ICCV 2003)
;; This also uses windows to keep from adding terms in Css more than max-dist away
(define (make-kde/fast-gauss p xs h max-dist q bin-bounds)
  ;; Calculate the centers of each bin
  (define x*s (for/list ([x1  (in-list bin-bounds)] [x2  (in-list (rest bin-bounds))])
                (cond [(eqv? x1 -inf.0)  x2]
                      [(eqv? x2 +inf.0)  x1]
                      [else  (fl* 0.5 (+ x1 x2))])))
  
  ;; Precalculate multiplicative factors
  (define scales (for/list ([a  (in-range p)])
                   (fl/ (exact->inexact (expt 2.0 a))
                        (exact->inexact (factorial a)))))
  
  ;; Calculate per-x*, per-a constants Css
  (define-values (_i Css)
    (for/fold ([i 0] [Css empty]) ([x*  (in-list x*s)])
      (define new-i (vector-find-index (λ (x) ((flabs (fl- x* x)) . fl<= . max-dist)) xs i))
      ;; A delay keeps this from evaluating until asking for the KDE in a range near this x*
      (define Cs
        (delay
          (cond [new-i
                 (define new-j (vector-find-index (λ (x) ((flabs (fl- x* x)) . fl> . max-dist))
                                                  xs new-i))
                 (for/list ([a  (in-range p)] [scale  (in-list scales)])
                   (* scale (apply + (for/list ([x  (in-vector xs new-i new-j)])
                                       (define zx (fl/ (fl- x x*) h))
                                       (fl* q (fl* (flexp (fl- 0.0 (fl* zx zx)))
                                                   (exact->inexact (expt zx a))))))))]
                [else  (build-list p (λ _ 0.0))])))
      (values (if new-i new-i 0) (cons Cs Css))))
  
  (λ (ys)
    (define yss (bin-samples bin-bounds ys))
    (append*
     (for/list ([x*  (in-list x*s)] [Cs  (in-list (reverse Css))] [ys  (in-list yss)])
       (for/list ([y  (in-list ys)])
         (apply + (for/list ([a  (in-range p)] [C  (in-list (force Cs))])
                    (define zy (fl/ (fl- y x*) h))
                    (fl* C (fl* (flexp (fl- 0.0 (fl* zy zy)))
                                (exact->inexact (expt zy a)))))))))))

;; The number of series terms to compute
;; Making this odd ensures fast-gauss doesn't return negatives (the series partial sums alternate +/-)
(define series-terms 9)

(defproc (kde [xs (listof real?)] [h real?]) (values mapped-function?
                                                     (or/c real? #f)
                                                     (or/c real? #f))
  (if (empty? xs)
      (values (mapped-function (λ (y) 0) (λ (ys) (map (λ _ 0.0) ys))) #f #f)
      (let* ([xs  (list->vector (sort (map exact->inexact xs) fl<))]
             [h   (exact->inexact h)])
        (define N (vector-length xs))
        (define q (fl/ 1.0 (exact->inexact N)))
        (define c (fl/ 1.0 (fl* (sqrt pi) h)))
        (define max-dist (fl* h 5.0))
        ;; The range of non-zero KDE values
        (define x-min (fl- (vector-ref xs 0) max-dist))
        (define x-max (fl+ (vector-ref xs (sub1 N)) max-dist))
        ;; Parameters for fast-gauss
        (define K (inexact->exact (flceiling (fl/ (fl- x-max x-min) h))))
        (define p series-terms)
        ;; Make the KDE functions
        (define kde/windowed (make-kde/windowed xs h max-dist q))
        (define kde/fast-gauss
          (delay
            (define bin-bounds (append (list -inf.0) (linear-seq x-min x-max (+ K 1)) (list +inf.0)))
            (make-kde/fast-gauss p xs h max-dist q bin-bounds)))
        (define fmap
          (sorted-apply
           (λ (ys) (sort ys <))
           (λ (ys)
             (let ([ys  (map exact->inexact ys)])
               (define first-ps (build-list (count (λ (y) (y . fl< . x-min)) ys) (λ _ 0.0)))
               (define last-ps (build-list (count (λ (y) (y . fl> . x-max)) ys) (λ _ 0.0)))
               (define mid-ys (filter (λ (y) (and (x-min . fl<= . y) (y . fl<= . x-max))) ys))
               (define mid-ps
                 (cond [(empty? mid-ys)  empty]
                       [else
                        (define M (length mid-ys))
                        ;; Use the KDE algorithms' asymptotic complexity to decide which to use
                        (define fast-gauss-time (+ M (* K p N)))
                        (define windowed-time   (* M N))
                        ;(printf "est. fast-gauss-time = ~v~n" (exact->inexact fast-gauss-time))
                        ;(printf "est. windowed-time   = ~v~n" (exact->inexact windowed-time))
                        ;; A bit of testing shows these to be fairly accurate estimates of actual time
                        ;; (proportional to a constant)
                        ;; So it seems the algorithms have similar multiplicative constants
                        (cond [(fast-gauss-time . < . windowed-time)  ((force kde/fast-gauss) mid-ys)]
                              [else                                   (kde/windowed mid-ys)])]))
               (append first-ps
                       (map (λ (p) (fl* p c)) mid-ps)
                       last-ps)))))
        (values (mapped-function (λ (x) (first (fmap (list x)))) fmap) x-min x-max))))

(defproc (density [xs (listof real?)] [bw-adjust real? 1]
                  [#:x-min x-min (or/c real? #f) #f] [#:x-max x-max (or/c real? #f) #f]
                  [#:y-min y-min (or/c real? #f) #f] [#:y-max y-max (or/c real? #f) #f]
                  [#:samples samples (and/c exact-integer? (>=/c 2)) (line-samples)]
                  [#:color color plot-color/c (line-color)]
                  [#:width width (>=/c 0) (line-width)]
                  [#:style style plot-pen-style/c (line-style)]
                  [#:alpha alpha (real-in 0 1) (line-alpha)]
                  [#:label label (or/c string? #f) #f]
                  ) renderer2d?
  (define n (length xs))
  (define sd (sqrt (- (/ (sum sqr xs) n) (sqr (/ (sum values xs) n)))))
  (define h (* bw-adjust 1.06 sd (expt n -0.2)))
  (define-values (f fx-min fx-max) (kde xs h))
  (let ([x-min  (if x-min x-min fx-min)]
        [x-max  (if x-max x-max fx-max)])
    (function f x-min x-max #:y-min y-min #:y-max y-max #:samples samples
              #:color color #:width width #:style style #:alpha alpha #:label label)))
