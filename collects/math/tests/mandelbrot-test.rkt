#lang typed/racket

(require math/array
         typed/rackunit)

(: mandelbrot (Float Integer -> (Array Integer)))
(define (mandelbrot step max-iters)
  (define xs (sequence->list (in-range -2.0 0.800001 step)))
  (define ys (sequence->list (in-range -1.4 1.400001 step)))
  (define x ((inst list->array Float) xs flonum?))
  (define y (array-slice-ref ((inst list->array Float) ys flonum?) (list ::... (::new 1))))
  (define c (array->fcarray (array+ x (array-scale y +1.0i))))
  (define-values (z divtime)
    (for/fold: ([z : FCArray c]
                [divtime : (Array Integer)  (make-array (array-shape c) max-iters)]
                ) ([i  (in-range max-iters)])
      (let ([z  (fcarray+ (fcarray-sqr z) c)])
        (values z (array-strict (array-if (array-and (flarray> (fcarray-magnitude z) (flarray 4.0))
                                                     (array= divtime (array max-iters)))
                                          (array i)
                                          divtime))))))
  divtime)

(check-equal? (mandelbrot 0.2 20)
              (array [[0 1 1 1 1 1 1 1 1 1 1 1 1 1 1]
                      [1 1 1 1 1 1 2 2 2 3 2 2 1 1 1]
                      [1 1 1 1 1 2 2 2 3 6 20 3 2 1 1]
                      [1 1 1 2 2 2 3 4 5 20 17 4 3 2 1]
                      [1 1 2 2 3 3 4 11 20 20 20 10 14 2 2]
                      [1 2 3 4 6 6 6 20 20 20 20 20 9 3 2]
                      [2 3 4 6 18 20 14 20 20 20 20 20 20 3 2]
                      [20 20 20 20 20 20 20 20 20 20 20 20 5 3 2]
                      [2 3 4 6 18 20 14 20 20 20 20 20 20 3 2]
                      [1 2 3 4 6 6 6 20 20 20 20 20 9 3 2]
                      [1 1 2 2 3 3 4 11 20 20 20 10 14 2 2]
                      [1 1 1 2 2 2 3 4 5 20 17 4 3 2 1]
                      [1 1 1 1 1 2 2 2 3 6 20 3 2 1 1]
                      [1 1 1 1 1 1 2 2 2 3 2 2 1 1 1]
                      [0 1 1 1 1 1 1 1 1 1 1 1 1 1 1]]))

#;
(begin
  (require images/flomap)
  
  (: array->flomap ((Array Real) -> flomap))
  (define (array->flomap arr)
    (match-define (vector h w) (array-shape arr))
    (build-flomap 1 w h (λ (k x y) (real->double-flonum (array-ref arr (vector y x))))))
  
  (define fm
    (array->flomap
     (time (mandelbrot 0.007 20))))
  
  (flomap-size fm)
  
  (flomap->bitmap (flomap-normalize fm)))
