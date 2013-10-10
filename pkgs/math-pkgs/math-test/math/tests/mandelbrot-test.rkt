#lang typed/racket

(require typed/racket/base
         math/array
         typed/rackunit)

(: mandelbrot (Float Integer -> (Array Integer)))
(define (mandelbrot step max-iters)
  (define x (list->array (sequence->list (in-range -2.0 0.800001 step))))
  (define y (array-slice-ref (list->array (sequence->list (in-range -1.4 1.400001 step)))
                             (list ::... (::new 1))))
  (define c (array->fcarray (array-make-rectangular x y)))
  (define-values (z divtime)
    (for/fold: ([z : FCArray c]
                [divtime : (Array Integer)  (array max-iters)]
                ) ([i  (in-range max-iters)])
      (let ([z  (fcarray+ (fcarray-sqr z) c)])
        (values z (array-strict (array-if (array-and (array> (fcarray-magnitude z) (array 4.0))
                                                     (array= divtime (array max-iters)))
                                          (array i)
                                          divtime))))))
  divtime)

(define truth
  (array #[#[0 1 1 1 1 1 1 1 1 1 1 1 1 1 1]
           #[1 1 1 1 1 1 2 2 2 3 2 2 1 1 1]
           #[1 1 1 1 1 2 2 2 3 6 20 3 2 1 1]
           #[1 1 1 2 2 2 3 4 5 20 17 4 3 2 1]
           #[1 1 2 2 3 3 4 11 20 20 20 10 14 2 2]
           #[1 2 3 4 6 6 6 20 20 20 20 20 9 3 2]
           #[2 3 4 6 18 20 14 20 20 20 20 20 20 3 2]
           #[20 20 20 20 20 20 20 20 20 20 20 20 5 3 2]
           #[2 3 4 6 18 20 14 20 20 20 20 20 20 3 2]
           #[1 2 3 4 6 6 6 20 20 20 20 20 9 3 2]
           #[1 1 2 2 3 3 4 11 20 20 20 10 14 2 2]
           #[1 1 1 2 2 2 3 4 5 20 17 4 3 2 1]
           #[1 1 1 1 1 2 2 2 3 6 20 3 2 1 1]
           #[1 1 1 1 1 1 2 2 2 3 2 2 1 1 1]
           #[0 1 1 1 1 1 1 1 1 1 1 1 1 1 1]]))

(check-true
 (equal? (mandelbrot 0.2 20)
         truth))

(check-true
 (equal? (parameterize ([array-strictness #f])
           (mandelbrot 0.2 20))
         truth))

#;
(begin
  (require images/flomap)
  
  (: array->flomap ((Array Real) -> flomap))
  (define (array->flomap arr)
    (let ([arr (array->flarray arr)])
      (define ds (array-shape arr))
      (match ds
        [(vector h w)  (flomap (flarray-data arr) 1 w h)]
        [(vector h w c)  (flomap (flarray-data arr) c w h)]
        [_  (error 'array->flomap "expected array with 2 or 3 dimensions; given shape ~e" ds)])))
  
  (define fm
    (array->flomap
     (time (mandelbrot 0.007 20))))
  
  (flomap-size fm)
  
  (flomap->bitmap (flomap-normalize fm)))
