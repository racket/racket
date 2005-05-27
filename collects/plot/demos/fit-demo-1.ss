(require (lib "plot.ss" "plot"))

(define x-vals (build-list 15 (lambda (x) x) ))
(define errors (build-list 15 (lambda (x) 1)))

(define (fun x)
  (* 3 (exp (* x -1 1.32))))
(define z-vals (map fun x-vals))

(define (gues-fun x a b)
  (* a (exp (* x -1 b))))

(define params 
  (fit gues-fun
       ((a 1) (b 1))
       (map vector x-vals z-vals errors)))

(plot (mix
       (points (map vector x-vals z-vals))
       (line (lambda (x)
               (apply gues-fun x (fit-result-final-params params)))))
      (x-min -1) (x-max 20)
      (y-min -1) (y-max 10))
