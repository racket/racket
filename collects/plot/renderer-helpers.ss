(module renderer-helpers mzscheme

  ; Contains the helpers for the plot-renderers package
  ; usefull for building your own renderers

  (require
   (lib "list.ss")
   (lib "math.ss")
   (lib "math.ss" "plot"))

  ; sample-size: number number number -> number
  (define (sample-size samples x-min x-max)
    (/ (- x-max x-min) (- samples 1)))

  ; scale-vectors : listof-posn number number -> listof-posn
  ; scales vectors, causing them to fit in their boxes
  (define (scale-vectors deltas x-sample-size y-sample-size)
    (let* ((x-max-value (apply max (map vector-x deltas)))
           (y-max-value (apply max (map vector-y deltas)))
           (x-div-const (/ x-max-value x-sample-size))
           (y-div-const (/ y-max-value y-sample-size)))
      (map (lambda (point) (vector (* (/ (vector-x point) x-div-const) 9/10) 
                                   (* (/ (vector-y point) y-div-const) 9/10))) deltas)))

  ; x-values : number number number -> listof-number
  (define (x-values samples x-min x-max)
    (let ((ss (sample-size samples x-min x-max)))
      (build-list samples (lambda (x) (+ x-min (* x ss))))))

  ; normalze-vector : posn number number -> posn
  (define (normalize-vector vec x-sample-size y-sample-size)
    (let* ((size (vector-magnitude vec)))
      (if (zero? size)
          (vector 0 0)
          (vector (* (/ (vector-x vec) size) x-sample-size 9/10)
                  (* (/ (vector-y vec) size) y-sample-size 9/10)))))

  ; normalize-vector : listof-posn number number -> listolf-posn
  (define (normalize-vectors deltas x-sample-size y-sample-size)
    (map (lambda (vec) (normalize-vector vec x-sample-size y-sample-size)) deltas))

  ; make-column : number listof-number -> listof-points
  (define (make-column x-val y-values)
    (map (lambda (y) (vector x-val y)) y-values))

  ; xy-list : number number number number number  -> listof-posn
  ; make a list of all the positions on the graph
  (define (xy-list samples x-min x-max y-min y-max)
    (let* ((x-vals (x-values samples x-min x-max))
           (y-vals (x-values samples y-min y-max)))
      (apply append (map (lambda (x) (make-column x y-vals)) x-vals))))

  ; zgrid : (number number -> number) listof-number listof-number -> listof-listof number
  (define (zgrid func x-vals y-vals samples)
    (map (lambda (x) (map (lambda (y) (func x y)) y-vals)) x-vals))

  (provide (all-defined))

  (require
   (lib "class.ss")
   (lib "etc.ss"))

  )
