(module flash mzscheme
  (require pict/private/pict
           mzlib/math
           mzlib/etc
           racket/draw
           mzlib/class)

  (provide filled-flash
           outline-flash)

  (define-syntax define-flash
    (syntax-rules ()
      [(_ id filled?)
       (define id
	 (opt-lambda (w h [points 10] [spike-fraction 0.25] [rotation 0])
	   (do-flash filled? w h points spike-fraction rotation)))]))

  (define-flash filled-flash #t)
  (define-flash outline-flash #f)

  (define no-brush
    (send the-brush-list find-or-create-brush "white" 'transparent))

  (define do-flash
    (lambda (filled? w h points spike-fraction rotation)
      (let ([p (new dc-path%)]
            [delta (/ pi points)]
            [in (- 1 spike-fraction)])
        (send p move-to 1 0)
        (let loop ([angle delta][points (sub1 points)])
          (send p line-to (* in (cos angle)) (* in (sin angle)))
          (unless (zero? points)
            (let ([angle (+ angle delta)])
              (send p line-to (cos angle) (sin angle))
              (loop (+ angle delta) (sub1 points)))))
        (send p close)
        (send p scale (/ w 2) (/ h 2))
        (unless (zero? rotation)
          (send p rotate rotation))        
        (let-values ([(bx by bw bh) (send p get-bounding-box)])
          (send p translate (- bx) (- by))
          (dc (lambda (dc x y)
		(let ([b (send dc get-brush)])
		  (if filled?
                      (send dc set-brush (send (send dc get-pen) get-color) 'solid)
                      (send dc set-brush no-brush))
		  (send dc draw-path p x y)
                  (send dc set-brush b)))
              bw bh))))))
