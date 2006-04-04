(require (lib "animation.ss" "frtime")
         (lib "mztake.ss" "mztake")
         (lib "useful-code.ss" "mztake")
         (as-is mzscheme assoc))

(define/bind (loc "random.ss" '(loop _)) x)

(define (assoc-inc l x)
  (let ([filtered (filter (lambda (y) (not (eq? x (first y)))) l)]
        [new-pair (let ([r (assoc x l)])
                    (if r `(,x ,(add1 (second r)))
                        `(,x 1)))])
    (cons new-pair filtered)))

(define histogram
  (accum-b ((changes x) . ==> . (lambda (x) (lambda (h) (assoc-inc h x))))
           empty))

(define x-scale 15)
(define y-scale 20)

(define (make-histogram-rectangle p)
  (let ([bin (first p)]
        [count (second p)])
    (make-rect (make-posn (* bin x-scale) 0)
               x-scale (* count y-scale)
               "blue")))

(define rectangles (map make-histogram-rectangle histogram))

(display-shapes rectangles)

(define largest-bin (apply max (cons 0 (map second histogram))))

(set-running! (< largest-bin 18))
