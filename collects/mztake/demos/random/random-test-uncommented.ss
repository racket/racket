(require (lib "graphics.ss" "graphics")         
         (lifted mzscheme
                 make-hash-table
                 hash-table-put!
                 hash-table-get))


(open-graphics)
(define window (open-viewport "Debugger" 600 500))
((draw-viewport window) (make-rgb 0.95 0.95 0.95))


(define-mztake-process p ("random.ss" [x-trace 4 6 bind 'x]))


(define largest-bin 0)
(define valcount (make-hash-table))


(hold (x-trace . -=> .(printf-b "largest count: ~a" largest-bin)))


(map-e (lambda (x)
         (let* ([new-cnt (add1 (hash-table-get valcount x (lambda () 0)))]
                [color (/ new-cnt (add1 largest-bin))])
           
           (when (= largest-bin 250)
             (kill p))
           
           (when (> new-cnt largest-bin) (set! largest-bin new-cnt))
           
           (hash-table-put! valcount x new-cnt)
           
           ((draw-solid-rectangle window) (make-posn (* x 6)  (- 500 (* 2 new-cnt)))
                                          6 10 ;; width height
                                          (make-rgb 0 (* 0.75 color) color))))
       x-trace)


(printf-b "count: ~a" (count-b x-trace))


(start/resume p)