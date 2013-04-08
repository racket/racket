#lang racket/base

(require "godel.rkt" 
         racket/gui/base
         racket/class
         racket/set
         racket/list
         math/base)
(module+ test (require rackunit))

(provide pick-a-maze
         draw-maze 
         build-walls
         decode-maze
         maze/s)

(define (decode-maze maze-w maze-h n)
  (define mazes (maze/s maze-w maze-h))
  (unless (and (exact-nonnegative-integer? n)
               (< n (spec-k mazes)))
    (raise-argument-error 'decode-maze 
                          (format "number less than ~a" (spec-k mazes))
                          n))
    (decode mazes n))

(define (memoize f)
  (define ht (make-hash))
  (λ args
    (hash-ref 
     ht args
     (λ ()
       (hash-set! ht args (apply f args))
       (hash-ref ht args)))))

(define maze/s 
  (memoize
   (λ (width height)
     (cond
       [(or (= 1 height) (= 1 width)) (unit/s #f)]
       [else
        (k*k-bind/s
         (wrap/s (fixed-length-list/s
                  (enum/s '(l t r b))
                  (wrap/s (nat-range/s (- height 1)) add1 sub1)
                  (wrap/s (nat-range/s (- width 1)) add1 sub1))
                 reverse reverse)
         (λ (ul-w/h-and-break)
           (define ul-w (list-ref ul-w/h-and-break 0))
           (define ul-h (list-ref ul-w/h-and-break 1))
           (define missing (list-ref ul-w/h-and-break 2))
           (define lr-w (- width ul-w))
           (define lr-h (- height ul-h))
           (fixed-length-list/s
            
            (case missing
              [(l)
               (fixed-length-list/s (unit/s #f)
                                    (nat-range/s ul-h)
                                    (nat-range/s lr-w)
                                    (nat-range/s lr-h))]
              [(t)
               (fixed-length-list/s (nat-range/s ul-w)
                                    (unit/s #f)
                                    (nat-range/s lr-w)
                                    (nat-range/s lr-h))]
              [(r)
               (fixed-length-list/s (nat-range/s ul-w)
                                    (nat-range/s ul-h)
                                    (unit/s #f)
                                    (nat-range/s lr-h))]
              [(b)
               (fixed-length-list/s (nat-range/s ul-w)
                                    (nat-range/s ul-h)
                                    (nat-range/s lr-w)
                                    (unit/s #f))])
            
            (maze/s ul-w ul-h)
            (maze/s lr-w ul-h)
            (maze/s ul-w lr-h)
            (maze/s lr-w lr-h))))]))))

(define (fixed-length-list/s . args)
  (let loop ([args args])
    (cond
      [(null? args) (unit/s '())]
      [else (cons/s (car args) (loop (cdr args)))])))

(define (pick-a-maze maze-w maze-h)
  (define maze-count (spec-k (maze/s maze-w maze-h)))
  (+ (if (zero? (random 2))
         (/ maze-count 2)
         0)
     (random-natural (/ maze-count 4))
     (random-natural (/ maze-count 4))))

(define (draw-maze dc dx dy w h edges maze-w maze-h 
                   #:next-edges [next-edges #f] 
                   #:solution [solution #f]
                   #:images [images '()])
  (define cell-size (min (/ w (+ maze-w 1/2)) (/ h (+ maze-h 1/2))))
  (define wall-pen-size (max 2 (ceiling (/ cell-size 6))))
  (define tot-maze-w (* cell-size maze-w))
  (define tot-maze-h (* cell-size maze-h))
  (define x-margin (/ (- w tot-maze-w) 2))
  (define y-margin (/ (- h tot-maze-h) 2))
  (define (mx->dcx mx) (+ (* mx cell-size) x-margin))
  (define (my->dcy my) (+ (* my cell-size) y-margin))
  
  #;
  (begin
    (define start-distances (find-distances (cons 0 0) edges))
    (define end-distances (find-distances (cons (- maze-w 1) (- maze-h 1)) edges))
    (for* ([x (in-range maze-w)]
           [y (in-range maze-h)])
      (define k (cons x y))
      (define ds (hash-ref start-distances k))
      (define de (hash-ref end-distances k))
      (define color (cond
                      [(= ds de) "white"]
                      [(< ds de) "Khaki"]
                      [(> ds de) "Lavender"]))
      (define dx (mx->dcx x))
      (define dy (my->dcy y))
      (send dc set-pen color 1 'transparent)
      (send dc set-brush color 'solid)
      (send dc draw-rectangle dx dy cell-size cell-size)))
  
  (send dc set-pen "lightblue" 1 'solid)
  (for ([x (in-range 1 maze-w)])
    (define e (mx->dcx x))
    (send dc draw-line (+ dx e) (+ dy y-margin) (+ dx e) (+ dy y-margin tot-maze-h)))
  (for ([y (in-range 1 maze-h)])
    (define e (my->dcy y))
    (send dc draw-line (+ dx x-margin) (+ dy e) (+ dx x-margin tot-maze-w) (+ dy e)))
  
  (send dc set-pen "black" wall-pen-size 'solid)
  (send dc draw-line
        (+ dx x-margin) (+ dy y-margin cell-size)
        (+ dx x-margin) (+ dy y-margin tot-maze-h))
  (send dc draw-line 
        (+ dx x-margin cell-size) (+ dy y-margin)
        (+ dx x-margin tot-maze-w) (+ dy y-margin))
  (send dc draw-line  
        (+ dx x-margin) (+ dy y-margin tot-maze-h)
        (+ dx x-margin tot-maze-w (- cell-size)) (+ dy y-margin tot-maze-h))
  (send dc draw-line
        (+ dx x-margin tot-maze-w) (+ dy y-margin)
        (+ dx x-margin tot-maze-w) (+ dy y-margin tot-maze-h (- cell-size)))
  
  (define (connect x1 y1 x2 y2)
    (unless (and (= x1 x2)
                 (= y1 y2))
      (send dc draw-line 
            (+ dx (mx->dcx x1))
            (+ dy (my->dcy y1))
            (+ dx (mx->dcx x2))
            (+ dy (my->dcy y2)))))

  (for ([(from neighbors) (in-hash edges)])
    (define from-x (car from))
    (define from-y (cdr from))
    (define (try to-x to-y)
      (when (<= 0 to-x (- maze-w 1))
        (when (<= 0 to-y (- maze-h 1))
          (define k (cons to-x to-y))
          (unless (set-member? neighbors k)
            (send dc set-pen "black"
                  wall-pen-size
                  'solid)
            (cond
              [(= from-x to-x)
               (connect from-x
                        to-y
                        (+ from-x 1)
                        to-y)]
              [(= from-y to-y)
               (connect to-x
                        to-y
                        to-x
                        (+ to-y 1))])))))
    (try (+ from-x 1) from-y)
    (try from-x (+ from-y 1)))
  
  (when solution
    (send dc set-pen "red" wall-pen-size 'solid)
    (for ([solution1 (in-list solution)]
          [solution2 (in-list (cdr solution))])
      (connect (+ (car solution1) 1/2)
               (+ (cdr solution1) 1/2)
               (+ (car solution2) 1/2)
               (+ (cdr solution2) 1/2))))
  
  (for ([image (in-list images)])
    (define-values (icons x y) (apply values image))
    (define icon (or (for/or ([icon (in-list icons)])
                       (and (<= (send icon get-width) cell-size)
                            (<= (send icon get-height) cell-size)
                            icon))
                     (last icons)))
    (send dc draw-bitmap 
          icon
          (+ dx (mx->dcx x) (/ (- cell-size (send icon get-width)) 2))
          (+ dy (my->dcy y) (/ (- cell-size (send icon get-height)) 2)))))

(define (find-solution edges maze-w maze-h)
  (define start (cons 0 0))
  (define end (cons (- maze-w 1) (- maze-h 1)))
  (define visited (make-hash))
  (let loop ([node start])
    (cond
      [(equal? node end) (list end)]
      [(hash-ref visited node #f) #f]
      [else
       (hash-set! visited node #t)
       (define neighbor-ans
         (for/or ([neighbor (in-set (hash-ref edges node))])
           (loop neighbor)))
       (and neighbor-ans
            (cons node neighbor-ans))])))

(define (find-distances end edges)
  (define distances (make-hash))
  (let loop ([node end]
             [distance 0])
    (cond
      [(hash-ref distances node #f) (void)]
      [else
       (hash-set! distances node distance)
       (for ([neighbor (in-set (hash-ref edges node))])
         (loop neighbor
               (+ distance 1)))]))
  distances)

(define (build-walls maze-spec maze-w maze-h)
  (define edges (make-hash))
  
  (define (add-edge! x1 y1 x2 y2)
    (add->edge! x1 y1 x2 y2)
    (add->edge! x2 y2 x1 y1))
  (define (add->edge! x1 y1 x2 y2)
    (define k (cons x1 y1))
    (hash-set! edges k (set-add (hash-ref edges k (set)) (cons x2 y2))))
  
  
  (define (remove-edge! x1 y1 x2 y2)
    (remove->edge! x1 y1 x2 y2)
    (remove->edge! x2 y2 x1 y1))
  (define (remove->edge! x1 y1 x2 y2)
    (define k (cons x1 y1))
    (define new-edges (set-remove (hash-ref edges k) (cons x2 y2)))
    (hash-set! edges k new-edges))
  
  (for* ([x (in-range 0 maze-w)]
         [y (in-range 0 maze-h)])
    (unless (zero? x)
      (add-edge! x y (- x 1) y))
    (unless (zero? y)
      (add-edge! x y x (- y 1))))
  
  ;; draws a line between (x1,y1) and (x2,y2)
  ;; which removes some edges
  (define (connect x1 y1 x2 y2)
    (let loop ([x1 (min x1 x2)]
               [y1 (min y1 y2)]
               [x2 (max x1 x2)]
               [y2 (max y1 y2)])
      (cond
        [(and (= x1 x2) (= y1 y2))
         (void)]
        [(= x1 x2)
         (remove-edge! (- x1 1) y1 x1 y1)
         (loop x1 (+ y1 1) x1 y2)]
        [(= y1 y2)
         (remove-edge! x1 (- y1 1) x1 y1)
         (loop (+ x1 1) y1 x2 y2)]
        [else 
         (error 'connect "ack ~s => ~s\n" (cons x1 y1) (cons x2 y2))])))
  
  (define (draw-horizontal-line line-break width x-start y-start)
    (cond
      [line-break
       (connect x-start y-start (+ x-start line-break) y-start)
       (connect (+ x-start line-break 1) y-start (+ x-start width) y-start)]
      [else
       (connect x-start y-start (+ x-start width) y-start)]))
  
  (define (draw-vertical-line line-break height x-start y-start)
    (cond
      [line-break
       (connect x-start y-start x-start (+ y-start line-break))
       (connect x-start (+ y-start line-break 1) x-start (+ y-start height))]
      [else
       (connect x-start y-start x-start (+ y-start height))]))
  
  (let loop ([maze maze-spec]
             [x 0]
             [y 0]
             [maze-w maze-w]
             [maze-h maze-h])
    (when maze
      (define sub-maze-info (list-ref maze 0))
      (define ul-w (list-ref sub-maze-info 0))
      (define ul-h (list-ref sub-maze-info 1))
      (define lr-w (- maze-w ul-w))
      (define lr-h (- maze-h ul-h))
      
      (define line-break-info (list-ref maze 1))
      (define-values (lb-left lb-top lb-right lb-bottom) (apply values line-break-info))
      (draw-horizontal-line lb-left   ul-w x          (+ y ul-h))
      (draw-vertical-line   lb-top    ul-h (+ x ul-w) y)
      (draw-horizontal-line lb-right  lr-w (+ x ul-w) (+ y ul-h))
      (draw-vertical-line   lb-bottom lr-h (+ x ul-w) (+ y ul-h))
      (define ul-submaze (list-ref maze 2))
      (define ur-submaze (list-ref maze 3))
      (define ll-submaze (list-ref maze 4))
      (define lr-submaze (list-ref maze 5))
      
      (loop ul-submaze x          y          ul-w ul-h)
      (loop ur-submaze (+ x ul-w) y          lr-w ul-h)
      (loop ll-submaze x          (+ y ul-h) ul-w lr-h)
      (loop lr-submaze (+ x ul-w) (+ y ul-h) lr-w lr-h)))  
  edges)

(module+ test
  
  (check-equal?
   (build-walls '((1 1 t) (0 #f 0 0) #f #f #f #f) 2 2)
   (make-hash 
    (list (cons '(0 . 0) (set '(0 . 1)))
          (cons '(0 . 1) (set '(0 . 0) '(1 . 1)))
          (cons '(1 . 0) (set '(1 . 1)))
          (cons '(1 . 1) (set '(0 . 1) '(1 . 0))))))
  
  (check-equal?
   (build-walls 
    '((1 2 b) (0 1 1 #f) #f ((1 1 l) (#f 0 0 0) #f #f #f #f) #f #f)
    3
    3)
   (make-hash 
    (list (cons '(0 . 0) (set '(0 . 1)))
          (cons '(0 . 1) (set '(0 . 0) '(1 . 1) '(0 . 2)))
          (cons '(0 . 2) (set '(0 . 1)))
          (cons '(1 . 0) (set '(2 . 0)))
          (cons '(1 . 1) (set '(0 . 1) '(2 . 1)))
          (cons '(1 . 2) (set '(2 . 2)))
          (cons '(2 . 0) (set '(1 . 0) '(2 . 1)))
          (cons '(2 . 1) (set '(2 . 0) '(2 . 2) '(1 . 1)))
          (cons '(2 . 2) (set '(2 . 1) '(1 . 2))))))
  
  (check-equal?
   (build-walls '((2 2 l) (#f 0 0 0) ((1 1 l) (#f 0 0 0) #f #f #f #f) #f #f #f)
                3 3)
   (make-hash 
    (list (cons '(0 . 0) (set '(1 . 0)))
          (cons '(0 . 1) (set '(1 . 1)))
          (cons '(0 . 2) (set '(1 . 2)))
          (cons '(1 . 0) (set '(0 . 0) '(1 . 1) '(2 . 0)))
          (cons '(1 . 1) (set '(0 . 1) '(1 . 0)))
          (cons '(1 . 2) (set '(0 . 2) '(2 . 2)))
          (cons '(2 . 0) (set '(1 . 0) '(2 . 1)))
          (cons '(2 . 1) (set '(2 . 0) '(2 . 2)))
          (cons '(2 . 2) (set '(2 . 1) '(1 . 2)))))))

(define (show-mazes)
  
  ;(define maze-w 34) (define maze-h 44)
  (define maze-w 12) (define maze-h 12)
  ;(define maze-w 2) (define maze-h 3)
  ;(define maze-w 16) (define maze-h 16)
  ;(define maze-w 8) (define maze-h 8)
  
  (define mazes (time (maze/s maze-w maze-h)))
  (define maze-count (spec-k mazes))
  (printf "~a mazes\n" maze-count)
  
  (define slider-max-value (min maze-count 10000))
  (define max-starting-point (- maze-count slider-max-value))
  (define starting-point (pick-a-maze maze-w maze-h))
  (define f (new frame% [label ""] [width 400] [height 400]))
  
  (define current-solution #f)
  (define current-edges #f)
  (define next-edges #f)
  (define which 0)
  
  (define c (new canvas% [parent f] 
                 [paint-callback
                  (λ (c dc) 
                    (send dc set-smoothing 'smoothed)
                    (define-values (w h) (send c get-client-size))
                    (draw-maze dc 0 0 w h current-edges maze-w maze-h 
                               #:next-edges next-edges 
                               #:solution current-solution))]))
  (define bp (new horizontal-panel% [parent f] [stretchable-height #f]))
  (define (move-to n)
    (set! which (modulo n maze-count))
    (set! current-edges (build-walls (decode mazes which) maze-w maze-h))
    (set! current-solution (find-solution current-edges maze-w maze-h))
    (set! next-edges (build-walls (decode mazes (modulo (+ which 1) maze-count))
                                  maze-w maze-h))
    (send slider set-value (- which starting-point))
    (send c refresh))
  
  (define slider 
    (new slider%
         [label #f]
         [min-value 0]
         [max-value slider-max-value]
         [parent bp]
         [callback
          (λ args
            (move-to (+ starting-point (send slider get-value))))]))
  (define tf (new text-field% 
                  [label "Starting point"]
                  [parent f]
                  [stretchable-width #t]
                  [init-value ""]
                  [callback
                   (λ args
                     (define n (string->number (send tf get-value)))
                     (define n-ok? (and n (<= n max-starting-point)))
                     (send tf set-field-background
                           (send the-color-database find-color
                                 (if n-ok? "white" "pink")))
                     (when n-ok?
                       (set! starting-point n)
                       (move-to starting-point)))]))
  
  (send tf set-value (format "~a" starting-point))
  (define (mk-b lab adj)
    (new button% 
         [parent bp]
         [label lab]
         [callback
          (λ args
            (move-to (adj which)))]))
  (mk-b "Next" add1)
  (mk-b "Previous" sub1)
  (new button%
       [parent bp]
       [label "Random"]
       [callback
        (λ args
          (set! starting-point (pick-a-maze maze-w maze-h))
          (send tf set-value (format "~a" starting-point))
          (move-to starting-point))])
  
  (define run? #f)
  (define timer
    (new timer%
         [notify-callback
          (λ () (move-to (+ which 1)))]))
  
  (define run/stop-button
    (new button% 
         [label "Run"]
         [parent f]
         [stretchable-width #t]
         [callback
          (λ args
            (send run/stop-button set-label 
                  (if run? "Run" "Stop"))
            (set! run? (not run?))
            (if run?
                (send timer start 100)
                (send timer stop)))]))
  (move-to starting-point)
  (send f show #t))

(module+ main (show-mazes))


