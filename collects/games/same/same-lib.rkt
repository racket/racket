#lang racket/base
(require racket/class
         racket/draw)

(provide make-a-move
         
         draw-board
         update-pen/draw-blob
         update-dc-scale
         
         colors
         board-ref
         cell-w
         cell-h
         find-same-colors)

;; these are the sizes that the on-paint callback draws at;
;; a scaling factor is applied to make the board fit the window
(define cell-w 11)
(define cell-h 11)
(define pen-size 10)

(define colors (map (lambda (x) (make-object color% x))
                    (list "blue" "red" "brown" "forestgreen" "purple")))
(define pale-colors 
  (for/list ([x (in-list colors)])
    (define (paleize x) (- 255 (floor (* (- 255 x) 2/3))))
    (make-object color%
      (paleize (send x red))
      (paleize (send x green))
      (paleize (send x blue)))))

(define (draw-board dc board-width board-height board cw ch
                    mouse-current-x mouse-current-y mouse-clicked-x mouse-clicked-y)
  (send dc erase) 
  (send dc set-smoothing 'smoothed)
  (update-dc-scale dc cw ch board-width board-height)
  (define painted (make-hash))
  (for* ([i (in-range 0 board-width)]
         [j (in-range 0 board-height)])
    (unless (hash-ref painted (xy->key board-width i j) #f)
      (define color (vector-ref (board-ref board i j) 0))
      (when color
        (define blob (find-same-colors board board-width board-height i j))
        (for ([x (in-list blob)])
          (hash-set! painted (xy->key board-width (blob-sel-x x) (blob-sel-y x)) #t))
        (update-pen/draw-blob
         blob dc color
         mouse-current-x mouse-current-y mouse-clicked-x mouse-clicked-y)))))

(define (update-dc-scale dc cw ch board-width board-height)
  (send dc set-scale 
        (/ cw (* board-width cell-w)) 
        (/ ch (* board-height cell-h))))

(define (update-pen/draw-blob
         blob dc color
         mouse-current-x mouse-current-y mouse-clicked-x mouse-clicked-y)
  (define mouse-over? #f)
  (define mouse-clicked-over? #f)
  (define multiple-cells? (not (or (null? blob) (null? (cdr blob)))))
  
  (when (or (number? mouse-current-x)
            (number? mouse-clicked-x))
    (for ([obj (in-list blob)])
      (define x (blob-sel-x obj))
      (define y (blob-sel-y obj))
      (when (and (equal? x mouse-current-x)
                 (equal? y mouse-current-y))
        (set! mouse-over? #t))
      (when (and (equal? x mouse-clicked-x)
                 (equal? y mouse-clicked-y))
        (set! mouse-clicked-over? #t))))
  
  (cond
    [mouse-clicked-x ;; has the mouse been clicked in a clickable place?
     (cond 
       [(and mouse-over? mouse-clicked-over? multiple-cells?)
        (send dc set-pen (list-ref pale-colors color) (* pen-size 2/3) 'solid)
        (draw-blob dc blob)]
       [else
        (send dc set-pen
              (list-ref colors color)
              pen-size
              'solid)
        (draw-blob dc blob)])]
    [else
     (cond
       [mouse-over?
        (send dc set-pen (list-ref pale-colors color) pen-size 'solid)
        (draw-blob dc blob)]
       [else
        (send dc set-pen (list-ref colors color) pen-size 'solid)
        (draw-blob dc blob)])]))

(define (draw-blob dc blob)
  (define (connect x1 y1 x2 y2)
    (send dc draw-line 
          (+ (/ cell-w 2) (* x1 cell-w))
          (+ (/ cell-h 2) (* y1 cell-h))
          (+ (/ cell-w 2) (* x2 cell-w))
          (+ (/ cell-h 2) (* y2 cell-h))))
  (cond
    [(null? (cdr blob))
     (define pt (car blob))
     (connect (blob-sel-x pt) (blob-sel-y pt) (blob-sel-x pt) (blob-sel-y pt))]
    [else
     (for* ([b1 (in-list blob)]
            [b2 (in-list blob)])
       (when (= (+ (abs (- (blob-sel-x b1) (blob-sel-x b2)))
                   (abs (- (blob-sel-y b1) (blob-sel-y b2))))
                1)
         (connect (blob-sel-x b1) (blob-sel-y b1) (blob-sel-x b2) (blob-sel-y b2))))]))

(define (xy->key board-width x y) (+ (* board-width y) x))

(define (make-same-bitmap pth)
  (define bw 32)
  (define bh 32)
  (define bitmap (make-bitmap bw bh))
  (define bdc (make-object bitmap-dc% bitmap))
  (define board-width 3)
  (define board-height 3)
  (define board 
    (vector (vector (vector 0 #f) (vector 1 #f) (vector 4 #f))
            (vector (vector 0 #f) (vector 1 #f) (vector 1 #f))
            (vector (vector 3 #f) (vector 3 #f) (vector 2 #f))))
  (draw-board bdc board-width board-height board bw bh
              #f #f #f #f)
  (send bdc set-bitmap #f)
  (send bitmap save-file pth 'png))
  
; (make-same-bitmap "same.png")

;; make-a-move : num num board num num -> num or #f
;; mutates 'board' to reflect removing the blob at (i,j)
;; result is the size of the removed blob, or #f if nothing got removed
(define (make-a-move i j board board-width board-height)
  (let ([same-colors (find-same-colors board board-width board-height i j)])
    (cond
      [(< (length same-colors) 2)
       #f]
      [else
       
       ;; slide down empty pieces
       (let ([is null])
         (for-each
          (lambda (p)
            (let ([i (blob-sel-x p)]
                  [j (blob-sel-y p)])
              (unless (member i is)
                (set! is (cons i is)))
              (let loop ([x j])
                (cond
                  [(<= 1 x)
                   (let ([next (board-ref board i (- x 1))]
                         [this (board-ref board i x)])
                     (vector-set! this 0 (vector-ref next 0))
                     (loop (- x 1)))]
                  [else
                   (vector-set! (board-ref board i x) 0 #f)]))))
          (sort same-colors
                (lambda (x y) (<= (blob-sel-y x) (blob-sel-y y)))))
         
         ;; slide empty over empty rows
         (set! is (sort is >))
         (let ([empty-is 
                (filter (lambda (i)
                          (not (vector-ref 
                                (board-ref board i (- board-height 1))
                                0)))
                        is)])
           (let ([is (if (null? empty-is)
                         is
                         (filter (lambda (x) (< x (car empty-is)))
                                 is))])
             (for-each (lambda (empty-i)
                         (let loop ([i empty-i])
                           (cond
                             [(<= i (- board-width 2))
                              (vector-set! board i (vector-ref board (+ i 1)))
                              (loop (+ i 1))]
                             [(= i (- board-width 1))
                              (vector-set! 
                               board
                               i
                               (build-vector board-height
                                             (λ (i) (vector #f #f))))])))
                       empty-is))))
       
       (length same-colors)])))

(define (blob-sel-x b) (vector-ref b 1))
(define (blob-sel-y b) (vector-ref b 2))
(define (board-ref b x y) (vector-ref (vector-ref b x) y))

(define (find-same-colors board board-width board-height i j)
  (let* ([index (vector-ref (board-ref board i j) 0)]
         [ans
          (let loop ([i i]
                     [j j]
                     [ps null])
            (cond
              [(not (and (<= 0 i) (< i board-width)
                         (<= 0 j) (< j board-height)))
               ps]
              [else
               (let ([v (board-ref board i j)])
                 (cond 
                   [(vector-ref v 1) ps]
                   [(not (vector-ref v 0)) ps]
                   [(= index (vector-ref v 0))
                    (vector-set! v 1 #t)
                    (loop (+ i 1)
                          j
                          (loop (- i 1)
                                j
                                (loop i
                                      (- j 1)
                                      (loop i
                                            (+ j 1)
                                            (cons (vector v i j)
                                                  ps)))))]
                   [else ps]))]))])
    (for-each (lambda (p) (vector-set! (vector-ref p 0) 1 #f)) ans)
    ans))
