(module make-bitmap racket
  (require "board.rkt"
           racket/gui
           racket/class)

  (provide show-board
           board-canvas%
           draw-board
           for-each-piece/position
           find-main-coordinates
           find-home-row-coordinates
           get-cell-size
           get-piece-size
           
           (struct-out home-row-c)
           (struct-out main-c)
           (struct-out start-c)
           (struct-out home-c))
  
  ;; a coordinate is either
  ;;   - (make-home-row-c number color)
  ;;   - (make-main-c number)
  ;;   - (make-start-c color)
  ;;   - (make-home-c color)
  ;; inspectors are to allow comparison with equal?
  (define-struct home-row-c (count color) #:inspector (make-inspector))
  (define-struct main-c (count) #:inspector (make-inspector))
  (define-struct start-c (color) #:inspector (make-inspector))
  (define-struct home-c (color) #:inspector (make-inspector))
  
  (define (get-cell-size horizontal? w h)
    (if horizontal?
        (values (* w 1/9) (* h 1/24))
        (values (* w 1/24) (* h 1/9))))
  
  (define colors
    '((green . "green")
      (red . "red")
      (yellow . "gold")
      (blue . "blue")
      (black . "black")
      (safety . "purple")
      (track-background . "light blue")))

  (define circle-gap 1/20)
  
  (define (set-color dc color)
    (send dc set-pen (send the-pen-list find-or-create-pen  (cdr (assq color colors)) 1 'solid))
    (send dc set-brush (send the-brush-list find-or-create-brush (cdr (assq color colors)) 'solid)))
  
  (define draw-board
    (lambda (board dc w h dx dy draw-pieces?)
      (set-color dc 'track-background)
      (send dc draw-rectangle (+ dx 0) (+ dy (* h 1/3)) w (* h 1/3))
      (send dc draw-rectangle (+ dx (* w 1/3)) (+ dy 0) (* w 1/3) h)
      
      (set-color dc 'blue)
      (send dc draw-ellipse
            (+ dx (* w circle-gap)) (+ dy (* h circle-gap 1/2))
            (- (/ w 3) (* w circle-gap)) (- (/ h 3) (* h circle-gap)))
      (send dc draw-rectangle (+ dx (* w (- 1/2 1/18))) (+ dy 0) (* w 1/9) (/ h 3))
      (send dc draw-polygon 
            (list (make-object point% (* w 1/3) (* h 1/3))
                  (make-object point% (* w 2/3) (* h 1/3))
                  (make-object point% (* w 1/2) (* h 1/2)))
            dx
            dy)
      
      (set-color dc 'red)
      (send dc draw-ellipse 
            (+ dx (* w 2/3) (* h circle-gap 1/2)) 
            (+ dy (* h circle-gap))
            (- (/ w 3) (* w circle-gap)) 
            (- (/ h 3) (* h circle-gap)))
      (send dc draw-rectangle (+ dx (* w 2/3)) (+ dy (* h (- 1/2 1/18))) (/ w 3) (* h 1/9))
      (send dc draw-polygon 
            (list (make-object point% (* w 2/3) (* h 1/3))
                  (make-object point% (* w 2/3) (* h 2/3))
                  (make-object point% (* w 1/2) (* h 1/2)))
            dx
            dy)
      
      (set-color dc 'yellow)
      (send dc draw-ellipse 
            (+ dx (* w circle-gap 1/2)) (+ dy (* h 2/3))
            (- (/ w 3) (* w circle-gap)) (- (/ h 3) (* h circle-gap)))
      (send dc draw-rectangle (+ dx 0) (+ dy (* h (- 1/2 1/18))) (/ w 3) (* h 1/9))
      (send dc draw-polygon 
            (list (make-object point% (* w 1/3) (* h 1/3))
                  (make-object point% (* w 1/3) (* h 2/3))
                  (make-object point% (* w 1/2) (* h 1/2)))
            dx
            dy)
      
      (set-color dc 'green)
      (send dc draw-ellipse 
            (+ dx (* w 2/3)) (+ dy (* h 2/3) (* h circle-gap 1/2))
            (- (/ w 3) (* w circle-gap)) (- (/ h 3) (* h circle-gap)))
      (send dc draw-rectangle (+ dx (* w (- 1/2 1/18))) (+ dy (* h 2/3)) (* w 1/9) (/ h 3))
      (send dc draw-polygon 
            (list (make-object point% (* w 1/3) (* h 2/3))
                  (make-object point% (* w 2/3) (* h 2/3))
                  (make-object point% (* w 1/2) (* h 1/2)))
            dx
            dy)
      
      (set-color dc 'safety)
      
      (send dc draw-rectangle 
            (+ dx (* w 4/9))
            (+ dy 0)
            (* w 1/9)
            (* h 1/3 1/8))
      (send dc draw-rectangle 
            (+ dx (* w 4/9))
            (+ dy (* h (- 1 (* 1/3 1/8))))
            (* w 1/9)
            (* h 1/3 1/8))
      (send dc draw-rectangle 
            (+ dx (* w 5/9))
            (+ dy (* h 1/3 4/8))
            (* w 1/9)
            (* h 1/3 1/8))
      (send dc draw-rectangle 
            (+ dx (* w 3/9))
            (+ dy (* h 19/24))
            (* w 1/9)
            (* h 1/3 1/8))
      
      (send dc draw-rectangle 
            (+ dx (* w (- 1 (* 1/3 1/8))))
            (+ dy (* h 4/9))
            (* w 1/3 1/8)
            (* h 1/9))
      (send dc draw-rectangle 
            (+ dx 0)
            (+ dy (* h 4/9))
            (* w 1/3 1/8)
            (* h 1/9))
      (send dc draw-rectangle 
            (+ dx (* w 4/24))
            (+ dy (* h 1/3))
            (* w 1/3 1/8)
            (* h 1/3 1/3))
      (send dc draw-rectangle 
            (+ dx (* w 19/24))
            (+ dy (* h 5/9))
            (* w 1/3 1/8)
            (* h 1/3 1/3))
      
      ;; blue entry
      (send dc draw-rectangle 
            (+ dx (* w 3/9))
            (+ dy (* h 1/3 4/8))
            (* w 1/9)
            (* h 1/3 1/8))
      
      ;; green entry
      (send dc draw-rectangle 
            (+ dx (* w 5/9))
            (+ dy (* h 19/24))
            (* w 1/9)
            (* h 1/24))
      
      ;; yellow entry
      (send dc draw-rectangle 
            (+ dx (* w 4/24))
            (+ dy (* h 5/9))
            (* w 1/24)
            (* h 1/9))
      
      ;; red entry
      (send dc draw-rectangle 
            (+ dx (* w 19/24))
            (+ dy (* h 1/3))
            (* w 1/24)
            (* h 1/9))
      
#|
      (set-color dc 'blue)
      (send dc draw-polygon
            (list (make-object point% (* w 3/9) (* h 4/24))
                  (make-object point% (* w 2/9) (* h 5/24))
                  (make-object point% (* w 4/9) (* h 5/24)))
            dx
            dy)
      
      (set-color dc 'green)
      (send dc draw-polygon 
            (list (make-object point% (* w 5/9) (* h 19/24))
                  (make-object point% (* w 6/9) (* h 20/24))
                  (make-object point% (* w 7/9) (* h 19/24)))
            dx
            dy)
      
      
      (set-color dc 'yellow)
      (send dc draw-polygon 
            (list (make-object point% (* w 4/24) (* h 6/9))
                  (make-object point% (* w 5/24) (* h 5/9))
                  (make-object point% (* w 5/24) (* h 7/9)))
            dx
            dy)
      
      
      (set-color dc 'red)
      (send dc draw-polygon
            (list (make-object point% (* w 20/24) (* h 3/9))
                  (make-object point% (* w 19/24) (* h 4/9))
                  (make-object point% (* w 19/24) (* h 2/9)))
            dx
            dy)
|#      
      (set-color dc 'black)
      
      #;
      (let loop ([i 7])
        (unless (zero? i)
          (send dc draw-line 
                (+ dx (* w 1/3))
                (+ dy (+ (* h 2/3) (* (* h 1/3) (/ i 8))))
                (+ dx (* w 2/3))
                (+ dy (+ (* h 2/3) (* (* h 1/3) (/ i 8)))))
          (send dc draw-line 
                (+ dx (* w 1/3))
                (+ dy (* (* h 1/3) (/ i 8)))
                (+ dx (* w 2/3))
                (+ dy (* (* h 1/3) (/ i 8))))
          (send dc draw-line 
                (+ dx (+ (* w 2/3) (* (* w 1/3) (/ i 8))))
                (+ dy (* h 1/3))
                (+ dx (+ (* w 2/3) (* (* w 1/3) (/ i 8))))
                (+ dy (* h 2/3)))
          (send dc draw-line 
                (+ dx (* (* w 1/3) (/ i 8)))
                (+ dy (* h 1/3))
                (+ dx (* (* w 1/3) (/ i 8)))
                (+ dy (* h 2/3)))
          (loop (- i 1))))
      
      (when draw-pieces?
        (draw-pieces board dc w h dx dy))))
  
  ;; piece : color left top coordinate -> void
  (define (for-each-piece/position board w h piece)
    (void)
    #;
(let* ([piece-size (get-piece-size w h)]
           [call-out
            (lambda (ent x y horizontal? coordinate)
              (let* ([pawn (car ent)])
                (cond
                  [(null? (cdr ent))
                   (piece pawn (- x (/ piece-size 2)) (- y (/ piece-size 2)) coordinate)]
                  [else 
                   (let ([pawn2 (cadr ent)])
                     (cond
                       [horizontal?
                        (piece pawn (- x piece-size) (- y (/ piece-size 2)) coordinate)
                        (piece pawn2 x (- y (/ piece-size 2)) coordinate)]
                       [else
                        (piece pawn (- x (/ piece-size 2)) (- y piece-size) coordinate)
                        (piece pawn2 (- x (/ piece-size 2)) y coordinate)]))])))])
      
      ;; main board
      (let loop ([i board-main-size])
        (unless (zero? i)
          (let ([ent (board-main-i board (- i 1))])
            (unless (null? ent)
              (let-values ([(x y horizontal?) (find-main-coordinates (- i 1) w h)])
                (call-out ent x y horizontal? (make-main-c (- i 1)))))
            (loop (- i 1)))))
      
      ;; home row
      (let ([handle-home-row
             (lambda (color)
               (let loop ([i board-home-row-size])
                 (unless (zero? i)
                   (let ([ent (board-home-row-i board color (- i 1))])
                     (unless (null? ent)
                       (let-values ([(x y horizontal?) (find-home-row-coordinates color (- i 1) w h)])
                         (call-out ent x y horizontal? (make-home-row-c (- i 1) color)))))
                   (loop (- i 1)))))])
        (handle-home-row 'red)
        (handle-home-row 'green)
        (handle-home-row 'blue)
        (handle-home-row 'yellow))
      
      ;; home and start
      (let ([handle-home/start
             (lambda (color select coordinates coord horiz?)
               (let* ([pawns (filter (lambda (x) (eq? (pawn-color x) color)) (select board))]
                      [num (length pawns)])
                 (let-values ([(mx my) (apply values (cdr (assoc color (coordinates w h piece-size))))])
                   (let ([do (lambda (pawn fx fy)
                               (piece pawn
                                      (+ mx (* piece-size fx))
                                      (+ my (* piece-size fy))
                                      coord))])
                     (cond
                       [(= num 4)
                        (do (list-ref pawns 3) 0 0)
                        (do (list-ref pawns 2) -1 0)
                        (do (list-ref pawns 1) 0 -1)
                        (do (list-ref pawns 0) -1 -1)]
                       [(and horiz? (= num 3))
                        (do (list-ref pawns 2) 0 -1/2)
                        (do (list-ref pawns 1) -1 0)
                        (do (list-ref pawns 0) -1 -1)]
                       [(= num 3)
                        (do (list-ref pawns 2) -1/2 0)
                        (do (list-ref pawns 1) 0 -1)
                        (do (list-ref pawns 0) -1 -1)]
                       [(and horiz? (= num 2))
                        (do (list-ref pawns 1) -1/2 -1)
                        (do (list-ref pawns 0) -1/2 0)]
                       [(= num 2)
                        (do (list-ref pawns 1) -1 -1/2)
                        (do (list-ref pawns 0) 0 -1/2)]
                       [(= num 1)
                        (do (list-ref pawns 0) -1/2 -1/2)])))))])
        (handle-home/start 'red board-home at-home-coordinates (make-home-c 'red) #t)
        (handle-home/start 'green board-home at-home-coordinates (make-home-c 'green) #f)
        (handle-home/start 'blue board-home at-home-coordinates (make-home-c 'blue) #f)
        (handle-home/start 'yellow board-home at-home-coordinates (make-home-c 'yellow) #t)
        (handle-home/start 'red board-start at-start-coordinates (make-start-c 'red) #t)
        (handle-home/start 'green board-start at-start-coordinates (make-start-c 'green) #t)
        (handle-home/start 'blue board-start at-start-coordinates (make-start-c 'blue) #t)
        (handle-home/start 'yellow board-start at-start-coordinates (make-start-c 'yellow) #t))))
  
  (define (draw-pieces board dc w h dx dy)
    (for-each-piece/position 
     board w h
     (lambda (pawn x y coord)
       (let ([font (get-number-font (pawn-color pawn) w h)]
             [str (number->string (pawn-id pawn))]
             [old-font (send dc get-font)]
             [old-fore (send dc get-text-foreground)]
             [size (get-piece-size w h)])
         (send dc draw-ellipse (+ dx x) (+ dy y) size size)
         (send dc set-font font)
         (send dc set-text-foreground (get-number-color (pawn-color pawn)))
         (send dc set-font old-font)))))
  
  (define home-row-coordinates
    (list (list 'red
                #f
                (vector (cons (+ 22/24 1/48) 1/2)
                        (cons (+ 21/24 1/48) 1/2)
                        (cons (+ 20/24 1/48) 1/2)
                        (cons (+ 19/24 1/48) 1/2)
                        (cons (+ 18/24 1/48) 1/2)
                        (cons (+ 17/24 1/48) 1/2)
                        (cons (+ 16/24 1/48) 1/2)))
          (list 'yellow
                #f
                (vector (cons (+ 1/24 1/48) 1/2)
                        (cons (+ 2/24 1/48) 1/2)
                        (cons (+ 3/24 1/48) 1/2)
                        (cons (+ 4/24 1/48) 1/2)
                        (cons (+ 5/24 1/48) 1/2)
                        (cons (+ 6/24 1/48) 1/2)
                        (cons (+ 7/24 1/48) 1/2)))
          (list 'blue
                #t
                (vector (cons 1/2 (+ 1/24 1/48))
                        (cons 1/2 (+ 2/24 1/48))
                        (cons 1/2 (+ 3/24 1/48))
                        (cons 1/2 (+ 4/24 1/48))
                        (cons 1/2 (+ 5/24 1/48))
                        (cons 1/2 (+ 6/24 1/48))
                        (cons 1/2 (+ 7/24 1/48))))
          (list 'green
                #t
                (vector (cons 1/2 (+ 22/24 1/48))
                        (cons 1/2 (+ 21/24 1/48))
                        (cons 1/2 (+ 20/24 1/48))
                        (cons 1/2 (+ 19/24 1/48))
                        (cons 1/2 (+ 18/24 1/48))
                        (cons 1/2 (+ 17/24 1/48))
                        (cons 1/2 (+ 16/24 1/48))))))

  (define (find-home-row-coordinates color index w h)
    (let ([ent (assq color home-row-coordinates)])
      (if ent
          (let ([v (caddr ent)])
            (if (< index (vector-length v))
                (let ([crds (vector-ref v index)])
                  (values (* w (car crds)) (* h (cdr crds)) (cadr ent)))
                (values 0 0 #f)))
          (values 0 0 #f))))
  
  (define main-coordinates
    (vector 
     ;; safety between yellow and green
     (list 1/2 (+ 23/24 1/48) #t)
            
     ;; row to the left of green, going up
     (list (+ 5/9 1/18) (+ 23/24 1/48) #t)
     (list (+ 5/9 1/18) (+ 22/24 1/48) #t)
     (list (+ 5/9 1/18) (+ 21/24 1/48) #t)
     (list (+ 5/9 1/18) (+ 20/24 1/48) #t)
     (list (+ 5/9 1/18) (+ 19/24 1/48) #t)
     (list (+ 5/9 1/18) (+ 18/24 1/48) #t)
     (list (+ 5/9 1/18) (+ 17/24 1/48) #t)
     (list (+ 5/9 1/18) (+ 16/24 1/48) #t)
     
     ;; row above green, going right
     (list (+ 16/24 1/48) (+ 5/9 1/18) #f)
     (list (+ 17/24 1/48) (+ 5/9 1/18) #f)
     (list (+ 18/24 1/48) (+ 5/9 1/18) #f)
     (list (+ 19/24 1/48) (+ 5/9 1/18) #f)
     (list (+ 20/24 1/48) (+ 5/9 1/18) #f)
     (list (+ 21/24 1/48) (+ 5/9 1/18) #f)
     (list (+ 22/24 1/48) (+ 5/9 1/18) #f)
     (list (+ 23/24 1/48) (+ 5/9 1/18) #f)
     
     ;; safety between green and red
     (list (+ 23/24 1/48) 1/2 #f)
     
     ;; row below red, going left
     (list (+ 23/24 1/48) (+ 3/9 1/18) #f)
     (list (+ 22/24 1/48) (+ 3/9 1/18) #f)
     (list (+ 21/24 1/48) (+ 3/9 1/18) #f)
     (list (+ 20/24 1/48) (+ 3/9 1/18) #f)
     (list (+ 19/24 1/48) (+ 3/9 1/18) #f)
     (list (+ 18/24 1/48) (+ 3/9 1/18) #f)
     (list (+ 17/24 1/48) (+ 3/9 1/18) #f)
     (list (+ 16/24 1/48) (+ 3/9 1/18) #f)

     ;; row to the left of red, going up
     (list (+ 5/9 1/18) (+ 7/24 1/48) #t)
     (list (+ 5/9 1/18) (+ 6/24 1/48) #t)
     (list (+ 5/9 1/18) (+ 5/24 1/48) #t)
     (list (+ 5/9 1/18) (+ 4/24 1/48) #t)
     (list (+ 5/9 1/18) (+ 3/24 1/48) #t)
     (list (+ 5/9 1/18) (+ 2/24 1/48) #t)
     (list (+ 5/9 1/18) (+ 1/24 1/48) #t)
     (list (+ 5/9 1/18) (+ 0/24 1/48) #t)
          
     ;; safety between red and blue
     (list 1/2 (+ 0/24 1/48) #t)

     ;; row to the right of blue, going down
     (list (+ 3/9 1/18) (+ 0/24 1/48) #t)
     (list (+ 3/9 1/18) (+ 1/24 1/48) #t)
     (list (+ 3/9 1/18) (+ 2/24 1/48) #t)
     (list (+ 3/9 1/18) (+ 3/24 1/48) #t)
     (list (+ 3/9 1/18) (+ 4/24 1/48) #t)
     (list (+ 3/9 1/18) (+ 5/24 1/48) #t)
     (list (+ 3/9 1/18) (+ 6/24 1/48) #t)
     (list (+ 3/9 1/18) (+ 7/24 1/48) #t)
     
     ;; row below blue, going left
     (list (+ 7/24 1/48) (+ 3/9 1/18) #f)
     (list (+ 6/24 1/48) (+ 3/9 1/18) #f)
     (list (+ 5/24 1/48) (+ 3/9 1/18) #f)
     (list (+ 4/24 1/48) (+ 3/9 1/18) #f)
     (list (+ 3/24 1/48) (+ 3/9 1/18) #f)
     (list (+ 2/24 1/48) (+ 3/9 1/18) #f)
     (list (+ 1/24 1/48) (+ 3/9 1/18) #f)
     (list (+ 0/24 1/48) (+ 3/9 1/18) #f)
     
     ;; safety between blue and yellow
     (list (+ 0/24 1/48) 1/2 #f)

     ;; row above yellow to the right
     (list (+ 0/24 1/48) (+ 5/9 1/18) #f)
     (list (+ 1/24 1/48) (+ 5/9 1/18) #f)
     (list (+ 2/24 1/48) (+ 5/9 1/18) #f)
     (list (+ 3/24 1/48) (+ 5/9 1/18) #f)
     (list (+ 4/24 1/48) (+ 5/9 1/18) #f)
     (list (+ 5/24 1/48) (+ 5/9 1/18) #f)
     (list (+ 6/24 1/48) (+ 5/9 1/18) #f)
     (list (+ 7/24 1/48) (+ 5/9 1/18) #f)
     
     ;; row to the right of yellow, going down
     (list (+ 3/9 1/18) (+ 16/24 1/48) #t)
     (list (+ 3/9 1/18) (+ 17/24 1/48) #t)
     (list (+ 3/9 1/18) (+ 18/24 1/48) #t)
     (list (+ 3/9 1/18) (+ 19/24 1/48) #t)
     (list (+ 3/9 1/18) (+ 20/24 1/48) #t)
     (list (+ 3/9 1/18) (+ 21/24 1/48) #t)
     (list (+ 3/9 1/18) (+ 22/24 1/48) #t)
     (list (+ 3/9 1/18) (+ 23/24 1/48) #t)
     ))
            
  
  (define (find-main-coordinates index w h)
    (let ([e (vector-ref main-coordinates index)])
      (values (* w (car e)) (* h (cadr e)) (caddr e))))
    
  (define (at-start-coordinates w h piece-size)
    `((blue ,(* w (+ 1/6 (* circle-gap 1/2))) ,(* h 1/6))
      (red ,(* w 5/6) ,(* h (+ 1/6 (* circle-gap 1/2))))
      (green ,(* w (- 5/6 (* circle-gap 1/2))) ,(* h 5/6))
      (yellow ,(* w 1/6) ,(* h (- 5/6 (* circle-gap 1/2))))))
  
  (define (at-home-coordinates w h piece-size)
    `((blue ,(* w 1/2) ,(+ (* h 1/3) piece-size))
      (red ,(- (* w 2/3) piece-size) ,(* h 1/2))
      (green ,(* w 1/2) ,(- (* h 2/3) piece-size))
      (yellow ,(+ (* w 1/3) piece-size) ,(* h 1/2))))
  
  (define (get-number-font color w h)
    (send the-font-list find-or-create-font
          (cond
            [(or (<= (* w 1/3 1/8) 10)
                 (<= (* h 1/3 1/8) 10))
             8]
            [(or (<= (* w 1/3 1/8) 15)
                 (<= (* h 1/3 1/8) 15))
             9]
            [else
             12])
          'default
          'normal
          'normal))
  
  (define (get-number-color color)
    (send the-color-database find-color
          (case color
            [(red) "white"]
            [else "black"])))
 
  (define (get-piece-size w h)
    (min (* w 1/3 1/8)
         (* h 1/3 1/8)))
  
  (define black (make-object color% "black"))
  
  (define board-canvas%
    (class canvas%
      (init-field [board (new-board)])
      (inherit get-dc get-client-size)
      (define/public (set-board b) 
        (set! board b)
        (unless buffer (resize-bitmap))
        (redraw-bitmap)
        (on-paint))
      
      (define buffer #f)
      (define bdc (make-object bitmap-dc%))
      
      (define/override (on-paint)
        (unless buffer
          (resize-bitmap))
        ;(send (get-dc) draw-bitmap buffer 0 0)
        (let ([dc (get-dc)])
          (send dc set-anti-alias #t)
          (draw-board board dc 32 32 0 0 #t)))
      
      (define/override (on-size w h)
        (resize-bitmap))
      
      (define/private (resize-bitmap)
        (let-values ([(w h) (get-client-size)])
          (set! buffer (make-object bitmap% w h))
          (redraw-bitmap)))
      
      (define/private (redraw-bitmap)
        (let-values ([(w h) (get-client-size)])
          (send bdc set-bitmap buffer)
          (draw-board board bdc w h 0 0 #t)
          (send bdc set-bitmap #f)))
      
      (super-new)
     
      (inherit min-client-width min-client-height)
      (min-client-height 32)
      (min-client-width 32)))

  (let ()
    (define board (new-board))
    (define bm (make-object bitmap% 32 32))
    (define mask-bm (make-object bitmap% 32 32 #t))
    (define bdc (make-object bitmap-dc% mask-bm))
    (send bdc clear)
    (send bdc set-bitmap bm)
    (send bm set-loaded-mask mask-bm)
    ;(send bdc set-anti-alias #t)
    (draw-board board bdc 32 32 0 0 #f)
    (send bdc set-bitmap mask-bm)
    (draw-board board bdc 32 32 0 0 #f)
    (send bdc set-bitmap #f)
    (send bm save-file "parcheesi.png" 'png))
  
  (define (show-board board)
    (define f (new frame% (label "")))
    (define c (new board-canvas% (parent f) (board board)))
    (send f show #t))

  (show-board (new-board)))
