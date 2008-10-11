#|

hint: include the size of the board in your world structure
This enables you to make test cases with different size boards,
making some of the test cases much easier to manage.

|#

(define circle-radius 20)
(define circle-spacing 22)

;; a world is:
;;  (make-world board posn state number)
(define-struct world (board cat state size))

;; a state is either:
;;   - 'playing
;;   - 'cat-won
;;   - 'cat-lost

;; a board is
;;  (listof cell)

;; a cell is
;; (make-cell (make-posn int[0-board-size]
;;                       int[0-board-size])
;;            boolean)
(define-struct cell (p blocked?))


;                                                                          
;                                                                          
;                                                                          
;                                                                          
;                                                                          
;          ;;                                     ;;;;                     
;       ;;;;                                      ;;;;;                    
;        ;;;                              ;                                
;    ;;; ;;;    ; ;;;;   ;;;;   ;;;;;;  ;;  ;;;;     ;;;;;;  ;;   ;;;; ;;; 
;   ;;;;;;;;;;;;;; ;;;; ;;;;;;;;;  ;;   ;;   ;   ;;;;;  ;;; ;;;;  ;;;;;;;  
;  ;;;;;;;;;  ;;; ;;;;;;;;;;;;;;; ;;;   ;;   ;;    ;;;  ;;; ;;;;  ;   ;;;; 
;  ;;;   ;;;  ;;; ;;;; ;;;    ;;; ;;;   ;;;  ;;;   ;;;  ;;;; ;;;  ;;   ;;; 
;  ;;   ;;;;  ;;;      ;;    ;;;; ;;;   ;;;; ;;;   ;;;  ;;;  ;;;   ;;;;;;; 
;  ;;;;;;;;   ;;;      ;;;;;;;;;; ;;;   ;; ;;;;;  ;;;;  ;;;  ;;;       ;;; 
;   ;;;;;;;;;;;;;;;;   ;;;;;;;;;;; ;;; ;;;  ;;;  ;;;;;;;;;;  ;;;  ;;;; ;;; 
;                       ;;;;;        ;;;;                   ;;;; ;;;;; ;;; 
;                                                             ;;;;;;; ;;;  
;                                                                 ;;;;;;   
;                                                                          


;; world->image : world -> image
(define (world->image w)
  (chop-whiskers
   (overlay (board->image (world-board w) (world-size w))
            (move-pinhole
             (cond
               [(equal? (world-state w) 'cat-won) happy-cat]
               [(equal? (world-state w) 'cat-lost) sad-cat]
               [else thinking-cat])
             (- (cell-center-x (world-cat w)))
             (- (cell-center-y (world-cat w)))))))

(check-expect
 (world->image
  (make-world (list (make-cell (make-posn 0 1) false))
              (make-posn 0 1)
              'playing
              2))
 (overlay
  (board->image (list (make-cell (make-posn 0 1) false))
                2)
  (move-pinhole thinking-cat 
                (- (cell-center-x (make-posn 0 1)))
                (- (cell-center-y (make-posn 0 1))))))
(check-expect
 (world->image
  (make-world (list (make-cell (make-posn 0 1) false))
              (make-posn 0 1)
              'cat-won
              2))
 (overlay
  (board->image (list (make-cell (make-posn 0 1) false))
                2)
  (move-pinhole happy-cat 
                (- (cell-center-x (make-posn 0 1)))
                (- (cell-center-y (make-posn 0 1))))))
(check-expect
 (world->image
  (make-world (list (make-cell (make-posn 0 1) false))
              (make-posn 0 1)
              'cat-lost
              2))
 (overlay
  (board->image (list (make-cell (make-posn 0 1) false))
                2)
  (move-pinhole sad-cat 
                (- (cell-center-x (make-posn 0 1)))
                (- (cell-center-y (make-posn 0 1))))))

;; chop-whiskers : image -> image
;; crops the image so that anything above or to the left of the pinhole is gone
(define (chop-whiskers img)
  (shrink img 
          0 
          0
          (- (image-width img) (pinhole-x img) 1) 
          (- (image-height img) (pinhole-y img) 1)))
(check-expect (chop-whiskers (rectangle 5 5 'solid 'black))
              (put-pinhole (rectangle 3 3 'solid 'black) 0 0))
(check-expect (chop-whiskers (rectangle 6 6 'solid 'black))
              (put-pinhole (rectangle 3 3 'solid 'black) 0 0))

(check-expect
 (pinhole-x
  (world->image 
   (make-world
    (list (make-cell (make-posn 0 0) false)
          (make-cell (make-posn 0 1) false)
          (make-cell (make-posn 1 0) false))
    (make-posn 0 0)
    'playing
    2)))
 0)
(check-expect
 (pinhole-x
  (world->image
   (make-world
    (list (make-cell (make-posn 0 0) false)
          (make-cell (make-posn 0 1) false)
          (make-cell (make-posn 1 0) false))
    (make-posn 0 1)
    'playing
    2)))
 0)


;; board->image : board number -> image
(define (board->image cs world-size)
  (foldl overlay 
         (nw:rectangle (world-width world-size)
                       (world-height world-size)
                       'outline
                       'black)
         (map cell->image cs)))

(check-expect (board->image (list (make-cell (make-posn 0 0) false)) 3)
              (overlay
               (cell->image 
                (make-cell (make-posn 0 0) false))
               (nw:rectangle (world-width 3)
                             (world-height 3)
                             'outline
                             'black)))


;; cell->image : cell -> image
(define (cell->image c)
  (local [(define x (cell-center-x (cell-p c)))
          (define y (cell-center-y (cell-p c)))]
    (move-pinhole 
     (cond
       [(cell-blocked? c)
        (circle circle-radius 'solid 'black)]
       [else
        (circle circle-radius 'solid 'lightblue)])
     (- x)
     (- y))))

(check-expect (cell->image (make-cell (make-posn 0 0) false))
              (move-pinhole (circle circle-radius 'solid 'lightblue)
                            (- circle-radius)
                            (- circle-radius)))
(check-expect (cell->image (make-cell (make-posn 0 0) true))
              (move-pinhole (circle circle-radius 'solid 'black)
                            (- circle-radius)
                            (- circle-radius)))

;; world-width : number -> number
;; computes the width of the drawn world in terms of its size
(define (world-width board-size)
  (local [(define rightmost-posn 
            (make-posn (- board-size 1) (- board-size 2)))]
    (+ (cell-center-x rightmost-posn) circle-radius)))
(check-expect (world-width 3) 150)

;; world-height : number -> number
;; computes the height of the drawn world in terms of its size
(define (world-height board-size)
  (local [(define bottommost-posn
            (make-posn (- board-size 1) (- board-size 1)))]
    (+ (cell-center-y bottommost-posn) circle-radius)))
(check-expect (world-height 3) 116.208)


;; cell-center : cell -> number
(define (cell-center-x p)
  (local [(define x (posn-x p))
          (define y (posn-y p))]
    (+ circle-radius
       (* x circle-spacing 2)
       (if (odd? y)
           circle-spacing
           0))))

(check-expect (cell-center-x (make-posn 0 0))
              circle-radius)
(check-expect (cell-center-x (make-posn 0 1))
              (+ circle-spacing circle-radius))
(check-expect (cell-center-x (make-posn 1 0))
              (+ (* 2 circle-spacing) circle-radius))
(check-expect (cell-center-x (make-posn 1 1))
              (+ (* 3 circle-spacing) circle-radius))

;; cell-center-y : cell -> number
(define (cell-center-y p)
  (local [(define y (posn-y p))]
    (+ circle-radius
       (* y circle-spacing 2 
          .866 ;; .866 is an exact approximate to sin(pi/3)
          ))))

(check-expect (cell-center-y (make-posn 1 1))
              (+ circle-radius (* 2 circle-spacing .866)))
(check-expect (cell-center-y (make-posn 1 0))
              circle-radius)


;                                                     
;                                                     
;                                                     
;                                                     
;                                                     
;                                           ;;;;;     
;                                            ;;;;     
;                                            ;;;      
;   ;;;; ;;;    ; ;;;;   ;;;;   ;;  ; ;;;;   ;;;   ;  
;   ;;;;;;; ;;;;;; ;;;; ;;;;;;;;; ;;;;;;;;;  ;;;  ;;; 
;   ;   ;;;;  ;;; ;;;;;;;;;;;;;;;  ;;    ;;  ;;; ;;;; 
;   ;;   ;;;  ;;; ;;;; ;;;    ;;;  ;     ;;  ;;;; ;;; 
;    ;;;;;;;  ;;;      ;;    ;;;;  ;    ;;;  ;;;  ;;; 
;        ;;;  ;;;      ;;;;;;;;;;  ;;;;;;;    ;;  ;;; 
;   ;;;; ;;;;;;;;;;;   ;;;;;;;;;;; ;; ;;;     ;;  ;;; 
;  ;;;;; ;;;            ;;;;;      ;;        ;;;; ;;; 
;  ;;;; ;;;                        ;;               ;;
;   ;;;;;;                        ;                   
;                                                     

;; a distance-map is 
;;  (listof dist-cells)

;; a dist-cell is
;;  - (make-dist-cell posn number)
(define-struct dist-cell (p n))

;; build-table/fast : world -> distance-map
(define (build-table/fast world)
  (local [(define board-size (world-size world))
          (define blocked (make-hash))
          (define ht (make-hash))
          (define (search p)
            (cond
              [(hash-ref blocked p)
               '∞]
              [(on-boundary? p board-size) 
               ((lambda (a b) b)
                (hash-set! ht p 0)
                0)]
              [(not (boolean? (hash-ref ht p #f)))
               (hash-ref ht p)]
              [else
               ((lambda (a b c) c)
                (hash-set! ht p '∞)
                (hash-set! 
                 ht
                 p
                 (add1/f (min-l (map search 
                                     (adjacent p board-size)))))
                (hash-ref ht p))]))]
    ((lambda (a b c) c)
     (for-each (lambda (cell)
                 (hash-set! blocked 
                            (cell-p cell)
                            (cell-blocked? cell)))
               (world-board world))
     (search (world-cat world))
     (hash-map ht make-dist-cell))))

;; build-table : world -> distance-map
(define (build-table world)
  (build-distance (world-board world)
                  (world-cat world)
                  '()
                  '()
                  (world-size world)))

;; build-distance : board posn table (listof posn) number -> distance-map
(define (build-distance board p t visited board-size)
  (cond
    [(cell-blocked? (lookup-board board p)) 
     (add-to-table p '∞ t)]
    [(on-boundary? p board-size)
     (add-to-table p 0 t)]
    [(in-table? t p)
     t]
    [(member p visited)
     (add-to-table p '∞ t)]
    [else
     (local [(define neighbors (adjacent p board-size))
             (define neighbors-t (build-distances
                                  board
                                  neighbors
                                  t
                                  (cons p visited)
                                  board-size))]
       (add-to-table p
                     (add1/f 
                      (min-l 
                       (map (lambda (neighbor)
                              (lookup-in-table neighbors-t neighbor))
                            neighbors)))
                     neighbors-t))]))

;; build-distances : board (listof posn) distance-map (listof posn) number 
;;                -> distance-map
(define (build-distances board ps t visited board-size)
  (cond
    [(empty? ps) t]
    [else 
     (build-distances board 
                      (rest ps) 
                      (build-distance board (first ps) t visited board-size)
                      visited
                      board-size)]))

(check-expect (build-distance (list (make-cell (make-posn 0 0) false)) 
                              (make-posn 0 0)
                              '()
                              '()
                              1)
              (list (make-dist-cell (make-posn 0 0) 0)))

(check-expect (build-distance (list (make-cell (make-posn 0 0) true)) 
                              (make-posn 0 0)
                              '()
                              '()
                              1)
              (list (make-dist-cell (make-posn 0 0) '∞)))

(check-expect (build-distance (list (make-cell (make-posn 0 1) false)
                                    (make-cell (make-posn 1 0) false)
                                    (make-cell (make-posn 1 1) false) 
                                    (make-cell (make-posn 1 2) false)
                                    (make-cell (make-posn 2 0) false) 
                                    (make-cell (make-posn 2 1) false) 
                                    (make-cell (make-posn 2 2) false)) 
                              (make-posn 1 1)
                              '()
                              '()
                              3)
              (list (make-dist-cell (make-posn 1 0) 0) 
                    (make-dist-cell (make-posn 2 0) 0) 
                    (make-dist-cell (make-posn 0 1) 0)
                    (make-dist-cell (make-posn 2 1) 0) 
                    (make-dist-cell (make-posn 1 2) 0)
                    (make-dist-cell (make-posn 2 2) 0)
                    (make-dist-cell (make-posn 1 1) 1)))

(check-expect (build-distance (list (make-cell (make-posn 0 1) true)
                                    (make-cell (make-posn 1 0) true)
                                    (make-cell (make-posn 1 1) false) 
                                    (make-cell (make-posn 1 2) true)
                                    (make-cell (make-posn 2 0) true) 
                                    (make-cell (make-posn 2 1) true) 
                                    (make-cell (make-posn 2 2) true)) 
                              (make-posn 1 1)
                              '()
                              '()
                              3)
              (list (make-dist-cell (make-posn 1 0) '∞)
                    (make-dist-cell (make-posn 2 0) '∞)
                    (make-dist-cell (make-posn 0 1) '∞)
                    (make-dist-cell (make-posn 2 1) '∞)
                    (make-dist-cell (make-posn 1 2) '∞)
                    (make-dist-cell (make-posn 2 2) '∞)
                    (make-dist-cell (make-posn 1 1) '∞)))

(check-expect (build-distance
               (append-all
                (build-list
                 5
                 (lambda (i)
                   (build-list
                    5
                    (lambda (j) 
                      (make-cell (make-posn i j) false))))))
               (make-posn 2 2)
               '()
               '()
               5)
              (list (make-dist-cell (make-posn 1 0) 0)
                    (make-dist-cell (make-posn 2 0) 0)
                    (make-dist-cell (make-posn 0 1) 0)
                    (make-dist-cell (make-posn 3 0) 0)
                    (make-dist-cell (make-posn 1 1) 1)
                    (make-dist-cell (make-posn 4 0) 0)
                    (make-dist-cell (make-posn 2 1) 1)
                    (make-dist-cell (make-posn 4 1) 0)
                    (make-dist-cell (make-posn 3 1) 1)
                    (make-dist-cell (make-posn 2 2) 2)
                    (make-dist-cell (make-posn 4 2) 0)
                    (make-dist-cell (make-posn 3 2) 1)
                    (make-dist-cell (make-posn 0 2) 0)
                    (make-dist-cell (make-posn 0 3) 0)
                    (make-dist-cell (make-posn 1 3) 1)
                    (make-dist-cell (make-posn 1 2) 1)
                    (make-dist-cell (make-posn 2 3) 1)
                    (make-dist-cell (make-posn 1 4) 0)
                    (make-dist-cell (make-posn 2 4) 0)
                    (make-dist-cell (make-posn 4 3) 0)
                    (make-dist-cell (make-posn 3 4) 0)
                    (make-dist-cell (make-posn 4 4) 0)
                    (make-dist-cell (make-posn 3 3) 1)))


;; lookup-board : board posn -> cell-or-false
(define (lookup-board board p)
  (cond
    [(empty? board) (error 'lookup-board "did not find posn")]
    [else
     (cond
       [(equal? (cell-p (first board)) p)
        (first board)]
       [else
        (lookup-board (rest board) p)])]))

(check-expect (lookup-board (list (make-cell (make-posn 2 2) false))
                            (make-posn 2 2))
              (make-cell (make-posn 2 2) false))
(check-error (lookup-board '() (make-posn 0 0))
             "lookup-board: did not find posn")

;; add-to-table : posn number table -> table
(define (add-to-table p n t) 
  (cond
    [(empty? t) (list (make-dist-cell p n))]
    [else 
     (cond
       [(equal? p (dist-cell-p (first t)))
        (cons (make-dist-cell p (min/f (dist-cell-n (first t)) n))
              (rest t))]
       [else
        (cons (first t) (add-to-table p n (rest t)))])]))

(check-expect (add-to-table (make-posn 1 2) 3 '())
              (list (make-dist-cell (make-posn 1 2) 3)))
(check-expect (add-to-table (make-posn 1 2)
                            3
                            (list (make-dist-cell (make-posn 1 2) 4)))
              (list (make-dist-cell (make-posn 1 2) 3)))
(check-expect (add-to-table (make-posn 1 2)
                            3 
                            (list (make-dist-cell (make-posn 1 2) 2)))
              (list (make-dist-cell (make-posn 1 2) 2)))
(check-expect (add-to-table (make-posn 1 2)
                            3
                            (list (make-dist-cell (make-posn 2 2) 2)))
              (list (make-dist-cell (make-posn 2 2) 2)
                    (make-dist-cell (make-posn 1 2) 3)))

;; in-table : table posn -> boolean
(define (in-table? t p) (number? (lookup-in-table t p)))

(check-expect (in-table? empty (make-posn 1 2)) false)
(check-expect (in-table? (list (make-dist-cell (make-posn 1 2) 3))
                         (make-posn 1 2))
              true)
(check-expect (in-table? (list (make-dist-cell (make-posn 2 1) 3))
                         (make-posn 1 2))
              false)

;; lookup-in-table : table posn -> number or '∞
;; looks for the distance as recorded in the table t, 
;; if not found returns a distance of '∞
(define (lookup-in-table t p)
  (cond
    [(empty? t) '∞]
    [else (cond
            [(equal? p (dist-cell-p (first t)))
             (dist-cell-n (first t))]
            [else
             (lookup-in-table (rest t) p)])]))

(check-expect (lookup-in-table empty (make-posn 1 2)) '∞)
(check-expect (lookup-in-table (list (make-dist-cell (make-posn 1 2) 3))
                               (make-posn 1 2))
              3)
(check-expect (lookup-in-table (list (make-dist-cell (make-posn 2 1) 3))
                               (make-posn 1 2))
              '∞)

;; on-boundary? : posn number -> boolean
(define (on-boundary? p board-size)
  (or (= (posn-x p) 0)
      (= (posn-y p) 0)
      (= (posn-x p) (- board-size 1))
      (= (posn-y p) (- board-size 1))))

(check-expect (on-boundary? (make-posn 0 1) 13) true)
(check-expect (on-boundary? (make-posn 1 0) 13) true)
(check-expect (on-boundary? (make-posn 12 1) 13) true)
(check-expect (on-boundary? (make-posn 1 12) 13) true)
(check-expect (on-boundary? (make-posn 1 1) 13) false)
(check-expect (on-boundary? (make-posn 10 10) 13) false)

;; adjacent : posn number -> (listof posn)
(define (adjacent p board-size)
  (local [(define x (posn-x p))
          (define y (posn-y p))]
    (filter (lambda (x) (in-bounds? x board-size))
            (cond
              [(even? y)
               (list (make-posn (- x 1) (- y 1))
                     (make-posn x (- y 1))
                     (make-posn (- x 1) y)
                     (make-posn (+ x 1) y)
                     (make-posn (- x 1) (+ y 1))
                     (make-posn x (+ y 1)))]
              [else
               (list (make-posn x (- y 1))
                     (make-posn (+ x 1) (- y 1))
                     (make-posn (- x 1) y)
                     (make-posn (+ x 1) y)
                     (make-posn x (+ y 1))
                     (make-posn (+ x 1) (+ y 1)))]))))

(check-expect (adjacent (make-posn 1 1) 11)
              (list (make-posn 1 0)
                    (make-posn 2 0)
                    (make-posn 0 1)
                    (make-posn 2 1)
                    (make-posn 1 2)
                    (make-posn 2 2)))
(check-expect (adjacent (make-posn 2 2) 11)
              (list (make-posn 1 1)
                    (make-posn 2 1)
                    (make-posn 1 2)
                    (make-posn 3 2)
                    (make-posn 1 3)
                    (make-posn 2 3)))

;; in-bounds? : posn number -> boolean
(define (in-bounds? p board-size)
  (and (<= 0 (posn-x p) (- board-size 1))
       (<= 0 (posn-y p) (- board-size 1))
       (not (equal? p (make-posn 0 0)))
       (not (equal? p (make-posn 0 (- board-size 1))))))
(check-expect (in-bounds? (make-posn 0 0) 11) false)
(check-expect (in-bounds? (make-posn 0 1) 11) true)
(check-expect (in-bounds? (make-posn 1 0) 11) true)
(check-expect (in-bounds? (make-posn 10 10) 11) true)
(check-expect (in-bounds? (make-posn 0 -1) 11) false)
(check-expect (in-bounds? (make-posn -1 0) 11) false)
(check-expect (in-bounds? (make-posn 0 11) 11) false)
(check-expect (in-bounds? (make-posn 11 0) 11) false)
(check-expect (in-bounds? (make-posn 10 0) 11) true)
(check-expect (in-bounds? (make-posn 0 10) 11) false)

;; min-l : (listof number-or-symbol) -> number-or-symbol
(define (min-l ls) (foldr (lambda (x y) (min/f x y)) '∞ ls))
(check-expect (min-l (list)) '∞)
(check-expect (min-l (list 10 1 12)) 1)

;; <=/f : (number or '∞) (number or '∞) -> (number or '∞)
(define (<=/f a b) (equal? a (min/f a b)))
(check-expect (<=/f 1 2) true)
(check-expect (<=/f 2 1) false)
(check-expect (<=/f '∞ 1) false)
(check-expect (<=/f 1 '∞) true)
(check-expect (<=/f '∞ '∞) true)

;; min/f : (number or '∞) (number or '∞) -> (number or '∞)
(define (min/f x y)
  (cond
    [(equal? x '∞) y]
    [(equal? y '∞) x]
    [else (min x y)]))
(check-expect (min/f '∞ 1) 1)
(check-expect (min/f 1 '∞) 1)
(check-expect (min/f '∞ '∞) '∞)
(check-expect (min/f 1 2) 1)

;; add1/f : number or '∞ -> number or '∞
(define (add1/f n)
  (cond
    [(equal? n '∞) '∞]
    [else (add1 n)]))
(check-expect (add1/f 1) 2)
(check-expect (add1/f '∞) '∞)

;                                          
;                                          
;                                          
;                                          
;                                          
;          ;;;;;  ;;;;         ;;;;;;      
;            ;;;  ;;;;;          ;;;;      
;            ;;;                 ;;;       
;    ;;;;;;  ;;;     ;   ;;;;;;  ;;;  ;;;; 
;   ;;; ;;;; ;;; ;;;;;  ;;; ;;;; ;;; ;;;;;;
;  ;;; ;;;;; ;;;   ;;; ;;; ;;;;; ;;; ;;;;; 
;  ;;;  ;;;; ;;;   ;;; ;;;  ;;;; ;;;;      
;  ;;;       ;;    ;;; ;;;        ;;;;;;   
;  ;;;    ; ;;;   ;;;; ;;;    ;   ;;    ;; 
;   ;;;  ;  ;;;; ;;;;;;;;;;  ;  ;;;;;  ;;;;
;    ;;;;                ;;;;              
;                                          
;                                          
;                                          


(define (clack world x y evt)
  (cond
    [(equal? evt 'button-up)
     (cond
       [(equal? 'playing (world-state world))
        (cond
          [(point-in-circle? (world-board world) x y)
           (move-cat 
            (make-world (add-obstacle (world-board world) x y)
                        (world-cat world)
                        (world-state world)
                        (world-size world)))]
          [else 
           world])]
       [else
        world])]
    [else 
     world]))

(check-expect (clack (make-world '() (make-posn 0 0) 'playing 1)
                     10
                     10
                     'button-down)
              (make-world '() (make-posn 0 0) 'playing 1))
(check-expect (clack (make-world '() (make-posn 0 0) 'cat-lost 1)
                     10
                     10
                     'button-up)
              (make-world '() (make-posn 0 0) 'cat-lost 1))
(check-expect (clack 
               (make-world
                (list (make-cell (make-posn 1 0) true)
                      (make-cell (make-posn 2 0) true)
                      (make-cell (make-posn 0 1) true)
                      (make-cell (make-posn 1 1) false)
                      (make-cell (make-posn 2 1) true)
                      (make-cell (make-posn 1 2) true)
                      (make-cell (make-posn 2 2) true))
                (make-posn 1 1)
                'playing
                3)
               10 10 'button-up)
              (make-world 
               (list (make-cell (make-posn 1 0) true)
                     (make-cell (make-posn 2 0) true)
                     (make-cell (make-posn 0 1) true)
                     (make-cell (make-posn 1 1) false)
                     (make-cell (make-posn 2 1) true)
                     (make-cell (make-posn 1 2) true)
                     (make-cell (make-posn 2 2) true))
               (make-posn 1 1)
               'cat-lost
               3))

;; move-cat : board -> board
(define (move-cat world)
  (local [(define cat-position (world-cat world))
          (define table (build-table/fast world))
          (define neighbors (adjacent cat-position (world-size world)))
          (define next-cat-position
            (find-best-position (first neighbors)
                                (lookup-in-table table (first neighbors))
                                (rest neighbors)
                                (map (lambda (p) (lookup-in-table table p))
                                     (rest neighbors))))]
    (make-world (world-board world)
                (cond 
                  [(boolean? next-cat-position)
                   cat-position]
                  [else next-cat-position])
                (cond
                  [(boolean? next-cat-position)
                   'cat-lost]
                  [(on-boundary? next-cat-position (world-size world))
                   'cat-won]
                  [else 'playing])
                (world-size world))))

(check-expect
 (move-cat
  (make-world (list (make-cell (make-posn 1 0) false)
                    (make-cell (make-posn 2 0) false)
                    (make-cell (make-posn 3 0) false)
                    (make-cell (make-posn 4 0) false)
                    
                    (make-cell (make-posn 0 1) false)
                    (make-cell (make-posn 1 1) true)
                    (make-cell (make-posn 2 1) true)
                    (make-cell (make-posn 3 1) false)
                    (make-cell (make-posn 4 1) false)
                    
                    (make-cell (make-posn 0 2) false)
                    (make-cell (make-posn 1 2) true)
                    (make-cell (make-posn 2 2) false)
                    (make-cell (make-posn 3 2) true)
                    (make-cell (make-posn 4 2) false)
                    
                    (make-cell (make-posn 0 3) false)
                    (make-cell (make-posn 1 3) true)
                    (make-cell (make-posn 2 3) false)
                    (make-cell (make-posn 3 3) false)
                    (make-cell (make-posn 4 3) false)
                    
                    (make-cell (make-posn 1 4) false)
                    (make-cell (make-posn 2 4) false)
                    (make-cell (make-posn 3 4) false)
                    (make-cell (make-posn 4 4) false))
              (make-posn 2 2)
              'playing
              5))
 (make-world (list (make-cell (make-posn 1 0) false)
                   (make-cell (make-posn 2 0) false)
                   (make-cell (make-posn 3 0) false)
                   (make-cell (make-posn 4 0) false)
                   
                   (make-cell (make-posn 0 1) false)
                   (make-cell (make-posn 1 1) true)
                   (make-cell (make-posn 2 1) true)
                   (make-cell (make-posn 3 1) false)
                   (make-cell (make-posn 4 1) false)
                   
                   (make-cell (make-posn 0 2) false)
                   (make-cell (make-posn 1 2) true)
                   (make-cell (make-posn 2 2) false)
                   (make-cell (make-posn 3 2) true)
                   (make-cell (make-posn 4 2) false)
                   
                   (make-cell (make-posn 0 3) false)
                   (make-cell (make-posn 1 3) true)
                   (make-cell (make-posn 2 3) false)
                   (make-cell (make-posn 3 3) false)
                   (make-cell (make-posn 4 3) false)
                   
                   (make-cell (make-posn 1 4) false)
                   (make-cell (make-posn 2 4) false)
                   (make-cell (make-posn 3 4) false)
                   (make-cell (make-posn 4 4) false))
             (make-posn 2 3)
             'playing
             5))

;; find-best-position : (nelistof posn) (nelistof number or '∞)
;;                   -> posn or #f
;; returns #f if there is no non-infinite move, otherwise returns
;; the next step for the cat.
(define (find-best-position best-posn score rest-posns scores)
  (cond
    [(empty? rest-posns) 
     (cond
       [(equal? score '∞)
        false]
       [else
        best-posn])]
    [else (cond
            [(<=/f score (first scores))
             (find-best-position best-posn
                                 score 
                                 (rest rest-posns)
                                 (rest scores))]
            [else
             (find-best-position (first rest-posns)
                                 (first scores)
                                 (rest rest-posns)
                                 (rest scores))])]))

(check-expect (find-best-position (make-posn 1 1)
                                  1
                                  (list (make-posn 2 2))
                                  (list 2))
              (make-posn 1 1))
(check-expect (find-best-position (make-posn 2 2)
                                  2
                                  (list)
                                  (list))
              (make-posn 2 2))
(check-expect (find-best-position (make-posn 2 2) 
                                  2 
                                  (list (make-posn 1 1)) 
                                  (list 1))
              (make-posn 1 1))
(check-expect (find-best-position (make-posn 2 2) 
                                  '∞ 
                                  (list (make-posn 1 1)) 
                                  (list 1))
              (make-posn 1 1))
(check-expect (find-best-position (make-posn 2 2)
                                  2
                                  (list (make-posn 1 1))
                                  (list '∞))
              (make-posn 2 2))

;; add-obstacle : board number number -> board
(define (add-obstacle board x y)
  (cond
    [(empty? board) board]
    [else 
     (local [(define cell (first board))
             (define cx (cell-center-x (cell-p cell)))
             (define cy (cell-center-y (cell-p cell)))]
       (cond
         [(and (<= (- cx circle-radius) x (+ cx circle-radius))
               (<= (- cy circle-radius) y (+ cy circle-radius)))
          (cons (make-cell (cell-p cell) true)
                (rest board))]
         [else
          (cons cell (add-obstacle (rest board) x y))]))]))

(check-expect (add-obstacle (list (make-cell (make-posn 0 0) false))
                            circle-spacing circle-spacing)
              (list (make-cell (make-posn 0 0) true)))
(check-expect (add-obstacle (list (make-cell (make-posn 0 0) false)) 100 100)
              (list (make-cell (make-posn 0 0) false)))
(check-expect (add-obstacle (list (make-cell (make-posn 0 0) false)
                                  (make-cell (make-posn 0 1) false))
                            circle-spacing circle-spacing)
              (list (make-cell (make-posn 0 0) true)
                    (make-cell (make-posn 0 1) false)))

;; point-in-circle? : board number number -> boolean
(define (point-in-circle? board x y)
  (cond
    [(empty? board) false]
    [else 
     (local [(define cell (first board))
             (define center (+ (cell-center-x (cell-p cell))
                               (* (sqrt -1) (cell-center-y (cell-p cell)))))
             (define p (+ x (* (sqrt -1) y)))]
       (or (<= (magnitude (- center p)) circle-radius)
           (point-in-circle? (rest board) x y)))]))
(check-expect (point-in-circle? empty 0 0) false)
(check-expect (point-in-circle? (list (make-cell (make-posn 0 0) false))
                                (cell-center-x (make-posn 0 0))
                                (cell-center-y (make-posn 0 0)))
              true)
(check-expect (point-in-circle? (list (make-cell (make-posn 0 0) false))
                                0 0)
              false)
                                
              



;                                
;                                
;                                
;                                
;                                
;                        ;;;;    
;                        ;;;     
;                        ;;;   ; 
;    ;;;;;;   ;;;;   ;;;;;;;;;;; 
;   ;;; ;;;; ;;;;;;;;;   ;;;   ;;
;  ;;; ;;;;;;;;;;;;;;;  ;;;      
;  ;;;  ;;;;;;;    ;;;  ;;; ;;;; 
;  ;;;      ;;    ;;;;  ;;; ;;;;;
;  ;;;    ; ;;;;;;;;;;  ;;;  ;;;;
;   ;;;  ;  ;;;;;;;;;;; ;;;   ;; 
;    ;;;;    ;;;;;       ;;;;;   
;                                
;                                
;                                


;; cat : symbol -> image
(define (cat mode)
  (local [(define face-color 
            (cond
              [(symbol=? mode 'sad) 'pink]
              [else 'lightgray]))
          
          (define left-ear (regular-polygon 3 8 'solid 'black (/ pi -3)))
          (define right-ear (regular-polygon 3 8 'solid 'black 0))
          (define ear-x-offset 14)
          (define ear-y-offset 9)
          
          (define eye (overlay (ellipse 12 8 'solid 'black)
                               (ellipse 6 4 'solid 'limegreen)))
          (define eye-x-offset 8)
          (define eye-y-offset 3)
          
          (define nose (regular-polygon 3 5 'solid 'black (/ pi 2)))
          
          (define mouth-happy 
            (overlay (ellipse 8 8 'solid face-color)
                     (ellipse 8 8 'outline 'black)
                     (move-pinhole
                      (rectangle 10 5 'solid face-color)
                      0
                      4)))
          (define mouth-no-expression
            (overlay (ellipse 8 8 'solid face-color)
                     (ellipse 8 8 'outline face-color)
                     (rectangle 10 5 'solid face-color)))
          
          (define mouth 
            (cond
              [(symbol=? mode 'happy) mouth-happy]
              [else mouth-no-expression]))
          (define mouth-x-offset 4)
          (define mouth-y-offset -5)]
    
    (add-line
     (add-line
      (add-line
       (add-line
        (add-line
         (add-line
          (overlay (move-pinhole left-ear (- ear-x-offset) ear-y-offset)
                   (move-pinhole right-ear (- ear-x-offset 1) ear-y-offset)
                   (ellipse 40 26 'solid 'black)
                   (ellipse 36 22 'solid face-color)
                   (move-pinhole mouth (- mouth-x-offset) mouth-y-offset)
                   (move-pinhole mouth mouth-x-offset mouth-y-offset)
                   (move-pinhole eye (- eye-x-offset) eye-y-offset)
                   (move-pinhole eye eye-x-offset eye-y-offset)
                   (move-pinhole nose -1 -4))
          6 4 30 12 'black)
         6 4 30 4 'black)
        6 4 30 -4 'black)
       -6 4 -30 12 'black)
      -6 4 -30 4 'black)
     -6 4 -30 -4 'black)))

(define happy-cat (cat 'happy))
(define sad-cat (cat 'sad))
(define thinking-cat (cat 'thinking))


;                                                        
;                                                        
;                                                        
;                                                        
;                                                        
;   ;;;;            ;;;;   ;;;;     ;;;;           ;;;;; 
;   ;;;;;           ;;;;;  ;;;      ;;;;;            ;;; 
;                          ;;;   ;                   ;;; 
;      ;;;;;;  ;;      ; ;;;;;;;;;     ;   ;;;;   ;; ;;; 
;  ;;;;;  ;;; ;;;; ;;;;;   ;;;   ;;;;;;;  ;;;;;;;;;  ;;; 
;    ;;;  ;;; ;;;;   ;;;  ;;;        ;;; ;;;;;;;;;;  ;;; 
;    ;;;  ;;;; ;;;   ;;;  ;;; ;;;;   ;;; ;;;    ;;;  ;;; 
;    ;;;  ;;;  ;;;   ;;;  ;;; ;;;;;  ;;; ;;    ;;;;  ;;  
;   ;;;;  ;;;  ;;;  ;;;;  ;;;  ;;;; ;;;; ;;;;;;;;;; ;;;  
;  ;;;;;;;;;;  ;;; ;;;;;;;;;;   ;; ;;;;;;;;;;;;;;;;;;;;; 
;             ;;;;         ;;;;;          ;;;;;          
;               ;;;                                      
;                                                        
;                                                        
;                                                    
;                                                    
;                                                    
;                                                    
;                                                    
;  ;;;;;                                           ;;
;   ;;;;                                        ;;;; 
;   ;;;                                          ;;; 
;   ;;; ;;;    ;;;;;    ;;;;   ;;   ; ;;;;   ;;; ;;; 
;   ;;;;;;;;  ;;;;;;;  ;;;;;;;;;;;;;;; ;;;; ;;;;;;;; 
;   ;;;;;;;; ;;;;;;;; ;;;;;;;;;;  ;;; ;;;;;;;;;;;;;; 
;   ;;;   ;; ;;     ; ;;;    ;;;  ;;; ;;;; ;;;   ;;; 
;   ;;;   ;; ;     ;; ;;    ;;;;  ;;;      ;;   ;;;; 
;   ;;;;;;;  ;;;;;;;; ;;;;;;;;;;  ;;;      ;;;;;;;;  
;  ;;;;;;;    ;;;;;;  ;;;;;;;;;;;;;;;;;;    ;;;;;;;;;
;              ;;;;    ;;;;;                         
;                                                    
;                                                    
;                                                    

;; append-all : (listof (list X)) -> (listof X)
(define (append-all ls)
  (foldr append empty ls))

(check-expect (append-all empty) empty)
(check-expect (append-all (list (list 1 2 3))) (list 1 2 3))
(check-expect (append-all (list (list 1) (list 2) (list 3)))
              (list 1 2 3))

(define dummy
  (local
    [(define board-size 11)
     (define initial-board
       (filter
        (lambda (c)
          (not (and (= 0 (posn-x (cell-p c)))
                    (or (= 0 (posn-y (cell-p c)))
                        (= (- board-size 1)
                           (posn-y (cell-p c)))))))
        (append-all
         (build-list
          board-size
          (lambda (i)
            (build-list
             board-size
             (lambda (j) 
               (let ([cat-cell? (and (= i (quotient board-size 2))
                                     (= j (quotient board-size 2)))])
                 (make-cell (make-posn i j) 
                            (and (not cat-cell?)
                                 (zero? (random 30))))))))))))
     (define initial-world
       (make-world initial-board
                   (make-posn (quotient board-size 2)
                              (quotient board-size 2))
                   'playing
                   board-size))]
    
    (and ;((lambda (x) true) (time (build-table initial-world))) ;((lambda (x) true) (time (build-table/fast initial-world)))
     (big-bang (world-width board-size)
               (world-height board-size)
               1 
               initial-world)
     (on-redraw world->image)
     (on-mouse-event clack))))
