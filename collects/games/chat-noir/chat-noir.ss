;#lang scheme (require htdp/world lang/posn) (define-syntax (check-expect stx) #'(void))

(require "hash.ss")

;; constants
(define circle-radius 20)
(define circle-spacing 22)

(define normal-color 'lightskyblue)
(define on-shortest-path-color 'white)
(define blocked-color 'black)
(define under-mouse-color 'black)

;; data definitions

;; a world is:
;;  (make-world board posn state number mouse posn-or-false boolean)
(define-struct world (board cat state size mouse-posn h-down?))

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
;;  - (make-dist-cell posn (number or '∞))
(define-struct dist-cell (p n))


;; build-bfs-table : world (or/c 'boundary posn) -> distance-table
(define (build-bfs-table world init-point)
  (local [;; posn : posn
          ;; dist : number
          (define-struct queue-ent (posn dist))
          
          (define neighbors/w (neighbors world))
          
          (define (bfs queue dist-table)
            (cond
              [(empty? queue) dist-table]
              [else
               (local [(define hd (first queue))]
                 (cond
                   [(boolean? (hash-ref dist-table (queue-ent-posn hd) #f))
                    (local [(define dist (queue-ent-dist hd))
                            (define p (queue-ent-posn hd))]
                      (bfs 
                       (append (rest queue) 
                               (map (lambda (p) (make-queue-ent p (+ dist 1)))
                                    (neighbors/w p)))
                       (hash-set dist-table p dist)))]
                   [else
                    (bfs (rest queue) dist-table)]))]))]
                    
    (hash-map 
     (bfs (list (make-queue-ent init-point 0))
          (make-immutable-hash/list-init))
     make-dist-cell)))

;; same-sets? : (listof X) (listof X) -> boolean
(define (same-sets? l1 l2)
  (and (andmap (lambda (e1) (member e1 l2)) l1)
       (andmap (lambda (e2) (member e2 l1)) l2)))

(check-expect (same-sets? (list) (list)) true)
(check-expect (same-sets? (list) (list 1)) false)
(check-expect (same-sets? (list 1) (list)) false)
(check-expect (same-sets? (list 1 2) (list 2 1)) true)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(check-expect (same-sets? 
               (build-bfs-table (make-world (empty-board 3) (make-posn 1 1) 'playing 3 (make-posn 0 0) false)
                                'boundary)
               (list
                (make-dist-cell 'boundary 0)
                
                (make-dist-cell (make-posn 1 0) 1)
                (make-dist-cell (make-posn 2 0) 1)
                
                (make-dist-cell (make-posn 0 1) 1)
                (make-dist-cell (make-posn 1 1) 2)
                (make-dist-cell (make-posn 2 1) 1)
                
                (make-dist-cell (make-posn 1 2) 1)
                (make-dist-cell (make-posn 2 2) 1)))
              true)

(check-expect (same-sets? 
               (build-bfs-table (make-world (empty-board 3) (make-posn 1 1) 'playing 3 (make-posn 0 0) false)
                                (make-posn 1 1))
               (list
                (make-dist-cell 'boundary 2)

                (make-dist-cell (make-posn 1 0) 1)
                (make-dist-cell (make-posn 2 0) 1)
                
                (make-dist-cell (make-posn 0 1) 1)
                (make-dist-cell (make-posn 1 1) 0)
                (make-dist-cell (make-posn 2 1) 1)
                
                (make-dist-cell (make-posn 1 2) 1)
                (make-dist-cell (make-posn 2 2) 1)))
              true)

(check-expect (same-sets? 
               (build-bfs-table (make-world (list
                                             (make-cell (make-posn 0 1) true)
                                             (make-cell (make-posn 1 0) true)
                                             (make-cell (make-posn 1 1) false)
                                             (make-cell (make-posn 1 2) true)
                                             (make-cell (make-posn 2 0) true)
                                             (make-cell (make-posn 2 1) true)
                                             (make-cell (make-posn 2 2) true))
                                            (make-posn 1 1)
                                            'playing
                                            3
                                            (make-posn 0 0)
                                            false)
                                'boundary)
               (list
                (make-dist-cell 'boundary 0)))
              true)

(check-expect (same-sets? 
               (build-bfs-table (make-world (empty-board 5)
                                            (make-posn 2 2)
                                            'playing
                                            5
                                            (make-posn 0 0)
                                            false)
                                'boundary)
               (list
                (make-dist-cell 'boundary 0)

                (make-dist-cell (make-posn 1 0) 1)
                (make-dist-cell (make-posn 2 0) 1)
                (make-dist-cell (make-posn 3 0) 1)
                (make-dist-cell (make-posn 4 0) 1)
                
                (make-dist-cell (make-posn 0 1) 1)
                (make-dist-cell (make-posn 1 1) 2)
                (make-dist-cell (make-posn 2 1) 2)
                (make-dist-cell (make-posn 3 1) 2)
                (make-dist-cell (make-posn 4 1) 1)
                
                (make-dist-cell (make-posn 0 2) 1)
                (make-dist-cell (make-posn 1 2) 2)
                (make-dist-cell (make-posn 2 2) 3)
                (make-dist-cell (make-posn 3 2) 2)
                (make-dist-cell (make-posn 4 2) 1)

                (make-dist-cell (make-posn 0 3) 1)
                (make-dist-cell (make-posn 1 3) 2)
                (make-dist-cell (make-posn 2 3) 2)
                (make-dist-cell (make-posn 3 3) 2)
                (make-dist-cell (make-posn 4 3) 1)

                
                (make-dist-cell (make-posn 1 4) 1)
                (make-dist-cell (make-posn 2 4) 1)
                (make-dist-cell (make-posn 3 4) 1)
                (make-dist-cell (make-posn 4 4) 1)))
              true)

(check-expect (same-sets? 
               (build-bfs-table (make-world (block-cell
                                             (make-posn 4 2)
                                             (empty-board 5))
                                            (make-posn 2 2)
                                            'playing
                                            5
                                            (make-posn 0 0)
                                            false)
                                'boundary)
               (list
                (make-dist-cell 'boundary 0)

                (make-dist-cell (make-posn 1 0) 1)
                (make-dist-cell (make-posn 2 0) 1)
                (make-dist-cell (make-posn 3 0) 1)
                (make-dist-cell (make-posn 4 0) 1)
                
                (make-dist-cell (make-posn 0 1) 1)
                (make-dist-cell (make-posn 1 1) 2)
                (make-dist-cell (make-posn 2 1) 2)
                (make-dist-cell (make-posn 3 1) 2)
                (make-dist-cell (make-posn 4 1) 1)
                
                (make-dist-cell (make-posn 0 2) 1)
                (make-dist-cell (make-posn 1 2) 2)
                (make-dist-cell (make-posn 2 2) 3)
                (make-dist-cell (make-posn 3 2) 3)

                (make-dist-cell (make-posn 0 3) 1)
                (make-dist-cell (make-posn 1 3) 2)
                (make-dist-cell (make-posn 2 3) 2)
                (make-dist-cell (make-posn 3 3) 2)
                (make-dist-cell (make-posn 4 3) 1)

                
                (make-dist-cell (make-posn 1 4) 1)
                (make-dist-cell (make-posn 2 4) 1)
                (make-dist-cell (make-posn 3 4) 1)
                (make-dist-cell (make-posn 4 4) 1)))
              true)

(check-expect (same-sets? 
               (build-bfs-table (make-world (empty-board 5)
                                            (make-posn 2 2)
                                            'playing
                                            5
                                            (make-posn 0 0)
                                            false)
                                (make-posn 2 2))
               (list
                (make-dist-cell 'boundary 3)
                
                (make-dist-cell (make-posn 1 0) 2)
                (make-dist-cell (make-posn 2 0) 2)
                (make-dist-cell (make-posn 3 0) 2)
                (make-dist-cell (make-posn 4 0) 3)
                
                (make-dist-cell (make-posn 0 1) 2)
                (make-dist-cell (make-posn 1 1) 1)
                (make-dist-cell (make-posn 2 1) 1)
                (make-dist-cell (make-posn 3 1) 2)
                (make-dist-cell (make-posn 4 1) 3)
                
                (make-dist-cell (make-posn 0 2) 2)
                (make-dist-cell (make-posn 1 2) 1)
                (make-dist-cell (make-posn 2 2) 0)
                (make-dist-cell (make-posn 3 2) 1)
                (make-dist-cell (make-posn 4 2) 2)

                (make-dist-cell (make-posn 0 3) 2)
                (make-dist-cell (make-posn 1 3) 1)
                (make-dist-cell (make-posn 2 3) 1)
                (make-dist-cell (make-posn 3 3) 2)
                (make-dist-cell (make-posn 4 3) 3)

                
                (make-dist-cell (make-posn 1 4) 2)
                (make-dist-cell (make-posn 2 4) 2)
                (make-dist-cell (make-posn 3 4) 2)
                (make-dist-cell (make-posn 4 4) 3)))
              true)

(check-expect (lookup-in-table
               (build-bfs-table (make-world (empty-board 5)
                                            (make-posn 2 2)
                                            'playing
                                            5
                                            (make-posn 0 0)
                                            false)
                                (make-posn 2 2))
               (make-posn 1 4))
              2)


;; lookup-in-table : distance-map posn -> number or '∞
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


;; p : world -> posn -> boolean
;; returns true when the posn is on the shortest path 
;; from the cat to the edge of the board, in the given world
(define (on-cats-path? w)
  (cond
    [(world-h-down? w)
     (local [(define edge-distance-map (build-bfs-table w 'boundary))
             (define cat-distance-map (build-bfs-table w (world-cat w)))
             (define cat-distance (lookup-in-table edge-distance-map 
                                                   (world-cat w)))]
       (cond
         [(equal? cat-distance '∞)
          (lambda (p) false)]
         [else
          (lambda (p)
            (equal? (+/f (lookup-in-table cat-distance-map p)
                         (lookup-in-table edge-distance-map p))
                    cat-distance))]))]
    [else
     (lambda (p) false)]))

(check-expect ((on-cats-path? (make-world (empty-board 5) (make-posn 1 1) 'playing 5 (make-posn 0 0) true))
               (make-posn 1 0))
              true)
(check-expect ((on-cats-path? (make-world (empty-board 5) (make-posn 1 1) 'playing 5 (make-posn 0 0) false))
               (make-posn 1 0))
              false)
(check-expect ((on-cats-path? (make-world (empty-board 5) (make-posn 1 1) 'playing 5 (make-posn 0 0) true))
               (make-posn 2 1))
              false)
(check-expect ((on-cats-path?
                (make-world (list
                             (make-cell (make-posn 0 1) true)
                             (make-cell (make-posn 1 0) true)
                             (make-cell (make-posn 1 1) false)
                             (make-cell (make-posn 1 2) true)
                             (make-cell (make-posn 2 0) true)
                             (make-cell (make-posn 2 1) true)
                             (make-cell (make-posn 2 2) true))
                            (make-posn 1 1)
                            'cat-lost
                            3
                            (make-posn 0 0)
                            true))
               (make-posn 0 1))
              false)

;; neighbors : world -> (or/c 'boundary posn) -> (listof (or/c 'boundary posn))
;; computes the neighbors of a posn, for a given board size
(define (neighbors w)
  (local [(define blocked 
            (map cell-p
                 (filter (lambda (c) 
                           (or (cell-blocked? c)
                               (equal? (cell-p c) (world-mouse-posn w))))
                         (world-board w))))
          (define boundary-cells (filter (lambda (p) 
                                           (and (not (member p blocked))
                                                (on-boundary? p (world-size w))))
                                         (map cell-p (world-board w))))]
    (lambda (p)
      (cond 
        [(member p blocked)
         '()]
        [(equal? p 'boundary)
         boundary-cells]
        [else
         (local [(define x (posn-x p))
                 (define y (posn-y p))
                 (define adjacent-posns (adjacent p (world-size w)))
                 (define in-bounds
                   (filter (lambda (x) (in-bounds? x (world-size w)))
                           adjacent-posns))]
           (filter
            (lambda (x) (not (member x blocked)))
            (cond
              [(equal? in-bounds adjacent-posns)
               in-bounds]
              [else
               (cons 'boundary in-bounds)])))]))))

(check-expect ((neighbors (empty-world 11))  (make-posn 1 1))
              (adjacent (make-posn 1 1) 11))
(check-expect ((neighbors (empty-world 11)) (make-posn 2 2))
              (adjacent (make-posn 2 2) 11))
(check-expect ((neighbors (empty-world 3)) 'boundary)
              (list (make-posn 0 1) 
                    (make-posn 1 0) 
                    (make-posn 1 2) 
                    (make-posn 2 0) 
                    (make-posn 2 1) 
                    (make-posn 2 2)))
(check-expect ((neighbors (empty-world 11)) (make-posn 1 0))
              (list 'boundary 
                    (make-posn 2 0)
                    (make-posn 0 1)
                    (make-posn 1 1)))
(check-expect ((neighbors (make-world (list
                                       (make-cell (make-posn 0 1) false)
                                       (make-cell (make-posn 1 0) false)
                                       (make-cell (make-posn 1 1) true)
                                       (make-cell (make-posn 1 2) false)
                                       (make-cell (make-posn 2 0) false)
                                       (make-cell (make-posn 2 1) false)
                                       (make-cell (make-posn 2 2) false))
                                      (make-posn 1 1)
                                      'playing
                                      3
                                      (make-posn 0 0)
                                      false))
               (make-posn 1 1))
              '())
(check-expect ((neighbors (make-world (list
                                       (make-cell (make-posn 0 1) false)
                                       (make-cell (make-posn 1 0) false)
                                       (make-cell (make-posn 1 1) true)
                                       (make-cell (make-posn 1 2) false)
                                       (make-cell (make-posn 2 0) false)
                                       (make-cell (make-posn 2 1) false)
                                       (make-cell (make-posn 2 2) false))
                                      (make-posn 1 1)
                                      'playing
                                      3
                                      (make-posn 0 0)
                                      false))
               (make-posn 1 0))
              (list 'boundary (make-posn 2 0) (make-posn 0 1)))


;; adjacent : posn number -> (listof posn)
;; returns a list of the posns that are adjacent to 
;; `p' on an infinite hex grid
(define (adjacent p board-size)
  (local [(define x (posn-x p))
          (define y (posn-y p))]
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
             (make-posn (+ x 1) (+ y 1)))])))

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

;; <=/f : (number or '∞) (number or '∞) -> boolean
(define (<=/f a b)
  (cond
    [(equal? b '∞) true]
    [(equal? a '∞) false]
    [else (<= a b)]))
(check-expect (<=/f 1 2) true)
(check-expect (<=/f 2 1) false)
(check-expect (<=/f '∞ 1) false)
(check-expect (<=/f 1 '∞) true)
(check-expect (<=/f '∞ '∞) true)

(define (+/f x y)
  (cond
    [(or (equal? x '∞) (equal? y '∞))
     '∞]
    [else 
     (+ x y)]))

(check-expect (+/f '∞ '∞) '∞)
(check-expect (+/f '∞ 1) '∞)
(check-expect (+/f 1 '∞) '∞)
(check-expect (+/f 1 2) 3)


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


;; render-world : world -> image
(define (render-world w)
  (chop-whiskers
   (overlay (board->image (world-board w) 
                          (world-size w)
                          (on-cats-path? w)
                          (world-mouse-posn w))
            (move-pinhole
             (cond
               [(equal? (world-state w) 'cat-won) happy-cat]
               [(equal? (world-state w) 'cat-lost) sad-cat]
               [else thinking-cat])
             (- (cell-center-x (world-cat w)))
             (- (cell-center-y (world-cat w)))))))

(check-expect
 (render-world
  (make-world (list (make-cell (make-posn 0 1) false))
              (make-posn 0 1)
              'playing
              2
              (make-posn 0 0)
              false))
 (overlay
  (board->image (list (make-cell (make-posn 0 1) false))
                2
                (lambda (x) true)
                false)
  (move-pinhole thinking-cat 
                (- (cell-center-x (make-posn 0 1)))
                (- (cell-center-y (make-posn 0 1))))))

(check-expect
 (render-world
  (make-world (list (make-cell (make-posn 0 1) false))
              (make-posn 0 1)
              'cat-won
              2
              false
              false))
 (overlay
  (board->image (list (make-cell (make-posn 0 1) false))
                2
                (lambda (x) true)
                false)
  (move-pinhole happy-cat 
                (- (cell-center-x (make-posn 0 1)))
                (- (cell-center-y (make-posn 0 1))))))

(check-expect
 (render-world
  (make-world (list (make-cell (make-posn 0 1) false))
              (make-posn 0 1)
              'cat-lost
              2
              false
              false))
 (overlay
  (board->image (list (make-cell (make-posn 0 1) false))
                2
                (lambda (x) true)
                false)
  (move-pinhole sad-cat 
                (- (cell-center-x (make-posn 0 1)))
                (- (cell-center-y (make-posn 0 1))))))

(check-expect
 (render-world
  (make-world (list
               (make-cell (make-posn 0 1) true)
               (make-cell (make-posn 1 0) true)
               (make-cell (make-posn 1 1) false)
               (make-cell (make-posn 1 2) true)
               (make-cell (make-posn 2 0) true)
               (make-cell (make-posn 2 1) true)
               (make-cell (make-posn 2 2) true))
              (make-posn 1 1)
              'cat-lost
              3
              false
              false))
 (overlay
  (board->image (list
                 (make-cell (make-posn 0 1) true)
                 (make-cell (make-posn 1 0) true)
                 (make-cell (make-posn 1 1) false)
                 (make-cell (make-posn 1 2) true)
                 (make-cell (make-posn 2 0) true)
                 (make-cell (make-posn 2 1) true)
                 (make-cell (make-posn 2 2) true))
                3
                (lambda (x) false)
                false)
  (move-pinhole sad-cat 
                (- (cell-center-x (make-posn 1 1)))
                (- (cell-center-y (make-posn 1 1))))))

(check-expect
 (render-world
  (make-world (list
               (make-cell (make-posn 0 1) false)
               (make-cell (make-posn 1 0) false)
               (make-cell (make-posn 1 1) false)
               (make-cell (make-posn 1 2) false)
               (make-cell (make-posn 2 0) false)
               (make-cell (make-posn 2 1) false)
               (make-cell (make-posn 2 2) false))
              (make-posn 1 1)
              'cat-lost
              3
              (make-posn (cell-center-x (make-posn 0 1))
                         (cell-center-y (make-posn 0 1)))
              true))
 
 (overlay
  (board->image (list
                 (make-cell (make-posn 0 1) false)
                 (make-cell (make-posn 1 0) false)
                 (make-cell (make-posn 1 1) false)
                 (make-cell (make-posn 1 2) false)
                 (make-cell (make-posn 2 0) false)
                 (make-cell (make-posn 2 1) false)
                 (make-cell (make-posn 2 2) false))
                3
                (lambda (x) true)
                (make-posn (cell-center-x (make-posn 0 1))
                           (cell-center-y (make-posn 0 1))))
  (move-pinhole sad-cat 
                (- (cell-center-x (make-posn 1 1)))
                (- (cell-center-y (make-posn 1 1))))))

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
  (render-world 
   (make-world
    (empty-board 3)
    (make-posn 0 0)
    'playing
    3
    (make-posn 0 0)
    false)))
 0)
(check-expect
 (pinhole-x
  (render-world
   (make-world
    (empty-board 3)
    (make-posn 0 1)
    'playing
    3
    (make-posn 0 0)
    false)))
 0)


;; board->image : board number (posn -> boolean) posn-or-false -> image
(define (board->image cs world-size on-cat-path? mouse)
  (foldl (lambda (x y) (overlay y x))
         (nw:rectangle (world-width world-size)
                       (world-height world-size)
                       'solid
                       'white)
         (map (lambda (c) (cell->image c 
                                       (on-cat-path? (cell-p c)) 
                                       (and (posn? mouse)
                                            (equal? mouse (cell-p c)))
                                       #;
                                       (and (posn? mouse)
                                            (point-in-this-circle? (cell-p c) 
                                                                   (posn-x mouse)
                                                                   (posn-y mouse)))))
              cs)))

(check-expect (board->image (list (make-cell (make-posn 0 0) false))
                            3
                            (lambda (x) false)
                            false)
              (overlay
               (nw:rectangle (world-width 3)
                             (world-height 3)
                             'solid
                             'white)
               (cell->image (make-cell (make-posn 0 0) false)
                            false
                            false)))

(check-expect (board->image (list (make-cell (make-posn 0 0) false))
                            3
                            (lambda (x) true)
                            false)
              (overlay
               (nw:rectangle (world-width 3)
                             (world-height 3)
                             'solid
                             'white)
               (cell->image (make-cell (make-posn 0 0) false)
                            true
                            false)))


(check-expect (board->image (list (make-cell (make-posn 0 0) false))
                            3
                            (lambda (x) false)
                            false)
              (overlay
               (nw:rectangle (world-width 3)
                             (world-height 3)
                             'solid
                             'white)
               (cell->image (make-cell (make-posn 0 0) false)
                            false
                            false)))

(check-expect (board->image (list (make-cell (make-posn 0 0) false)
                                  (make-cell (make-posn 0 1) false))
                            3
                            (lambda (x) (equal? x (make-posn 0 1)))
                            false)
              (overlay
               (nw:rectangle (world-width 3)
                             (world-height 3)
                             'solid
                             'white)
               (cell->image (make-cell (make-posn 0 0) false)
                            false
                            false)
               (cell->image (make-cell (make-posn 0 1) false)
                            true
                            false)))

(check-expect (board->image (list (make-cell (make-posn 0 0) false)
                                  (make-cell (make-posn 0 1) false))
                            3
                            (lambda (x) (equal? x (make-posn 0 1)))
                            (make-posn 0 0))
              (overlay
               (nw:rectangle (world-width 3)
                             (world-height 3)
                             'solid
                             'white)
               (cell->image (make-cell (make-posn 0 0) false)
                            false
                            true)
               (cell->image (make-cell (make-posn 0 1) false)
                            true
                            false)))

;; cell->image : cell boolean boolean -> image
(define (cell->image c on-short-path? under-mouse?)
  (local [(define x (cell-center-x (cell-p c)))
          (define y (cell-center-y (cell-p c)))
          (define main-circle 
            (cond
              [(cell-blocked? c)
               (circle circle-radius 'solid blocked-color)]
              [else
               (circle circle-radius 'solid normal-color)]))]
    (move-pinhole 
     (cond
       [under-mouse?
        (overlay main-circle
                 (circle (quotient circle-radius 2) 'solid under-mouse-color))]
       [on-short-path?
        (overlay main-circle
                 (circle (quotient circle-radius 2) 'solid on-shortest-path-color))]
       [else
        main-circle])
     (- x)
     (- y))))

(check-expect (cell->image (make-cell (make-posn 0 0) false) false false)
              (move-pinhole (circle circle-radius 'solid normal-color)
                            (- circle-radius)
                            (- circle-radius)))
(check-expect (cell->image (make-cell (make-posn 0 0) true) false false)
              (move-pinhole (circle circle-radius 'solid 'black)
                            (- circle-radius)
                            (- circle-radius)))
(check-expect (cell->image (make-cell (make-posn 0 0) false) true false)
              (move-pinhole (overlay (circle circle-radius 'solid normal-color)
                                     (circle (quotient circle-radius 2) 'solid on-shortest-path-color))
                            (- circle-radius)
                            (- circle-radius)))
(check-expect (cell->image (make-cell (make-posn 0 0) false) true true)
              (move-pinhole (overlay (circle circle-radius 'solid normal-color)
                                     (circle (quotient circle-radius 2) 'solid under-mouse-color))
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


;; cell-center-x : posn -> number
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

;; cell-center-y : posn -> number
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
       [(and (equal? 'playing (world-state world))
             (point-in-a-circle? (world-board world) x y))
        (move-cat 
         (update-world-posn 
          (make-world (add-obstacle (world-board world) x y)
                      (world-cat world)
                      (world-state world)
                      (world-size world)
                      (world-mouse-posn world)
                      (world-h-down? world))
          (make-posn x y)))]
       [else (update-world-posn world (make-posn x y))])]
    [(equal? evt 'button-down)
     world]
    [(equal? evt 'drag) world]
    [(equal? evt 'move)
     (update-world-posn world (make-posn x y))]
    [(equal? evt 'enter)
     (update-world-posn world (make-posn x y))]
    [(equal? evt 'leave)
     (update-world-posn world false)]))

(check-expect (clack (make-world '() (make-posn 0 0) 'playing 1 false false)
                     1 1 'button-down)
              (make-world '() (make-posn 0 0) 'playing 1 false false))
(check-expect (clack (make-world '() (make-posn 0 0) 'playing 1 false false)
                     1 1 'drag)
              (make-world '() (make-posn 0 0) 'playing 1 false false))
(check-expect (clack (make-world (list (make-cell (make-posn 0 0) false))
                                 (make-posn 0 1)
                                 'playing
                                 1
                                 false
                                 false)
                     (cell-center-x (make-posn 0 0))
                     (cell-center-y (make-posn 0 0))
                     'move)
              (make-world (list (make-cell (make-posn 0 0) false))
                          (make-posn 0 1)
                          'playing
                          1
                          (make-posn 0 0)
                          false))
(check-expect (clack (make-world (list (make-cell (make-posn 0 0) false))
                                 (make-posn 0 1)
                                 'playing
                                 1
                                 false
                                 false)
                     (cell-center-x (make-posn 0 0))
                     (cell-center-y (make-posn 0 0))
                     'enter)
              (make-world (list (make-cell (make-posn 0 0) false))
                          (make-posn 0 1)
                          'playing
                          1
                          (make-posn 0 0)
                          false))
(check-expect (clack (make-world '() (make-posn 0 0) 'playing 1 (make-posn 0 0) false)
                     1 1 'leave)
              (make-world '() (make-posn 0 0) 'playing 1 false false))

(check-expect (clack (make-world '() (make-posn 0 0) 'playing 1 (make-posn 0 0) false)
                     10
                     10
                     'button-down)
              (make-world '() (make-posn 0 0) 'playing 1 (make-posn 0 0) false))

(check-expect (clack (make-world (list (make-cell (make-posn 0 0) false)
                                       (make-cell (make-posn 1 1) false))
                                 (make-posn 1 1)
                                 'playing
                                 3
                                 (make-posn 0 0)
                                 false)
                     (cell-center-x (make-posn 0 0)) 
                     (cell-center-y (make-posn 0 0))
                     'button-up)
              (make-world (list (make-cell (make-posn 0 0) true)
                                (make-cell (make-posn 1 1) false))
                          (make-posn 1 1)
                          'cat-lost
                          3
                          (make-posn 0 0)
                          false))


(check-expect (clack (make-world '() (make-posn 0 0) 'cat-lost 1 (make-posn 0 0) false)
                     10
                     10
                     'button-up)
              (make-world '() (make-posn 0 0) 'cat-lost 1 (make-posn 0 0) false))
(check-expect (clack 
               (make-world
                (list (make-cell (make-posn 1 0) false)
                      (make-cell (make-posn 2 0) true)
                      (make-cell (make-posn 0 1) true)
                      (make-cell (make-posn 1 1) false)
                      (make-cell (make-posn 2 1) true)
                      (make-cell (make-posn 1 2) true)
                      (make-cell (make-posn 2 2) true))
                (make-posn 1 1)
                'playing
                3
                false
                false)
               (cell-center-x (make-posn 1 0))
               (cell-center-y (make-posn 1 0))
               'button-up)
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
               3
               (make-posn 1 0)
               false))

(check-expect (clack 
               (make-world
                (list (make-cell (make-posn 1 0) false)
                      (make-cell (make-posn 2 0) false)
                      (make-cell (make-posn 0 1) true)
                      (make-cell (make-posn 1 1) false)
                      (make-cell (make-posn 2 1) true)
                      (make-cell (make-posn 1 2) true)
                      (make-cell (make-posn 2 2) true))
                (make-posn 1 1)
                'playing
                3
                false
                false)
               (cell-center-x (make-posn 1 0))
               (cell-center-y (make-posn 1 0))
               'button-up)
              (make-world 
               (list (make-cell (make-posn 1 0) true)
                     (make-cell (make-posn 2 0) false)
                     (make-cell (make-posn 0 1) true)
                     (make-cell (make-posn 1 1) false)
                     (make-cell (make-posn 2 1) true)
                     (make-cell (make-posn 1 2) true)
                     (make-cell (make-posn 2 2) true))
               (make-posn 2 0)
               'cat-won
               3
               (make-posn 1 0)
               false))

;; update-world-posn/playing : world posn-or-false -> world
(define (update-world-posn w p)
  (cond
    [(equal? (world-state w) 'playing)
     (cond
       [(posn? p)
        (local [(define mouse-spot 
                  (circle-at-point (world-board w) 
                                   (posn-x p)
                                   (posn-y p)))]
          (make-world (world-board w)
                      (world-cat w)
                      (world-state w)
                      (world-size w)
                      (cond
                        [(equal? mouse-spot (world-cat w))
                         false]
                        [else
                         mouse-spot])
                      (world-h-down? w)))]
       [else
        (make-world (world-board w)
                    (world-cat w)
                    (world-state w)
                    (world-size w)
                    false
                    (world-h-down? w))])]
    [else w]))

(check-expect (update-world-posn 
               (make-world (list (make-cell (make-posn 0 0) false)) (make-posn 0 1) 'playing 1 false false)
               (make-posn (cell-center-x (make-posn 0 0))
                          (cell-center-y (make-posn 0 0))))
               (make-world (list (make-cell (make-posn 0 0) false)) (make-posn 0 1) 'playing 1 (make-posn 0 0) false))

(check-expect (update-world-posn 
               (make-world (list (make-cell (make-posn 0 0) false)) (make-posn 0 0) 'playing 1 false false)
               (make-posn (cell-center-x (make-posn 0 0))
                          (cell-center-y (make-posn 0 0))))
               (make-world (list (make-cell (make-posn 0 0) false)) (make-posn 0 0) 'playing 1 false false))

(check-expect (update-world-posn 
               (make-world (list (make-cell (make-posn 0 0) false)) (make-posn 0 1) 'playing 1 (make-posn 0 0) false)
               (make-posn 0 0))
               (make-world (list (make-cell (make-posn 0 0) false)) (make-posn 0 1) 'playing 1 false false))
(check-expect (update-world-posn 
               (make-world (list (make-cell (make-posn 0 0) false)) (make-posn 0 1) 'cat-won 1 false false)
               (make-posn (cell-center-x (make-posn 0 0))
                          (cell-center-y (make-posn 0 0))))
               (make-world (list (make-cell (make-posn 0 0) false)) (make-posn 0 1) 'cat-won 1 false false))
(check-expect (update-world-posn 
               (make-world (list (make-cell (make-posn 0 0) false)) (make-posn 0 1) 'cat-lost 1 false false)
               (make-posn (cell-center-x (make-posn 0 0))
                          (cell-center-y (make-posn 0 0))))
               (make-world (list (make-cell (make-posn 0 0) false)) (make-posn 0 1) 'cat-lost 1 false false))

;; move-cat : world -> world
(define (move-cat world)
  (local [(define cat-position (world-cat world))
          (define table (build-bfs-table world 'boundary))
          (define neighbors (adjacent cat-position (world-size world)))
          (define next-cat-positions
            (find-best-positions neighbors
                                 (map (lambda (p) (lookup-in-table table p))
                                      neighbors)))
          (define next-cat-position
            (cond
              [(boolean? next-cat-positions) false]
              [else
               (list-ref next-cat-positions
                         (random (length next-cat-positions)))]))]
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
                (world-size world)
                (world-mouse-posn world)
                (world-h-down? world))))


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
              5
              (make-posn 0 0)
              false))
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
             5
             (make-posn 0 0)
             false))

;; find-best-positions : (nelistof posn) (nelistof number or '∞) -> (nelistof posn) or false
(define (find-best-positions posns scores)
  (local [(define best-score (foldl (lambda (x sofar)
                                      (if (<=/f x sofar)
                                          x
                                          sofar))
                                    (first scores)
                                    (rest scores)))]
    (cond
      [(symbol? best-score) false]
      [else
       (map
        second
        (filter (lambda (x) (equal? (first x) best-score))
                (map list scores posns)))])))
(check-expect (find-best-positions (list (make-posn 0 0)) (list 1))
              (list (make-posn 0 0)))
(check-expect (find-best-positions (list (make-posn 0 0)) (list '∞))
              false)
(check-expect (find-best-positions (list (make-posn 0 0)
                                         (make-posn 1 1))
                                   (list 1 2))
              (list (make-posn 0 0)))
(check-expect (find-best-positions (list (make-posn 0 0)
                                         (make-posn 1 1))
                                   (list 1 1))
              (list (make-posn 0 0)
                    (make-posn 1 1)))
(check-expect (find-best-positions (list (make-posn 0 0)
                                         (make-posn 1 1))
                                   (list '∞ 2))
              (list (make-posn 1 1)))
(check-expect (find-best-positions (list (make-posn 0 0)
                                         (make-posn 1 1))
                                   (list '∞ '∞))
              false)

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

;; circle-at-point : board number number -> posn-or-false
;; returns the posn corresponding to cell where the x,y coordinates are
(define (circle-at-point board x y)
  (cond
    [(empty? board) false]
    [else 
     (cond
       [(point-in-this-circle? (cell-p (first board)) x y)
        (cell-p (first board))]
       [else
        (circle-at-point (rest board) x y)])]))
(check-expect (circle-at-point empty 0 0) false)
(check-expect (circle-at-point (list (make-cell (make-posn 0 0) false))
                               (cell-center-x (make-posn 0 0))
                               (cell-center-y (make-posn 0 0)))
              (make-posn 0 0))
(check-expect (circle-at-point (list (make-cell (make-posn 0 0) false))
                               0 0)
              false)

     
;; point-in-a-circle? : board number number -> boolean
(define (point-in-a-circle? board x y)
  (posn? (circle-at-point board x y)))
(check-expect (point-in-a-circle? empty 0 0) false)
(check-expect (point-in-a-circle? (list (make-cell (make-posn 0 0) false))
                                  (cell-center-x (make-posn 0 0))
                                  (cell-center-y (make-posn 0 0)))
              true)
(check-expect (point-in-a-circle? (list (make-cell (make-posn 0 0) false))
                                  0 0)
              false)

;; point-in-this-circle? : posn number number -> boolean
(define (point-in-this-circle? p x y)
  (local [(define center (+ (cell-center-x p)
                            (* (sqrt -1) (cell-center-y p))))
          (define p2 (+ x (* (sqrt -1) y)))]
    (<= (magnitude (- center p2)) circle-radius)))

(check-expect (point-in-this-circle? (make-posn 0 0)
                                     (cell-center-x (make-posn 0 0))
                                     (cell-center-y (make-posn 0 0)))
              true)
(check-expect (point-in-this-circle? (make-posn 0 0) 0 0)
              false)
              
;; change : world key-event -> world
(define (change w ke)
  (make-world (world-board w)
              (world-cat w)
              (world-state w)
              (world-size w)
              (world-mouse-posn w)
              (key=? ke #\h)))

(check-expect (change (make-world '() (make-posn 1 1) 'playing 1 (make-posn 0 0) false)
                      #\h)
              (make-world '() (make-posn 1 1) 'playing 1 (make-posn 0 0) true))
(check-expect (change (make-world '() (make-posn 1 1) 'playing 1 (make-posn 0 0) true)
                      'release)
              (make-world '() (make-posn 1 1) 'playing 1 (make-posn 0 0) false))




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

;; add-n-random-blocked-cells : number (listof cell) number -> (listof cell)
(define (add-n-random-blocked-cells n all-cells board-size)
  (cond
    [(zero? n) all-cells]
    [else
     (local [(define unblocked-cells 
               (filter (lambda (x) 
                         (let ([cat-cell? (and (= (posn-x (cell-p x)) (quotient board-size 2))
                                               (= (posn-y (cell-p x)) (quotient board-size 2)))])
                           
                           (and (not (cell-blocked? x))
                                (not cat-cell?))))
                       all-cells))
             (define to-block (list-ref unblocked-cells 
                                        (random (length unblocked-cells))))]
       (add-n-random-blocked-cells
        (sub1 n)
        (block-cell (cell-p to-block) all-cells)
        board-size))]))

;; block-cell : posn board -> board
(define (block-cell to-block board)
  (map (lambda (c) (if (equal? to-block (cell-p c))
                       (make-cell to-block true)
                       c))
       board))
(check-expect (block-cell (make-posn 1 1)
                          (list (make-cell (make-posn 0 0) false)
                                (make-cell (make-posn 1 1) false)
                                (make-cell (make-posn 2 2) false)))
              (list (make-cell (make-posn 0 0) false)
                    (make-cell (make-posn 1 1) true)
                    (make-cell (make-posn 2 2) false)))

(check-expect (add-n-random-blocked-cells 0 (list (make-cell (make-posn 0 0) true)) 10)
              (list (make-cell (make-posn 0 0) true)))
(check-expect (add-n-random-blocked-cells 1 (list (make-cell (make-posn 0 0) false)) 10)
              (list (make-cell (make-posn 0 0) true)))

;; empty-board : number -> (listof cell)
(define (empty-board board-size)
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
          (make-cell (make-posn i j) 
                     false))))))))

(check-expect (empty-board 3)
              (list
               (make-cell (make-posn 0 1) false)
               (make-cell (make-posn 1 0) false)
               (make-cell (make-posn 1 1) false)
               (make-cell (make-posn 1 2) false)
               (make-cell (make-posn 2 0) false)
               (make-cell (make-posn 2 1) false)
               (make-cell (make-posn 2 2) false)))

;; empty-world : number -> world
(define (empty-world board-size)
  (make-world (empty-board board-size)
              (make-posn (quotient board-size 2)
                         (quotient board-size 2))
              'playing
              board-size
              (make-posn 0 0)
              false))

(check-expect (empty-world 3)
              (make-world (list
                           (make-cell (make-posn 0 1) false)
                           (make-cell (make-posn 1 0) false)
                           (make-cell (make-posn 1 1) false)
                           (make-cell (make-posn 1 2) false)
                           (make-cell (make-posn 2 0) false)
                           (make-cell (make-posn 2 1) false)
                           (make-cell (make-posn 2 2) false))
                          (make-posn 1 1)
                          'playing
                          3
                          (make-posn 0 0)
                          false))

(define dummy
  (local
    [(define board-size 11)
     (define initial-board
       (add-n-random-blocked-cells
        6
        (empty-board board-size)
        board-size))
     (define initial-world
       (make-world initial-board
                   (make-posn (quotient board-size 2)
                              (quotient board-size 2))
                   'playing
                   board-size
                   false
                   false))]
    
    (and 
     (big-bang (world-width board-size)
               (world-height board-size)
               1 
               initial-world)
     (on-redraw render-world)
     (on-key-event change)
     (on-mouse-event clack))))
