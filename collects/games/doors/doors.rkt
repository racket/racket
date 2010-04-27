(module doors mzscheme
  (require games/gl-board-game/gl-board
           sgl/gl-vectors
           sgl
           sgl/bitmap
           mred
           mzlib/list
           mzlib/etc
           mzlib/class)
  
  (provide door-game%
           player-data
           thing-data
           bitmap->drawer)
  
  (define-struct door-game (board))
  
  (define light-blue (gl-float-vector 0.5 0.5 1.0 0.5))
  (define light-red (gl-float-vector 1.0 0.5 0.5 0.5))
    
  (define-struct room (data players things))
  (define-struct wall (drawer))
  (define-struct player (data drawer i j))
  (define-struct thing (data drawer i j heads-up?))

  (define (bitmap->drawer bm game)
    (let*-values ([(bm mask)
                   (cond
                    [(bm . is-a? . bitmap%) 
                     (values bm (send bm get-loaded-mask))]
                    [(bm . is-a? . image-snip%)
                     (values (send bm get-bitmap)
                             (send bm get-bitmap-mask))]
                    [else (raise-type-error
                           'bitmap->drawer
                           "bitmap% or image-snip% object"
                           bm)])]
                  [(dl) (bitmap->gl-list bm 
                                         #:with-gl (lambda (f)
                                                     (send game with-gl-context f))
                                         #:mask mask)])
      (lambda ()
        (gl-call-list dl))))
    
  (define door-game%
    (class object%
      (init [(canvas-parent parent)] x-rooms y-rooms 
            [move-callback void])
      
      (define rooms (build-vector
                     x-rooms
                     (lambda (i)
                       (build-vector
                        y-rooms
                        (lambda (j)
                          (make-room #f null null))))))

      (define walls (build-vector
                     (add1 (* 2 (add1 x-rooms)))
                     (lambda (i)
                       (build-vector
                        (add1 (* 2 (add1 y-rooms)))
                        (lambda (j)
                          (make-wall #f))))))

      (define board
        (new gl-board% 
             (parent canvas-parent)
             (min-x -1) (max-x (add1 x-rooms))
             (min-y -1) (max-y (add1 y-rooms))
             (lift 0.15)
             (move (lambda (piece to)
                     (let ((to-i (inexact->exact (floor (gl-vector-ref to 0))))
                           (to-j (inexact->exact (floor (gl-vector-ref to 1)))))
                       (when (and (< -1 to-i x-rooms)
                                  (< -1 to-j y-rooms))
                         (let ([room (vector-ref (vector-ref rooms to-i) to-j)])
                           (move-callback (player-data piece) (room-data room) to-i to-j))))))
             (phi 30)))
      
      (define/private (make-wall-dl dir door?)
        (send board with-gl-context
              (lambda ()
                (let ((list-id (gl-gen-lists 1)))
                  (gl-new-list list-id 'compile)
                  (let ([one-wall
                         (lambda (color)
                           (gl-material-v 'front-and-back 'ambient-and-diffuse color)
                           (when door?
                             (gl-begin 'polygon)
                             (gl-vertex 0.0 0.0 0.0)
                             (gl-vertex 0.33 0.0 0.0)
                             (gl-vertex 0.33 0.0 0.35)
                             (gl-vertex 0.0 0.0 0.35)
                             (gl-end)
                             (gl-begin 'polygon)
                             (gl-vertex 0.66 0.0 0.0)
                             (gl-vertex 1.0 0.0 0.0)
                             (gl-vertex 1.0 0.0 0.35)
                             (gl-vertex 0.66 0.0 0.35)
                             (gl-end))
                           (gl-begin 'polygon)
                           (gl-vertex 0.0 0.0 (if door? 0.35 0.0))
                           (gl-vertex 1.0 0.0 (if door? 0.35 0.0))
                           (gl-vertex 1.0 0.0 0.52)
                           (gl-vertex 0.0 0.0 0.52)
                           (gl-end))])
                    (case dir
                      [(s)
                       (one-wall light-blue)]
                      [(n)
                       (gl-push-matrix)
                       (gl-translate 0.0 1.0 0.0)
                       (one-wall light-blue)
                       (gl-pop-matrix)]
                      [(w)
                       (gl-push-matrix)
                       (gl-rotate 90 0 0 1)
                       (one-wall light-red)
                       (gl-pop-matrix)]
                      [(e)
                       (gl-push-matrix)
                       (gl-rotate 90 0 0 1)
                       (gl-translate 0.0 -1.0 0.0)
                       (one-wall light-red)
                       (gl-pop-matrix)])
                    (gl-end-list)
                    list-id)))))
      
      (define cache (make-hash-table 'equal))
      (define/private (make-wall-dl/cached dir door?)
        (let ([key (list dir door?)])
          (hash-table-get cache key
                          (lambda ()
                            (let ([dl (make-wall-dl dir door?)])
                              (hash-table-put! cache key dl)
                              dl)))))
    
      (define/private (make-wall-draw dx dy dir door)
        (let ([space-dl (make-wall-dl/cached dir
                                             (and door #t))])
          (lambda ()
            (gl-enable 'blend)
            (gl-blend-func 'src-alpha 'one-minus-src-alpha)
            (gl-push-matrix)
            (gl-translate dx dy 0.0)
            (gl-call-list space-dl)
            (gl-pop-matrix)
            (gl-blend-func 'one 'one)
            (gl-disable 'blend)
            (when door
              (let ([a-door
                     (lambda (ddx ddy rot)
                       (gl-push-matrix)
                       (gl-translate dx dy 0.0)
                       (gl-translate ddx ddy 0.0)
                       (gl-rotate rot 0 0 1)
                       (gl-translate 0.33 0.0 0.35)
                       (gl-scale 0.33 1 0.35)
                       (gl-rotate -90 1 0 0)
                       (door)
                       (gl-pop-matrix))])
                (case dir
                  [(s) (a-door 0 0 0)]
                  [(e) (a-door 1.0 0 90)]
                  [(n) (a-door 0 1.0 0)]
                  [(w) (a-door 0 0 90)]))))))
      
      ;; Switch lighting to ambient
      (send board with-gl-context
            (lambda () 
              (gl-disable 'light0)        
              (gl-light-model-v 'light-model-ambient (gl-float-vector 1.0 1.0 1.0 0.0))))
      
      (define/public (with-gl-context f)
        (send board with-gl-context f))
      
      (define/public (set-wall-image loc wall? door-image)
        (let-values ([(ri rj dir)
                      (cond
                       [(and (list? loc)
                             (= 3 (length loc)))
                        (apply values loc)]
                       [(and (list? loc)
                             (= 2 (length loc)))
                        (let ([i (car loc)]
                              [j (cadr loc)])
                          (if (= 1 (+ (if (integer? i) 1 0)
                                      (if (integer? j) 1 0)))
                              (values (if (integer? i)
                                          i
                                          (add1 (floor (inexact->exact i))))
                                      (if (integer? j)
                                          j
                                          (add1 (floor (inexact->exact j))))
                                      (if (integer? i) 's 'w))
                              (values 0 0 'bad)))]
                       [else (values 0 0 'bad)])])
          (case dir
            [(n s e w) 'ok]
            [else (raise-type-error
                   'set-wall
                   "location"
                   loc)])
          (let* ([i (+ (* 2 ri) (case dir
                                  [(w) 0]
                                  [(n s) 1]
                                  [(e) 2]))]
                 [j (+ (* 2 rj) (case dir
                                  [(n) 2]
                                  [(w e) 1]
                                  [(s) 0]))]
                 [wall (vector-ref (vector-ref walls i) j)]
                 [door-image (if (or (door-image . is-a? . bitmap%) 
                                     (door-image . is-a? . image-snip%))
                                 (bitmap->drawer door-image this)
                                 door-image)]
                 [drawer (if wall?
                             (make-wall-draw ri rj dir 
                                             (if (null? door-image)
                                                 void
                                                 door-image))
                             void)])
            (if (wall-drawer wall)
                (send board set-space-draw wall drawer)
                (send board add-space drawer wall))
            (set-wall-drawer! wall drawer))
          (send board refresh)))

      (define/public (set-room-data loc data)
        (let ([room (vector-ref (vector-ref rooms (car loc)) (cadr loc))])
          (set-room-data! room data)))
      
      (public [new-player make-player-icon])
      (define (new-player drawer data)
        (make-player data drawer #f #f))
      
      (define/public (move-player-icon player loc)
        (let ([i (and loc (car loc))]
              [j (and loc (cadr loc))])
          (let ([from-room (and (player-i player)
                                (vector-ref (vector-ref rooms (player-i player)) (player-j player)))]
                [to-room (and loc (vector-ref (vector-ref rooms i) j))])
            (when from-room
              (set-room-players! from-room (remq player (room-players from-room))))
            (when to-room
              (set-room-players! to-room (cons player (room-players to-room))))
            (set-player-i! player i)
            (set-player-j! player j)
            (when from-room
              (send board remove-piece player))
            (when to-room
              (send board add-piece (+ i 0.5) (+ j 0.5) 0.0 (player-drawer player) player))
            (send board refresh))))
      
      (public [new-thing make-thing-icon])
      (define (new-thing drawer data)
        (make-thing data drawer #f #f #f))
      
      (define/public (move-thing-icon thing loc)
        (let ([i (and (pair? loc) (car loc))]
              [j (and (pair? loc) (cadr loc))]
              [hu? (eq? loc 'heads-up)])
          (let ([from-hu? (thing-heads-up? thing)]
                [from-room (and (thing-i thing)
                                (vector-ref (vector-ref rooms (thing-i thing)) (thing-j thing)))]
                [to-room (and i (vector-ref (vector-ref rooms i) j))])
            (when from-room
              (set-room-things! from-room (remq thing (room-things from-room))))
            (when to-room
              (set-room-things! to-room (cons thing (room-things to-room))))
            (set-thing-i! thing i)
            (set-thing-j! thing j)
            (set-thing-heads-up?! thing hu?)
            (when from-room
              (send board remove-piece thing))
            (when from-hu?
              (send board remove-heads-up thing))
            (when to-room
              (send board add-piece (+ i 0.5) (+ j 0.5) 0.0 (thing-drawer thing) thing)
              (send board enable-piece thing #f))
            (when hu?
              (send board add-heads-up 1.0 1.0 (thing-drawer thing) thing))
            (send board refresh))))

      (super-new))))
