(module value-turtles mzscheme
  (require mzlib/math
           mzlib/class
           mred
           (all-except mzlib/list merge)
           mzlib/struct)

  (provide turtles move draw turn turn/radians merge clean turtles?)

  ;; a turtle is:
  ;; - (make-turtle x y theta)
  ;; where x, y, and theta are numbers
  (define-struct turtle (x y angle))
  (define turtle->vector (make-->vector turtle))
  
  (define-struct offset (x y angle))
  (define offset->vector (make-->vector offset))

  ;; a lines is:
  ;;   - (list-of line)
  (define-struct line (x1 y1 x2 y2 black?))
  (define line->vector (make-->vector line))
  
  ;; a turtles is either
  ;;  - (make-tmerge (list-of turtles/offset))
  ;;  - (list-of turtle)
  (define-struct tmerge (turtles))
  (define tmerge->vector (make-->vector tmerge))

  ;; a turtles/offset is
  ;; - (make-turtles/offset turtles offset)
  (define-struct turtles/offset (turtles offset))
  (define turtles/offset->vector (make-->vector turtles/offset))

  (define saved-turtle-snip% #f)
  (define saved-turtles #f)
  (define (vec->struc sexp)
    (cond
      [(pair? sexp) (cons (vec->struc (car sexp)) (vec->struc (cdr sexp)))]
      [(vector? sexp)
       (apply (case (vector-ref sexp 0)
                [(struct:turtle) make-turtle]
                [(struct:offset) make-offset]
                [(struct:line) make-line]
                [(struct:merge) make-tmerge]
                [(struct:turtles/offset) make-turtles/offset]
                [else (error 'vec->struc "unknown structure: ~s\n" sexp)])
              (map vec->struc (vector-ref sexp 1)))]
      [else sexp]))
  
  (define (struc->vec sexp)
    (cond
      [(pair? sexp) (cons (struc->vec (car sexp)) (struc->vec (cdr sexp)))]
      [(turtle? sexp) (vector 'struct:turtle (map struc->vec (vector->list (turtle->vector sexp))))]
      [(offset? sexp) (vector 'struct:offset (map struc->vec (vector->list (offset->vector sexp))))]
      [(line? sexp) (vector 'struct:line (map struc->vec (vector->list (line->vector sexp))))]
      [(tmerge? sexp) (vector 'struct:tmerge (map struc->vec (vector->list (tmerge->vector sexp))))]
      [(turtles/offset? sexp) (vector 'struct:turtles/offset (map struc->vec (vector->list (turtles/offset->vector sexp))))]
      [else sexp]))
  
  (define prim-read read)
  (define prim-write write)

  (define turtle-snip-class%
    (class snip-class% ()
      (define/override (read in-stream)
        (unless saved-turtles
          (error 'turtles "click execute before running the turtles"))
        (let ([str (send in-stream get-string #f)])
          (if (or (not str)
                  (string=? "" str))
              (saved-turtles 150 150)
              (let ([sexp (vec->struc (prim-read (open-input-string str)))])
                (make-object saved-turtle-snip%
                  (first sexp)
                  (second sexp)
                  (third sexp)
                  (fourth sexp)
                  (fifth sexp))))))
      (super-instantiate ())))
  
  (define turtle-snipclass (make-object turtle-snip-class%))
  
  (send turtle-snipclass set-classname "drscheme:turtle-snip")
  (send turtle-snipclass set-version 1)
  (send (get-the-snip-class-list) add turtle-snipclass)
  
  (define pi/2 (/ pi 2))
  (define (set-box/f b v) (when (box? b) (set-box! b v)))
  (define icon-color "PURPLE")
  (define icon-pen (send the-pen-list find-or-create-pen icon-color 1 'xor))
  (define icon-brush (send the-brush-list find-or-create-brush icon-color 'xor))
  (define blank-pen (send the-pen-list find-or-create-pen "BLACK" 1 'transparent))
  (define w-pen (send the-pen-list find-or-create-pen "white" 1 'solid))
  (define b-pen (send the-pen-list find-or-create-pen "black" 1 'solid))
  (define w-brush (send the-brush-list find-or-create-brush "WHITE" 'solid))
  
  (define empty-cache (make-offset 0 0 0))
  
  (define turtle-snip%
    (class snip% 
      (init-field width height turtles cache lines)
      (define/public (get-lines) lines)
      (define/public (get-turtles) turtles)
      (define/public (get-cache) cache)
      (define/public (get-width) width)
      (define/public (get-height) height)
      
      (define turtle-style 'triangle)
      (define bitmap #f)
      
      [define pl (make-object point% 0 0)]
      [define pr (make-object point% 0 0)]
      [define ph (make-object point% 0 0)]
      [define points (list pl pr ph)]
      (define (flip-icons dc dx dy)
        (case turtle-style
          [(triangle line)
           (let* ([proc
                   (if (eq? turtle-style 'line)
                       (lambda (turtle)
                         (let ([x (turtle-x turtle)]
                               [y (turtle-y turtle)]
                               [theta (turtle-angle turtle)]
                               [size 2])
                           (send dc draw-line
                                 (+ dx x)
                                 (+ dy y)
                                 (+ dx x (* size (cos theta)))
                                 (+ dy y (* size (sin theta))))))
                       (lambda (turtle)
                         (let* ([x (turtle-x turtle)]
                                [y (turtle-y turtle)]
                                [theta (turtle-angle turtle)]
                                [long-size 20]
                                [short-size 7]
                                [l-theta (+ theta pi/2)]
                                [r-theta (- theta pi/2)])
                           (send ph set-x (+ dx x (* long-size (cos theta))))
                           (send ph set-y (+ dy y (* long-size (sin theta))))
                           (send pl set-x (+ dx x (* short-size (cos l-theta))))
                           (send pl set-y (+ dy y (* short-size (sin l-theta))))
                           (send pr set-x (+ dx x (* short-size (cos r-theta))))
                           (send pr set-y (+ dy y (* short-size (sin r-theta))))
                           (send dc draw-polygon points))))])
             (if (eq? turtle-style 'line)
                 (send dc set-pen icon-pen)
                 (begin
                   (send dc set-pen blank-pen)
                   (send dc set-brush icon-brush)))
             (for-each proc turtles)
             (send dc set-pen b-pen))]
          [else
           (void)]))
      
      (define (construct-bitmap)
        (unless bitmap
          (flatten)
          (set! bitmap (make-object bitmap% width height))
          (let* ([bitmap-dc (make-object bitmap-dc% bitmap)])
            (send bitmap-dc clear)
            (for-each (lambda (line)
                        (send bitmap-dc set-pen (if (line-black? line) b-pen w-pen))
                        (send bitmap-dc draw-line
                              (line-x1 line)
                              (line-y1 line)
                              (line-x2 line)
                              (line-y2 line)))
                      lines))))
      
      (define/override (draw dc x y left top right bottom dx dy draw-caret)
        (construct-bitmap)
        (let ([old-pen (send dc get-pen)]
              [old-brush (send dc get-brush)]
              [old-clip (send dc get-clipping-region)])
          (send dc set-pen b-pen)
          (send dc set-brush w-brush)
          (send dc draw-rectangle x y (+ width 2) (+ height 2))
          (send dc set-clipping-rect (+ x 1) (+ y 1) width height)
          (send dc draw-bitmap bitmap (+ x 1) (+ y 1))
          (flip-icons dc (+ x 1) (+ y 1))
          (send dc set-pen old-pen)
          (send dc set-brush old-brush)
          (send dc set-clipping-region old-clip)))
      (define/override (get-extent dc x y w h descent space lspace rspace)
        (set-box/f w (+ width 2))
        (set-box/f h (+ height 2))
        (set-box/f descent 0)
        (set-box/f space 0)
        (set-box/f lspace 0)
        (set-box/f rspace 0))
      
      (define/override (copy)
        (make-object turtle-snip% width height turtles cache lines))
      (define/override (write stream-out)
        (let ([p (open-output-string)])
          (prim-write (struc->vec (list width height turtles cache lines)) p)
          (send stream-out put (get-output-string p))))
      
      (define/public (flatten)
        (letrec ([walk-turtles
                  (lambda (turtles offset sofar)
                    (cond
                      [(tmerge? turtles)
                       (let ([turtles/offsets (tmerge-turtles turtles)]
                             [ac (apply-offset cache)])
                         (foldl (lambda (turtles/offset sofar)
                                  (walk-turtles (turtles/offset-turtles turtles/offset)
                                                (combine-offsets
                                                 offset
                                                 (turtles/offset-offset turtles/offset))
                                                sofar))
                                sofar
                                turtles/offsets))]
                      [else
                       (let ([f (apply-offset offset)])
                         (cond
                           [(null? sofar)
                            (map f turtles)]
                           [else
                            (foldl (lambda (t l) (cons (f t) l)) sofar turtles)]))]))])
          (set! turtles (walk-turtles turtles cache null))
          (set! cache empty-cache)))

      (define (move-turtle dist)
        (lambda (turtle)
          (let ([x (turtle-x turtle)]
                [y (turtle-y turtle)]
                [theta (turtle-angle turtle)])
            (make-turtle
             (+ x (* dist (cos theta)))
             (+ y (* dist (sin theta)))
             theta))))

      (define/public (draw-op d)
        (flatten)
        (let ([build-line
               (lambda (turtle)
                 (let ([x (turtle-x turtle)]
                       [y (turtle-y turtle)]
                       [theta (turtle-angle turtle)])
                   (make-line
                    x
                    y
                    (+ x (* d (cos theta)))
                    (+ y (* d (sin theta)))
                    #t)))])
          (make-object turtle-snip%
            width
            height
            (map (move-turtle d) turtles)
            cache
            (foldl (lambda (turtle lines) (cons (build-line turtle) lines))
                   lines
                   turtles))))
      (define/public (merge-op tvs)
        (make-object turtle-snip%
          width
          height
          (make-tmerge (map (lambda (tv) (make-turtles/offset
                                          (send tv get-turtles)
                                          (send tv get-cache)))
                            (cons this tvs)))
          empty-cache
          lines))

      (define/public (move-op n)
        (make-object turtle-snip%
          width
          height
          turtles
          (let* ([angle (offset-angle cache)]
                 [x (offset-x cache)]
                 [y (offset-y cache)]
                 [newx (+ x (* n (cos angle)))]
                 [newy (+ y (* n (sin angle)))])
            (make-offset newx newy angle))
          lines))
      (define/public (turn-op d)
        (make-object turtle-snip%
          width
          height
          turtles
          (make-offset (offset-x cache)
                       (offset-y cache)
                       (- (offset-angle cache)
                          d))
          lines))
      (define/public (clean-op)
        (flatten)
        (make-object turtle-snip%
          width
          height
          null
          empty-cache
          lines))
      (super-instantiate ())
      (inherit set-snipclass)
      (set-snipclass turtle-snipclass)))
  
  (define apply-offset
    (lambda (offset)
      (let ([x (offset-x offset)]
            [y (offset-y offset)]
            [offset-angle (offset-angle offset)])
        (lambda (turtle)
          (let* ([angle (turtle-angle turtle)])
            (let* ([c (cos angle)]
                   [s (sin angle)]
                   [rx (- (* x c) (* y s))]
                   [ry (+ (* y c) (* x s))])
              (make-turtle (+ rx (turtle-x turtle))
                           (+ ry (turtle-y turtle))
                           (+ offset-angle angle))))))))
  
  (define combine-offsets
    (lambda (offset1 offset2)
      (let ([answer ((apply-offset offset1)
                     (make-turtle
                      (offset-x offset2)
                      (offset-y offset2)
                      (offset-angle offset2)))])
        (make-offset
         (turtle-x answer)
         (turtle-y answer)
         (turtle-angle answer)))))
  
  (define turtles
    (case-lambda
      [(width height x y theta)
       (make-object turtle-snip%
         width height
         (list (make-turtle x y theta))
         empty-cache
         null)]
      [(width height)
       (turtles width height 
                (quotient width 2)
                (quotient height 2)
                0)]))
  
  (define (turtles? x) (is-a? x turtle-snip%))
  
  (define (move d tv) (send tv move-op d))
  (define (draw d tv) (send tv draw-op d))
  (define (turn/radians d tv) (send tv turn-op d))
  (define (turn d tv) (turn/radians (* (/ d 360) 2 pi) tv))
  (define (merge tv . tvs) (send tv merge-op tvs))
  (define (clean tv) (send tv clean-op))
  
  (set! saved-turtle-snip% turtle-snip%)
  (set! saved-turtles turtles))
