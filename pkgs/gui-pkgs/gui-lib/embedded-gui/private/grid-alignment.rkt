#| Not yet functional |#

(module grid-alignment mzscheme
  (define grid-alignment #f)
  (provide grid-alignment))

#;(module grid-alignment mzscheme
  
  (require
   mzlib/class
   mred
   mzlib/etc
   mzlib/list
   mzlib/match
   (prefix a: "alignment.rkt")
   mrlib/click-forwarding-editor
   
   "on-show-pasteboard.rkt"
   "really-resized-pasteboard.rkt"
   "interface.rkt"
   "snip-lib.rkt"
   "alignment-helpers.rkt")
  
  (provide grid-alignment%)
  
  ;; totally broken and not up to date with last revision
  (define grid-alignment%
    (class* object% (#;alignment<%>)
      (init-field
       columns
       (parent false))
      (field
       [pasteboard false]
       [rows empty]
       [row-heights 0]
       [column-widths 0]
       [show? true])
      
      ;; need base class for this method
      (define (show/hide-child child show?)
        (if (is-a? child alignment<%>)
            (send child show show?)
            (if show?
                (send pasteboard insert child)
                (send pasteboard release-snip child))))
      
      (define/public (add row)
        (set! rows (append rows (list row)))
        (unless (= (vector-length row) columns)
          (error 'add "Invalid number of rows"))
        (send pasteboard lock-alignment true)
        (let loop ([column 0])
          (unless (>= column columns)
            (let ([child (vector-ref row column)])
              (cond
                [(is-a? child snip%)
                 (when show?
                   (send pasteboard insert child false))]
                [(is-a? child alignment<%>)
                 (send child set-pasteboard pasteboard)])
              (loop (add1 column)))))
        (send pasteboard lock-alignment false))
      
      (define/public (set-min-sizes)
        
        (set! column-widths
              (map
               (lambda (column)
                 (apply vacuous-max
                        (map (lambda (row)
                               (child-width
                                (vector-ref row column)))
                             rows)))
               (build-list columns identity)))
        
        (set! row-heights
              (map
               (lambda (row)
                 (apply vacuous-max
                        (map (lambda (column)
                               (child-height
                                (vector-ref row column)))
                             (build-list columns identity))))
               rows)))
      
      ;; STATUS: This function currently doesn't stretch snips.
      (define/public (align x-offset y-offset width height)
          (define (align-row row init-x y)
            (let xloop ([x init-x]
                        [column 0]
                        [widths column-widths])
              (unless (or (>= column columns) (empty? widths))
                (move-child (vector-ref row column) pasteboard x y)
                (xloop (+ x (first widths))
                       (add1 column)
                       (rest widths)))))
          (when show?
            (let yloop ([y y-offset]
                        [the-rows rows]
                        [heights row-heights])
              (unless (or (empty? the-rows) (empty? heights))
                (align-row (first the-rows) x-offset y)
                (yloop (+ y (first heights))
                       (rest the-rows)
                       (rest heights))))))
      
      (define/public (get-min-width)
        (if show?
            (apply + column-widths)
            0))
      (define/public (get-min-height)
        (if show?
            (apply + row-heights)
            0))
      
      (define/public (show bool)
        (define (show/hide-row row)
          (let loop ([column 0])
            (unless (>= column columns)
              (let ([child (vector-ref row column)])
                (show/hide-child child bool)
                (loop (add1 column))))))
        (unless (boolean=? bool show?)
          (set! show? bool)
          (send pasteboard lock-alignment true)
          (for-each show/hide-row rows)
          (send pasteboard lock-alignment false)))
      
      (define/public (stretchable-width?) false)
      (define/public (stretchable-height?) false)
      (define/public (set-pasteboard pb) (set! pasteboard pb))
      
      (super-new)
      (when parent (send parent add this))))
  
  (define (move-child child pasteboard x y)
    (cond
      [(is-a? child snip%)
       (send pasteboard move-to child x y)]
      [(is-a? child alignment<%>)
       (send child align x y)]))
  )
