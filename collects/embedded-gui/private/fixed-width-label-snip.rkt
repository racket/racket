(module fixed-width-label-snip mzscheme
  
  (require
   mzlib/class
   mzlib/list
   mred)
  
  (provide fixed-width-label-snip)
  
  (define (fixed-width-label-snip labels)
    (define label-snip%
      (class snip%
        (inherit set-snipclass)
        (init-field
         label
         (with-border? #f)
         (left-margin 5)
         (right-margin 5)
         (top-margin 5)
         (bottom-margin 5)
         (left-inset 1)
         (top-inset 1)
         (right-inset 1)
         (bottom-inset 1))
        
        (field [font normal-control-font])
        
        (unless (member label labels)
          (error 'fixed-width-label-snip
                 "Instantiation of label-snip expected one of ~s. Given ~s"
                 labels
                 label))
        
        (define (get-string-width dc string)
          (let-values ([(width height baseline vspace)
                        (send dc get-text-extent string font)])
            width))
        
        (define (get-string-height dc string)
          (let-values ([(width height baseline vspace)
                        (send dc get-text-extent string font)])
            height))
        
        (define (get-max-string-width dc strings)
          (foldl
           (lambda (str max-width)
             (max (get-string-width dc str) max-width))
           (get-string-width dc (first strings))
           (rest strings)))
        
        (define (get-max-string-height dc strings)
          (foldl
           (lambda (str max-height)
             (max (get-string-height dc str) max-height))
           (get-string-height dc (first strings))
           (rest strings)))
        
        (define/override (get-extent dc x y w h descent space lspace rspace)
          (let ([width (get-max-string-width dc labels)]
                [height (get-max-string-height dc labels)])
            (when w (set-box! w (+ left-margin width right-margin)))
            (when h (set-box! h (+ top-margin height bottom-margin)))))
        
        (define/override (draw dc x y left top right bottom dx dy draw-caret)
          (super draw dc x y left top right bottom dx dy draw-caret)
          (let ([max-width (get-max-string-width dc labels)]
                [width (get-string-width dc label)]
                [max-height (get-max-string-height dc labels)])
            (let ([f (send dc get-font)])
              (send dc set-font font)
              (send dc draw-text label
                    (+ left-margin x (- max-width width))
                    (+ y top-margin))
              (send dc set-font f))
            (when with-border?
              (let ((w (+ left-margin max-width right-margin))
                    (h (+ top-margin max-height bottom-margin)))
                (send dc draw-lines 
                      (list (make-object point% left-inset top-inset)
                            (make-object point% left-inset (- h bottom-inset))
                            (make-object point% (- w right-inset) (- h bottom-inset))
                            (make-object point% (- w right-inset) top-inset)
                            (make-object point% left-inset top-inset))
                    x 
                    y)))))
        
        ;(define/override (copy)
        ;  (super copy))
        
        (define/override (resize w h) #f)
        
        ;; write ((is-a?/c editor-stream-out%) . -> . void?)
        ;; write the snip out to the stream
        (define/override (write f)
          (send f put label))
        
        (super-new)
        (set-snipclass (new label-snip-class%))))
    
    (define label-snip-class%
      (class snip-class%
        ;; read ((is-a?/c editor-stream-in%) . -> . snip%)
        ;; read a snip from the stream
        (define/override (read f)
          (new label-snip% (label (send f get-string))))
        (super-new)))
    
    (let ([lsc (new label-snip-class%)])
      (send lsc set-classname "...")
      (send lsc set-version 1)
      (send (get-the-snip-class-list) add lsc))
    
    label-snip%)

  ;;;;;;;;;;
  ;; tests
  
  ;(define mylabels (list "Call" "Expected" "Actual"))
  ;(define label% (fixed-width-label-snip mylabels))
  ;(define align? #t)
  ;(define f (new frame% (label "test") (width 175) (height 175)))
  ;(define e (new pasteboard%))
  ;(define c (new editor-canvas% (editor e) (parent f)))
  ;(for-each
  ; (lambda (s)
  ;   (send e insert (new label% (label s))))
  ; '("Expected"))
  ;(send f show #t)
  )
