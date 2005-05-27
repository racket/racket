(module rectangle mzscheme
  (require (lib "error.ss" "htdp")
	   (lib "draw-sig.ss" "htdp")
	   (lib "big-draw.ss" "htdp")
           (lib "unitsig.ss")
           (lib "list.ss")
	   (lib "posn.ss" "lang"))
  
  (provide show)
  
  (define-primitive show show/proc)
  
  ;; do we really need this? Can't they load draw as well instead?
  (provide-signature-elements draw^)
  
  ;; show : rectangle -> #t
  (define (show/proc rect)
    (check-arg 'show
               (and (list? rect) (andmap (lambda (l) (and (list? l) (andmap rgb? l))) rect))
               "rectangle (list of list of colors)" "" rect)
    
    (clear-all)
    
    (let ((x 0) (y 0))
      (for-each (lambda (line)
                  (for-each (lambda (color) 
                              (draw-square x y color)
                              (set! x (+ x LENGTH-SQUARE)))
                            line)
                  (set! x 0)
                  (set! y (+ y LENGTH-SQUARE)))
                rect)
      #t))
  
  ;; could be done by students -- after they learn about accumulators
  (define (show2 rect)
    (let OL ((rect rect) (y 0))
      (cond
        ((null? rect) #t)
        (else (let IL ((line (first rect)) (x 0))
                (cond
                  ((null? line) (void))
                  (else (and (draw-square x y (first line))
                             (IL (rest line) (+ x LENGTH-SQUARE))))))
              (OL (rest rect) (+ y LENGTH-SQUARE))))))
  
  ;; draw-square : number number symbol -> #t
  (define (draw-square y x c)
    (draw-solid-rect (make-posn y x) LENGTH-SQUARE LENGTH-SQUARE c))
  
  (define LENGTH-SQUARE 10))
