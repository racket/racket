#lang racket/gui

(define (get-left-side-padding) (+ button-label-inset circle-spacer))
(define button-label-inset 1)
(define black-color (make-object color% "BLACK"))

(define triangle-width 10)
(define triangle-height 14)
(define triangle-color (make-object color% 50 50 50))

(define border-inset 1)
(define circle-spacer 4)
(define rrect-spacer 3)

(provide/contract
 [get-left-side-padding (-> number?)]
 [pad-xywh (-> number? number? (>=/c 0) (>=/c 0)
               (values number? number? (>=/c 0) (>=/c 0)))]
 [draw-button-label
  (->i ([dc (is-a?/c dc<%>)]
        [label (or/c false/c string?)]
        [x number?]
        [y number?]
        [w number?]
        [h (and/c number? (>=/c (* 2 border-inset)))]
        [mouse-over? boolean?]
        [grabbed? boolean?]
        [button-label-font (is-a?/c font%)]
        [bkg-color (or/c false/c (is-a?/c color%) string?)])
       #:pre (w h)
       (w . > . (- h (* 2 border-inset)))
       [result void?])]
 
 [calc-button-min-sizes
  (->* ((is-a?/c dc<%>) string?)
       ((is-a?/c font%))
       (values number? number?))])

(provide name-message%)

(define name-message%
  (class canvas% 
    (init-field [string-constant-untitled "Untitled"]
                [string-constant-no-full-name-since-not-saved 
                 "The file does not have a full name because it has not yet been saved."])
    (inherit popup-menu get-dc get-size get-client-size min-width min-height
             stretchable-width stretchable-height
             get-top-level-window refresh)
  
    (define short-title? #f)
    
    (define hidden? #f)
    (define/public (set-hidden? d?) 
      (unless (eq? hidden? d?)
        (set! hidden? d?)
        (refresh)))
    
    (define allow-to-shrink #f)
    (define/public (set-allow-shrinking w) 
      (unless (eq? w allow-to-shrink)
        (set! allow-to-shrink w)
        (set! to-draw-message #f)
        (update-min-sizes)))
    
    (define paths #f)
    
    ;; label : string
    (init-field [label string-constant-untitled]
                [font small-control-font])
    
    (define/private (get-label)
      (if short-title?
          "/"
          label))
    
    (define full-name-window #f)
    
    (define mouse-grabbed? #f)
    (define mouse-over? #f)
    
    (define/public (on-choose-directory dir)
      (void))
    
    ;; set-message : boolean (union #f path string) -> void
    ;; if file-name? is #t, path-name should be a path (or #f)
    ;; if file-name? is #f, path-name should be a string (or #f)
    (define/public (set-message file-name? path-name)
      (set! paths (if (and file-name? 
                           path-name 
                           (file-exists? path-name))
                      (map path->string (explode-path (simple-form-path path-name)))
                      #f))
      (let ([new-label (cond
                         [(and paths (not (null? paths))) (last paths)]
                         [path-name path-name]
                         [else string-constant-untitled])])
        (unless (equal? label new-label)
          (set! label new-label)
          (set! to-draw-message #f)
          (update-min-sizes)
          (refresh))))
    
    (define/public (set-short-title st?) 
      (set! short-title? st?)
      (set! to-draw-message #f)
      (update-min-sizes)
      (refresh))
    
    (define/public (fill-popup menu reset)
      (if (and paths (not (null? paths)))
          (let loop ([paths (cdr (reverse paths))])
            (cond
              [(null? paths) (void)]
              [else 
               (make-object menu-item% (car paths) menu
                 (lambda (evt item)
                   (reset)
                   (on-choose-directory (apply build-path (reverse paths)))))
               (loop (cdr paths))]))
          (let ([i (make-object menu-item% 
                     string-constant-no-full-name-since-not-saved
                     menu void)])
            (send i enable #f))))
    
    (define/override (on-event evt)
      (unless hidden?
        (let-values ([(max-x max-y) (get-size)])
          (let ([inside? (and (not (send evt leaving?))
                              (<= 0 (send evt get-x) max-x)
                              (<= 0 (send evt get-y) max-y))])
            (unless (eq? inside? mouse-over?)
              (set! mouse-over? inside?)
              (refresh))))
        
        (cond
          [(send evt button-down?)
           (let-values ([(width height) (get-size)]
                        [(reset) (lambda ()
                                   (set! mouse-grabbed? #f)
                                   (set! mouse-over? #f)
                                   (refresh))])
             (set! mouse-over? #t)
             (set! mouse-grabbed? #t)
             (let ([menu (make-object popup-menu% #f
                           (lambda x
                             (reset)))])
               (fill-popup menu reset)
               
               ;; Refresh the screen (wait for repaint)
               (set! paint-sema (make-semaphore))
               (refresh) 
               (yield paint-sema)
               (set! paint-sema #f)
               
               ;; Popup menu
               (popup-menu menu
                           0
                           height)))])))
    
    (define paint-sema #f)
    
    (inherit get-parent)
    (define/private (update-min-sizes)
      (let-values ([(w h) (calc-button-min-sizes (get-dc) (get-label) font)])
        (cond
          [allow-to-shrink
           (cond
             [(< w allow-to-shrink)
              (stretchable-width #f)
              (min-width w)]
             [else
              (stretchable-width #t)
              (min-width allow-to-shrink)])]
          [else
           (min-width w)])
        (min-height h)
        (send (get-parent) reflow-container)))
    
    (define/override (on-paint)
      (when paint-sema
        (semaphore-post paint-sema))
      (unless to-draw-message
        (compute-new-string))
      (let ([dc (get-dc)])
        (let-values ([(w h) (get-client-size)])
          (cond
            [hidden? 
             #;
             (let ([pen (send dc get-pen)]
                   [brush (send dc get-brush)])
               (send dc set-brush (send the-brush-list find-or-create-brush (get-panel-background) 'panel))
               (send dc set-pen (send the-pen-list find-or-create-pen "black" 1 'transparent))
               (send dc draw-rectangle 0 0 w h)
               (send dc set-pen pen)
               (send dc set-brush brush))]
            [else
             (when (and (> w 5) (> h 5))
               (draw-button-label dc to-draw-message 0 0 w h mouse-over? mouse-grabbed? font (get-background-color)))]))))
    
    (define/public (get-background-color) #f)
    
    (define to-draw-message #f)
    (define/private (compute-new-string)
      (let ([label (get-label)])
        (let-values ([(cw ch) (get-client-size)])
          (let ([width-to-use (- cw (get-left-side-padding) triangle-width circle-spacer)])
            (let loop ([c (string-length label)])
              (cond
                [(= c 0) (set! to-draw-message "")]
                [else 
                 (let ([candidate (if (= c (string-length label))
                                      label
                                      (string-append (substring label 0 c) "..."))])
                   (let-values ([(tw th _1 _2) (send (get-dc) get-text-extent candidate small-control-font)])
                     (cond
                       [(tw . <= . width-to-use) (set! to-draw-message candidate)]
                       [else
                        (loop (- c 1))])))]))))))
    
    (define/override (on-size w h)
      (compute-new-string)
      (refresh))
    
    (super-new [style '(transparent no-focus)])
    (update-min-sizes)
    (stretchable-width #f)
    (stretchable-height #f)
    (send (get-dc) set-smoothing 'aligned)))

(define (offset-color color offset-one)
  (make-object color%
    (offset-one (send color red))
    (offset-one (send color green))
    (offset-one (send color blue))))

(define mouse-over-color (case (system-type)
                           [(macosx) "darkgray"]
                           [else (make-object color% 230 230 230)]))
(define mouse-grabbed-color (make-object color% 100 100 100))
(define grabbed-fg-color (make-object color% 220 220 220))

(define (calc-button-min-sizes dc label [button-label-font (send dc get-font)])
  (let-values ([(w h a d) (send dc get-text-extent label button-label-font)])
    (let-values ([(px py pw ph) (pad-xywh 0 0 w h)])
      (values pw ph))))

(define (pad-xywh tx ty tw th)
  (let* ([ans-h
          (+ button-label-inset
             (max 0 
                  (+ 2 (inexact->exact (ceiling th)))
                  (+ 2 triangle-height))
             button-label-inset)]
         [ans-w
          (+ border-inset 
             circle-spacer
             button-label-inset
             (if (eq? (system-type) 'windows) 1 0)  ;; because "(define ...)" has the wrong size under windows
             (max 0 (inexact->exact (ceiling tw)))
             button-label-inset
             triangle-width
             circle-spacer 
             border-inset)])
    (values 
     (- tx (quotient (ceiling (- ans-w tw)) 2))
     (- ty (quotient (ceiling (- ans-h th)) 2))
     ans-w
     ans-h)))

(define (draw-button-label dc label dx dy full-w h mouse-over? grabbed? button-label-font bkg-color)
  
  (define label-width
    (if label
        (let-values ([(w _1 _2 _3) (send dc get-text-extent label button-label-font)])
          w)
        0))
  
  (define w (+ border-inset circle-spacer button-label-inset label-width button-label-inset triangle-width circle-spacer border-inset))

  (when (and bkg-color
             (and (not (or mouse-over? grabbed?))))
    (send dc set-pen bkg-color 1 'solid)
    (send dc set-brush bkg-color 'solid)
    (send dc draw-rectangle dx dy w h))
  
  (when (or mouse-over? grabbed?)
    (let ([color (if grabbed?
                     mouse-grabbed-color
                     mouse-over-color)]
          [xh (- h (* 2 border-inset))])
      (case (system-type)
        [(macosx)
         (send dc set-pen (send the-pen-list find-or-create-pen color 1 'solid))
         (send dc set-brush (send the-brush-list find-or-create-brush color 'solid))
         
         (send dc draw-ellipse (+ dx border-inset) (+ dy border-inset) xh xh)
         (send dc draw-ellipse (+ dx (- w xh)) (+ dy border-inset) xh xh)

         (send dc set-pen (send the-pen-list find-or-create-pen "black" 1 'transparent))
         (send dc draw-rectangle (+ dx (quotient xh 2)) (+ dy border-inset) (- w xh) xh)
         (send dc set-pen (send the-pen-list find-or-create-pen color 1 'solid))
         (send dc draw-line 
               (+ dx (quotient xh 2))
               (+ dy border-inset)
               (+ dx (- w (quotient xh 2)))
               (+ dy border-inset))
         (send dc draw-line 
               (+ dx (quotient xh 2))
               (+ dy (- h 1 border-inset))
               (+ dx (- w (quotient xh 2)))
               (+ dy (- h 1 border-inset)))]
        [else
         (send dc set-pen (send the-pen-list find-or-create-pen triangle-color 1 'solid))
         (send dc set-brush (send the-brush-list find-or-create-brush color 'solid))
         (send dc draw-rounded-rectangle (+ dx rrect-spacer) (+ dy border-inset) (- w border-inset rrect-spacer) xh 2)])))
  
  (when label
    (send dc set-text-foreground (if grabbed? grabbed-fg-color black-color))
    (send dc set-font button-label-font)
    (let-values ([(tw th _1 _2) (send dc get-text-extent label)])
      (send dc draw-text label
            (+ dx (+ border-inset circle-spacer button-label-inset))
            (+ dy (- (/ h 2) (/ th 2)))
            #t)))
  
  (send dc set-pen (send the-pen-list find-or-create-pen 
                         (if grabbed? grabbed-fg-color triangle-color)
                         1 'solid))
  (let ([x (- w triangle-width circle-spacer border-inset)]
        [y (- (/ h 2) (/ triangle-height 2))])
    (let loop ([x-off 0][off-y 5])
      (unless (= 5 x-off)
        (send dc draw-line 
              (+ dx (+ x 1 x-off))
              (+ dy (+ y off-y))
              (+ dx (+ x (- triangle-width 1 x-off)))
              (+ dy (+ y off-y)))
        (loop (+ x-off 1) (+ off-y 1)))))
  
  (void))
