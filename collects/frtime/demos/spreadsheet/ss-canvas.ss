(module ss-canvas (lib "frtime.ss" "frtime")
  
  (require 
          
           (lib "class.ss")
           (lib "list.ss" "frtime")
          
           (all-except (lib "mred.ss" "mred") send-event)
           (lib "mixin-macros.ss" "frtime" "demos" "gui")
           )
  (require (rename (lib "frp-core.ss" "frtime") super-lift super-lift))
 
  
  
  (define-struct line (vert? x y len))
  (define-struct text-disp (x y string))
  (define-struct select-box (x y w h))

  
  (define (draw-line a-line dc)
    (let ([vert? (line-vert? a-line)]
          [x (line-x a-line)]
          [y (line-y a-line)]
          [len (line-len a-line)])
      (send dc draw-line 
            x
            y
            (if vert?
                x
                (+ x len))
            (if vert?
                (+ y len)
                y))))
  
  (define (draw-text a-text dc)
    (send dc draw-text 
          (text-disp-string a-text)
          (text-disp-x a-text)
          (text-disp-y a-text)))
  
  (define (draw-select-box a-sb dc)
    (let ([b (send dc get-brush)])
      (send dc set-brush "lightsteelblue" 'opaque)
      (send dc draw-rectangle
            (select-box-x a-sb)
            (select-box-y a-sb)
            (select-box-w a-sb)
            (select-box-h a-sb))
      (send dc set-brush b)))
    
  
  (define spread-canvas%
    (class ((callbacks->args-evts scroll-events
                           on-scroll
                           (s-evt))
            canvas%)
      (init (grid-lines '()) (content '()) (select-area '()))
      (inherit get-dc)
      (super-new (scroll-events-event-processor
                  (lambda (es)
                    (split (map-e car es) (lambda (e) (send e get-direction))))))
      
      (define text-values content)
      (define grid grid-lines)
      (define selection select-area)
      
      (define offscreen-dc (new bitmap-dc% (bitmap (make-object bitmap% 1280 1024 #f))))
      
      (for-each-e! (merge-e (changes text-values)
                            (changes selection))
                   (lambda (_) (on-paint))
                   this)
      
      (define/override (on-paint)
        (let ([texts (value-now text-values)]
              [select-bx (value-now selection)])
          
          (send offscreen-dc clear)
          (send offscreen-dc set-pen "black" 1 'solid)
          
          (for-each
           (lambda (s)
             (draw-select-box s offscreen-dc))
           select-bx)
          
          (for-each 
           (lambda (l)
             (draw-line l offscreen-dc))
           grid)
          
          (for-each
           (lambda (t)
             (draw-text t offscreen-dc))
           texts)
          
          
                      
          (send (get-dc) draw-bitmap (send offscreen-dc get-bitmap) 0 0)))
      
       (define all-mouse (event-receiver))
       
       (define (harvest-mouse getter match)
         (map-e (lambda (evt)
                  (getter evt))
                (filter-e
                 (lambda (evt)
                   (let ([type (send evt get-event-type)])
                     (ormap (lambda (x) (eq? x type)) match)))
                 all-mouse)))
       
       
       (define identity (lambda (x) x))
       
       (define mouse-x-e (harvest-mouse (lambda (e) (send e get-x)) '(enter motion)))
       (define mouse-x-b (hold mouse-x-e))
       (define mouse-y-e (harvest-mouse (lambda (e) (send e get-y)) '(enter motion)))
       (define mouse-y-b (hold mouse-y-e))
       (define l-clicks-e (harvest-mouse identity '(left-down)))
       (define m-clicks-e (harvest-mouse identity '(middle-down)))
       (define r-clicks-e (harvest-mouse identity '(right-down)))
       (define l-release-e (harvest-mouse identity '(left-up)))
       (define m-release-e (harvest-mouse identity '(middle-up)))
       (define r-release-e (harvest-mouse identity '(right-up)))
       (define l-down? (hold (merge-e (map-e (lambda (e) #t) l-clicks-e)
                                      (map-e (lambda (e) #f) l-release-e))
                             #f))
       
       (define/override (on-subwindow-event a-window event)
         (begin
           (send-event all-mouse event)
           (super on-subwindow-event a-window event))
         #;(begin
             (case (send event get-event-type)
               [(enter motion)
                (send-event mouse-x-e (send event get-x))
                (send-event mouse-y-e (send event get-y))]
               [(left-down)
                (send-event l-clicks-e event)]
               [(middle-down)
                (send-event m-clicks-e event)]
               [(right-down)
                (send-event r-clicks-e event)])
             (super on-subwindow-event a-window event)))
       
       (define/public (get-mouse-x) mouse-x-b)
       (define/public (get-mouse-y) mouse-y-b)
       (define/public (get-l-clicks) l-clicks-e)
       (define/public (get-m-clicks) m-clicks-e)
       (define/public (get-r-clicks) r-clicks-e)
       (define/public (get-all-clicks) (merge-e l-clicks-e
                                                m-clicks-e
                                                r-clicks-e))
       (define/public (get-l-down?) l-down?)
      
      ))
  
  
  
  
  
  (define-struct posn (x y))
  (define-struct animation (pic pos))
  
  (provide (all-defined))
  )