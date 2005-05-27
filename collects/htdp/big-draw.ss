#cs
(module big-draw mzscheme
  (require "error.ss"
           "draw-sig.ss"
           (lib "etc.ss")
           (lib "posn.ss" "lang")
           (lib "prim.ss" "lang")           
           (lib "unitsig.ss")
           (prefix mred: (lib "mred.ss" "mred"))
           (lib "class.ss")
           (lib "mred-sig.ss" "mred")
           (lib "graphics-sig.ss" "graphics")
           (lib "graphics-posn-less-unit.ss" "graphics"))
  
  (define-values/invoke-unit/sig graphics:posn-less^
                                 graphics-posn-less@ #f 
                                 (mred : mred^)
                                 graphics:posn^)
  
  (provide-signature-elements graphics:posn-less^)
  
  (define-primitive draw-solid-disk draw-solid-disk/proc)
  (define-primitive draw-circle draw-circle/proc)
  (define-primitive draw-solid-rect draw-solid-rect/proc)
  (define-primitive draw-solid-line draw-solid-line/proc)  
  
  (define-primitive clear-solid-disk clear-solid-disk/proc)
  (define-primitive clear-circle clear-circle/proc)
  (define-primitive clear-solid-rect clear-solid-rect/proc)
  (define-primitive clear-solid-line clear-solid-line/proc)  
  (define-primitive clear-all clear-all/proc)
  
  ;   (provide draw-solid-string clear-solid-string)
  
  (define-primitive draw-solid-string draw-string/proc)
  (define-primitive clear-solid-string clear-string/proc)
  
  (define-primitive sleep-for-a-while sleep-for-a-while/proc)
  (define-primitive wait-for-mouse-click wait-for-mouse-click/proc)
  (define-primitive get-mouse-event get-mouse-event/proc)
  (define-primitive get-key-event get-key-event/proc)  
  
  (define-higher-order-primitive on-key-event on-key-event/proc (handle-event))
  (define-higher-order-primitive on-tick-event on-tick-event/proc (handle-tick))
  (define-primitive big-bang big-bang/proc)
  (define-primitive end-of-time end-of-time/proc)
  
  (define the-error (lambda x (error "evaluate (start <num> <num>) first")))
  (define-syntax (define-hook stx)
    (syntax-case stx ()
      [(_ name)
       (let* ([stuff (symbol->string (syntax-e (syntax name)))]
              [fools (lambda (x) (datum->syntax-object #'name (string->symbol x)))]
              [%name (fools (format "%~a" stuff))]
              [proc  (fools (format "~a/proc" stuff))])
         #`(define-values (#,%name #,proc)
             (values the-error
                     (lambda a (apply #,%name a)))))]))
  
  (define-syntax (define-hook-draw/clear stx)
    (syntax-case stx () 
      [(_ name)
       (let* ([stuff (symbol->string (syntax-e (syntax name)))]
              [fools (lambda (x) (datum->syntax-object #'name (string->symbol x)))]
              [clear (fools (format "clear-~a" stuff))]
              [draw  (fools (format "draw-~a" stuff))])
         #`(begin
             (define-hook #,clear)
             (define-hook #,draw)))]))
  
  (define-hook-draw/clear solid-disk) 
  (define-hook-draw/clear circle)
  (define-hook-draw/clear string)
  (define-hook-draw/clear solid-rect)
  (define-hook-draw/clear solid-line)  
  
  (define-hook clear-all)
  
  (define-hook get-key-event)
  (define-hook get-mouse-event)
  (define-hook wait-for-mouse-click)
  
  (define-hook big-bang)     
  (define-hook on-key-event)
  (define-hook on-tick-event)
  (define-hook end-of-time)
  
  (define (make-true f) (lambda x (apply f x) #t))
  (define sleep-for-a-while/proc (make-true mred:sleep/yield))
  
  (define-syntax (define-make stx)
    (syntax-case stx ()
      [(_ tag procedure)
       (identifier? (syntax tag))
       (let* ([stuff (symbol->string (syntax-e (syntax tag)))]
              [fools (lambda (x) (datum->syntax-object stx (string->symbol x)))]
              [make- (fools (format "make-~a" stuff))]
              [name  (fools "name")]
              [ffff  (fools "f")]
              [x     (fools "x")])
         #`(define (#,make- #,name #,ffff)
             (make-true (lambda #,x (apply procedure #,x)))))]))
  
  (define-make line 
    (lambda (p1 p2 . c)
      (check-arg name (posn? p1) "posn" "first" p1)
      (check-arg name (posn? p2) "posn" "second" p2)
      (f p1 p2 (check-optional name 3 c "third" x))))
  
  (define-make rect
    (lambda (p w h . c)
      (check-arg name (posn? p) "posn" "first" p)
      (check-arg name (and (integer? w) (> w 0)) "positive integer" "second" w)
      (check-arg name (and (integer? h) (> h 0)) "positive integer" "third" h)
      (f p w h (check-optional name 4 c "fourth" x))))
  
  (define-make %string 
    (lambda (p s)
      (check-arg name (posn? p) "posn" "first" p)
      (check-arg name (string? s) "string" "second" s)
      (f p s)))
  
  (define-make circle
    (lambda (p r . c)
      (check-arg name (posn? p) "posn" "first" p)
      (check-arg name (and (integer? r) (> r 0)) "positive integer" "second" r)
      ((ellipsis-2-circle f) p r (check-optional name 3 c "third" x))))
  
  ;; Local function for make-circle 
  ;; (Posn Number Symbol[color] -> void) -> (Posn Number Symbol[color] -> void)
  (define (ellipsis-2-circle f)
    (lambda (p r c)
      (let ((d (* r 2)))
        (f (make-posn (- (posn-x p) r) (- (posn-y p) r)) d d c))))
  
  ;; (Listof _) String (Listof _) -> Symbol[color]
  ;; contract: c is shared suffix of all
  ;; check whether c contains a single color symbol and all has proper length
  (define (check-optional name n c position x)
    (if (pair? c)
        (begin
          (check-arity name n x)
          (check-arg name (symbol? (car c)) "symbol" position (car c)))
        (check-arity name (- n 1) x))
    (symbol->color (if (null? c) 'black (car c))))
  
  (define (start WIDTH HEIGHT)
    (check-arg 'start (and (integer? WIDTH) (> WIDTH 0)) "positive integer" "first" WIDTH)
    (check-arg 'start (and (integer? HEIGHT) (> HEIGHT 0)) "positive integer" "second" HEIGHT)
    ;; --- 
    (open-graphics)
    (let ((current-window (open-viewport "Canvas" WIDTH HEIGHT))
          (*delta* 0))
      (set! @vp current-window)
      (set! %clear-all (clear-viewport current-window))
      
      (set! %draw-solid-line
            (make-line 'draw-solid-line (draw-line current-window)))
      
      (set! %clear-solid-line
            (make-line 'clear-solid-line
                       (lambda (p1 p2 c)
                         ((clear-line current-window) p1 p2))))
      
      (set! %draw-solid-rect (make-rect 'draw-solid-rect (draw-solid-rectangle current-window)))
      (set! %clear-solid-rect
            (make-rect 'clear-solid-rect
                       (lambda (p w h c)
                         ((clear-solid-rectangle current-window) p w h))))
      
      (set! %draw-solid-disk (make-circle 'draw-solid-disk (draw-solid-ellipse current-window)))
      (set! %clear-solid-disk
            (make-circle 'clear-solid-disk
                         (lambda (p r1 r2 c)
                           ((clear-solid-ellipse current-window) p r1 r2))))
      
      (set! %draw-circle (make-circle 'draw-circle (draw-ellipse current-window)))
      (set! %clear-circle
            (make-circle 'clear-circle
                         (lambda (p r1 r2 c)
                           ((clear-ellipse current-window) p r1 r2))))
      
      
      (set! %draw-string (make-%string 'draw-string (draw-string current-window)))
      (set! %clear-string (make-%string 'clear-string (clear-string current-window)))
      
      
      (set! %wait-for-mouse-click
            (lambda ()
              (mouse-click-posn
               (get-mouse-click @vp))))
      
      (set! %get-key-event
            (lambda ()
              (cond
                [(ready-key-press @vp) => key-value]
                [else false])))
      
      (set! %on-key-event
            (lambda (f)
              (check-proc 'on-key-event f 2 'first 'two)
              ((set-on-key-event @vp) 
               (lambda (x y) (f (key-value x) y)))
              #t))
      
      (set! %on-tick-event
            (lambda (f)
              (let* ([w (ceiling (* 1000 *delta*))]
                     [w (if (exact? w) w (inexact->exact w))])
                (check-proc 'on-key-event f 1 'first 'one)
                ((set-on-tick-event @vp) w f)
                #t)))
      
      (set! %big-bang 
            (lambda (delta w)
              (check-arg 'big-bang
                         (and (number? delta) (>= delta 0))
                         "number [of seconds] between 0 and 1000000"
                         "first"
                         delta)
              (set! *delta* delta)
              ((init-world @vp) w) #t))
      
      (set! %end-of-time (lambda () ((stop-tick @vp))))
      
      (set! %get-mouse-event
            (lambda ()
              (cond
                [(ready-mouse-click @vp) => mouse-click-posn]
                [else false])))
      #t))
  
  (define (stop)
    (close-graphics)
    (set! @vp #f)
    (set! %clear-all the-error)
    
    (set! %draw-solid-line the-error)
    (set! %clear-solid-line the-error)
    
    (set! %draw-solid-rect the-error)
    (set! %clear-solid-rect the-error)
    
    (set! %draw-solid-disk the-error)
    (set! %clear-solid-disk the-error)
    
    (set! %draw-circle the-error)
    (set! %clear-circle the-error)
    
    (set! %wait-for-mouse-click the-error)
    
    (set! %get-key-event the-error)
    (set! %on-key-event the-error)
    (set! %big-bang the-error)
    
    (set! %get-mouse-event the-error)
    #t)
  
  
  
  ;; start/cartesian-plane : Number Number -> true
  ;; start up a canvas of size width x height  and draw a centered cartesian coordinate
  (define (start/cartesian-plane width height)
    (check-arg 'start/cartesian-plane
               (and (integer? width) (> width 0)) "positive integer" "first" width)
    (check-arg 'start/cartesian-plane
               (and (integer? height) (> height 0)) "positive integer" "second" height)    
    (local ((define trash  (start width height))
            (define mid-x  (quotient width 2))
            (define mid-y  (quotient height 2)))
      (and (draw-solid-line (make-posn mid-x 0) (make-posn mid-x height))
           (draw-solid-line (make-posn 0 mid-y) (make-posn width mid-y)))))
  
  (define @vp #f)
  #cs(define (get-@VP) @vp)
  
  (provide-signature-elements draw^)
  
  ;; symbol->color : symbol -> color
  ;; to convert symbol to 
  (define (symbol->color s)
    (check-arg 'draw.ss (symbol? s) "symbol" "first" s)
    (case s 
      ((white)   (make-rgb 1 1 1))
      ((yellow)  (make-rgb 1 1 0))
      ((red)     (make-rgb 1.0 0 0))
      ((green)   (make-rgb 0 1.0 0))
      ((blue)    (make-rgb 0 0 1.0))
      ((black)   (make-rgb 0 0 0))
      (else
       (let ([x (send mred:the-color-database find-color (symbol->string s))])
         (if (rgb? x)
             x
             (error 'draw.ss "The symbol ~e is not a legal color in draw.ss." s)))))))