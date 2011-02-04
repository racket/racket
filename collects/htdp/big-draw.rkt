#lang scheme/gui

(require htdp/error
         htdp/draw-sig
         lang/posn
         lang/prim
         mzlib/etc
         mzlib/unit
         mzlib/class
         mred/mred-sig
         mred/mred-unit
         graphics/graphics-sig
         graphics/graphics-posn-less-unit)

(define-values/invoke-unit/infer
  (export graphics^) 
  (link standard-mred@ graphics-posn-less@))

(provide-signature-elements graphics^)

(define-primitive stop stop/proc)

(define-primitive draw-solid-disk draw-solid-disk/proc)
(define-primitive draw-circle draw-circle/proc)
(define-primitive draw-solid-rect draw-solid-rect/proc)
(define-primitive draw-solid-line draw-solid-line/proc)  

(define-primitive clear-solid-disk clear-solid-disk/proc)
(define-primitive clear-circle clear-circle/proc)
(define-primitive clear-solid-rect clear-solid-rect/proc)
(define-primitive clear-solid-line clear-solid-line/proc)  
(define-primitive clear-all clear-all/proc)

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

;; (union #f viewport)
;; the view port for normal operation 
(define @vp #f)

;; boolean 
;; state: are the operations "grouped" into a draw sequence? 
(define in-sequence? #f) 

;; (union #f pixmap) 
;; the pixmap for "grouped" operations 
(define @pm #f)

;; -> (list Viewport Viewport)
(define (get-@VP) (list @vp @pm))

(define the-error (lambda x (error "evaluate (start <num> <num>) first")))
(define-syntax (define-hook stx)
  (syntax-case stx ()
    [(_ name)
     (let* ([stuff (symbol->string (syntax-e (syntax name)))]
            [fools (lambda (x) (datum->syntax #'name (string->symbol x)))]
            [%name (fools (format "%~a" stuff))] ;; works on viewport 
            [proc  (fools (format "~a/proc" stuff))])
       #`(define-values (#,%name #,proc)
           (values the-error (lambda a (apply #,%name a)))))]))

(define-syntax (define-hook-draw/clear stx)
  (syntax-case stx () 
    [(_ name)
     (let* ([stuff (symbol->string (syntax-e (syntax name)))]
            [fools (lambda (x) (datum->syntax #'name (string->symbol x)))]
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

(define-hook stop)

(define (sleep-for-a-while/proc s) (sleep/yield s) #t)

(define-syntax (define-make stx)
  (syntax-case stx ()
    [(_ tag procedure)
     (identifier? (syntax tag))
     (let* ([stuff (symbol->string (syntax-e (syntax tag)))]
            [fools (lambda (x) (datum->syntax stx (string->symbol x)))]
            [make- (fools (format "make-~a" stuff))]
            [name  (fools "name")]
            [ffff  (fools "f")]
            [x     (fools "x")])
       #`(define (#,make- #,name #,ffff)
           (lambda #,x
             (apply procedure #,x)
             #t)))]))

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

;; Semaphore
;; only one world can perform a draw sequence, including a start-up sequence 
(define seq-lock (make-semaphore 1))

(define is-graphics-open? #f)
(define (start WIDTH HEIGHT) (start-and-export WIDTH HEIGHT (make-hash)))
(define (start-and-export WIDTH HEIGHT h)
  (define-syntax setter
    (syntax-rules ()
      [(_ vp* pm* name exp) 
       (begin 
         (set! name 
               (let ([direct (let ([vp* vp*]) exp)][pmap (let ([vp* pm*]) exp)])
                 (lambda a (if in-sequence? (apply pmap a) (apply direct a)))))
         (hash-set! h 'name name))]))
  
  ;; Call after (start ... ...) to collect all the newly created closures 
  (check-arg 'start (and (integer? WIDTH) (> WIDTH 0)) "positive integer" "first" WIDTH)
  (check-arg 'start (and (integer? HEIGHT) (> HEIGHT 0)) "positive integer" "second" HEIGHT)
  (semaphore-wait seq-lock)
  ;; ---
  (unless is-graphics-open?
    (set! is-graphics-open? #t)
    (open-graphics))
  (let* ((tag (symbol->string (gensym)))
         (vpn (string-append "Canvas VP: " tag))
         (pmn (string-append "Canvas PM: " tag))
         (vp* (open-viewport vpn WIDTH HEIGHT))
         (pm* (open-pixmap pmn WIDTH HEIGHT))
         (lbl (lambda () (if in-sequence? pmn vpn)))
         (*delta* 0))
    (hash-set! h 'label lbl)
    (set! @vp vp*)
    (set! @pm pm*)
    ;; --- the following need two versions 
    (setter vp* pm* %clear-all (clear-viewport vp*))    
    (setter vp* pm* %draw-solid-line (make-line 'draw-solid-line (draw-line vp*)))
    (setter vp* pm* %clear-solid-line (make-line 'clear-solid-line (lambda (p1 p2 c) ((clear-line vp*) p1 p2))))
    (setter vp* pm* %draw-solid-rect (make-rect 'draw-solid-rect (draw-solid-rectangle vp*)))
    (setter vp* pm* %clear-solid-rect (make-rect 'clear-solid-rect (lambda (p w h c) ((clear-solid-rectangle vp*) p w h))))
    (setter vp* pm* %draw-solid-disk (make-circle 'draw-solid-disk (draw-solid-ellipse vp*)))
    (setter vp* pm* %clear-solid-disk (make-circle 'clear-solid-disk (lambda (p r1 r2 c) ((clear-solid-ellipse vp*) p r1 r2))))
    (setter vp* pm* %draw-circle (make-circle 'draw-circle (draw-ellipse vp*)))
    (setter vp* pm* %clear-circle (make-circle 'clear-circle (lambda (p r1 r2 c) ((clear-ellipse vp*) p r1 r2))))
    (setter vp* pm* %draw-string (make-%string 'draw-string (lambda (p s) [(draw-string vp*) p s])))
    (setter vp* pm* %clear-string (make-%string 'clear-string (clear-string vp*))) 
    ;; --- 
    (set! %end-of-time
          (let () #;([vp* vp*][pm* pm*])
            (lambda () 
              [(stop-tick vp*)]
              [(stop-tick pm*)] 
              #t)))
    (hash-set! h '%end-of-time %end-of-time)
    ;; ---
    (set! %stop
          (let* ([vp* vp*]
                 [pm* pm*]
                 [a (lambda () (close-viewport vp*) (close-viewport pm*))])
            (lambda ()
              [(stop-tick vp*)]
              [(stop-tick pm*)]
              (if in-sequence? 
                  (set! @end-actions (cons a @end-actions))
                  [a])
              #t)))
    (hash-set! h '%stop %stop)
    ;; --- 
    ;; see ../htdch/draw/support.scm (copy) for explanation and design rationale
    (hash-set! h 'copy (lambda () (set! @vp vp*) (set! @pm pm*) [(clear-viewport pm*)]))
    ;; ---
    ;; --- the following can't happen during a draw sequence --- 
    (set! %wait-for-mouse-click (lambda () (mouse-click-posn (get-mouse-click vp*))))      
    (set! %get-key-event
          (lambda ()
            (cond
              [(ready-key-press vp*) => key-value]
              [else false])))    
    (set! %get-mouse-event
          (lambda ()
            (cond
              [(ready-mouse-click vp*) => mouse-click-posn]
              [else false])))
    (set! %on-key-event
          (lambda (f)
            (check-proc 'on-key-event f 2 'first 'two)
            ((set-on-key-event vp*) 
             (lambda (x y) (f (key-value x) y)))
            #t))
    (set! %on-tick-event
          (lambda (f)
            (let* ([w (ceiling (* 1000 *delta*))]
                   [w (if (exact? w) w (inexact->exact w))])
              (check-proc 'on-key-event f 1 'first 'one)
              ((set-on-tick-event vp*) w (lambda (x) (f x)))
              #t)))
    (set! %big-bang 
          (lambda (delta w)
            (check-arg 'big-bang
                       (and (number? delta) (>= delta 0))
                       "number [of seconds] between 0 and 1000000"
                       "first"
                       delta)
            (set! *delta* delta)
            ((init-world vp*) w) 
            #t))
    
    (semaphore-post seq-lock)
    #t))

;; [Listof (-> Void)]
;; a list of actions to be performed after the drawing action is done. 
(define @end-actions '())

;; Viewport Pixmap -> true
;; start a drawing sequence by clearing the pixmap and making it the "target" for all operations 
;; effect: in-sequence?, @vp and @pm so that copy-viewport can work later 
;; The draw sequence can only draw (and clear) elements from the pixmap. 
;; It doesn't react to events. Should it disable them? 
;; Or do we count on finishing the sequence fast enough?
(define (begin-draw-sequence)
  (semaphore-wait seq-lock)
  (set! in-sequence? #t)
  #t)

;; -> true 
;; stop a drawing sequence and copy the pixmap into the viewport 
;; effect: in-sequence? 
(define (end-draw-sequence)
  (set! in-sequence? #f)
  (copy-viewport @pm @vp)
  (for-each (lambda (th) (th)) @end-actions)
  (set! @end-actions '())
  (semaphore-post seq-lock)
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
     (let ([x (send the-color-database find-color (symbol->string s))])
       (if (rgb? x)
           x
           (error 'draw.ss "The symbol ~e is not a legal color in draw.ss." s))))))
