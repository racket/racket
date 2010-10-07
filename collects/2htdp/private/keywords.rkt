#lang racket

(require (for-syntax "syn-aux.ss")
         "syn-aux.ss" 
         "syn-aux-aux.ss"
         "check-aux.rkt"
         (only-in "universe.rkt" make-bundle)
         (rename-in lang/prim (first-order->higher-order f2h)))

(provide [for-syntax WldSpec UniSpec])

(define-keywords AllSpec '() define-all
  ;; -- on-tick must specify a tick handler; it may specify a clock-tick rate
  [on-tick   
   DEFAULT #'#f
   (function-with-arity
    1 
    except
    [(_ f rate) 
     #'(list 
        (proc> 'on-tick (f2h f) 1)
        (num> 'on-tick rate (lambda (x) (and (real? x) (positive? x)))
              "positive number" "rate"))])]
  ;; -- state specifies whether to display the current state 
  [state
   DEFAULT #'#f
   (expr-with-check bool> "expected a boolean (show state or not)")]
  ;; -- check-with must specify a predicate 
  [check-with
   DEFAULT #'True
   (function-with-arity 1)])

;  (create-world world0)
(define-keywords WldSpec AllSpec create-world
  ;; -- on-draw must specify a rendering function; it may specify dimensions
  [on-draw to-draw
           DEFAULT #'#f
           (function-with-arity 
            1 
            except
            [(_ f width height)
             #'(list (proc> 'to-draw (f2h f) 1) 
                     (nat> 'to-draw width "width")
                     (nat> 'to-draw height "height"))])]
  ;; -- on-mouse must specify a mouse event handler 
  [on-mouse
   DEFAULT #'K
   (function-with-arity 4)]
  ;; -- on-key must specify a key event handler 
  [on-key
   DEFAULT #'K
   (function-with-arity 2)]
  ;; -- on-release must specify a release event handler 
  [on-release
   DEFAULT #'K
   (function-with-arity 2)]
  ;; -- on-receive must specify a receive handler 
  [on-receive
   DEFAULT #'#f
   (function-with-arity 2)]
  ;; -- stop-when must specify a predicate; it may specify a rendering function
  [stop-when
   DEFAULT #'False
   (function-with-arity 
    1
    except
    [(_ stop? last-picture)
     #'(list (proc> 'stop-when (f2h stop?) 1)
             (proc> 'stop-when (f2h last-picture) 1))])]
  ;; -- should the session be recorded and turned into PNGs and an animated GIF
  [record?
   DEFAULT #'#f
   (expr-with-check bool> "expected a boolean (to record? or not)")]
  [name
   DEFAULT #'#f
   (expr-with-check string> "expected a name (string) for the world")]
  ;; -- register must specify the internet address of a host (e.g., LOCALHOST)
  [register
   DEFAULT #'#f
   (expr-with-check ip> "expected a host (ip address)")])

;  (create-universe universe0)
(define-keywords UniSpec AllSpec create-universe
  ;; -- on-new must specify what happens when a world joins the universe
  [on-new 
   DEFAULT #'"my-bad"
   (function-with-arity 2)]
  ;; -- on-msg must specify what happens to a message from a world 
  [on-msg
   DEFAULT #'"my-bad"
   (function-with-arity 3)]
  ;; -- on-disconnect may specify what happens when a world drops out
  [on-disconnect
   ;; ******************************************************************
   DEFAULT #'(lambda (u w) (make-bundle u '() '()))
   ;; this is the wrong default function 
   ;; instead of K there should be a function that produces a bundle 
   (function-with-arity 2)
   ;; ******************************************************************
   ]
  ;; -- to-string specifies how to render the universe as a string for display
  [to-string
   DEFAULT #'#f
   (function-with-arity 1)])

(provide new-world)
(define new-world (create-world world0))

(provide new-universe)
(define new-universe (create-universe universe0))