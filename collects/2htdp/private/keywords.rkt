#lang racket

(require (for-syntax "syn-aux.ss") "syn-aux.ss" 
         "syn-aux-aux.ss"
         (rename-in lang/prim (first-order->higher-order f2h)))

(provide (for-syntax AllSpec WldSpec UniSpec))

(define-keywords AllSpec
  ;; -- on-tick must specify a tick handler; it may specify a clock-tick rate
  [on-tick (function-with-arity
            1 
            except
            [(_ f rate) 
             #'(list 
                (proc> 'on-tick (f2h f) 1)
                (num> 'on-tick rate (lambda (x) (and (real? x) (positive? x)))
                      "positive number" "rate"))])]
  ;; -- state specifies whether to display the current state 
  [state (expr-with-check bool> "expected a boolean (show state or not)")]
  ;; -- check-with must specify a predicate 
  [check-with (function-with-arity 1)])

(define-keywords WldSpec 
  ;; -- on-draw must specify a rendering function; it may specify dimensions
  [on-draw to-draw
           (function-with-arity 
            1 
            except
            [(_ f width height)
             #'(list (proc> 'to-draw (f2h f) 1) 
                     (nat> 'to-draw width "width")
                     (nat> 'to-draw height "height"))])]
  ;; -- on-mouse must specify a mouse event handler 
  [on-mouse (function-with-arity 4)]
  ;; -- on-key must specify a key event handler 
  [on-key (function-with-arity 2)]
  ;; -- on-release must specify a release event handler 
  [on-release (function-with-arity 2)]
  ;; -- on-receive must specify a receive handler 
  [on-receive (function-with-arity 2)]
  ;; -- stop-when must specify a predicate; it may specify a rendering function
  [stop-when (function-with-arity 
              1
              except
              [(_ stop? last-picture)
               #'(list (proc> 'stop-when (f2h stop?) 1)
                       (proc> 'stop-when (f2h last-picture) 1))])]
  ;; -- should the session be recorded and turned into PNGs and an animated GIF
  [record? (expr-with-check bool> "expected a boolean (to record? or not)")]
  [name (expr-with-check string> "expected a name (string) for the world")]
  ;; -- register must specify the internet address of a host (e.g., LOCALHOST)
  [register (expr-with-check ip> "expected a host (ip address)")])

(define-keywords UniSpec
  ;; -- on-new must specify what happens when a world joins the universe
  [on-new (function-with-arity 2)]
  ;; -- on-msg must specify what happens to a message from a world 
  [on-msg (function-with-arity 3)]
  ;; -- on-disconnect may specify what happens when a world drops out
  [on-disconnect (function-with-arity 2)]
  ;; -- to-string specifies how to render the universe as a string for display
  [to-string (function-with-arity 1)])