#lang racket/gui

;; ---------------------------------------------------------------------------------------------------
;; the universe library provides the functionality to create interactive and distributed FPs in HtDP

;; DONT USE ___to-draw___ IN THIS FILE 

#| TODO: 
   -- run callbacks in user eventspace
   -- make timer fire just once; restart after on-tick callback finishes
          [on-tick tick-handler tick-producer tick-limit]
          tick-producer: World -> PositiveNumber 
          allow the time span to be a function of the world state 
   -- take out counting; replace by 0.25 delay

   -- make window resizable :: why
|#

(require (for-syntax "private/clauses-spec-and-process.rkt"
                     stepper/private/syntax-property)
         "private/define-keywords.rkt"
         "private/clauses-spec-aux.rkt" 
         ;; ---
         "private/world.rkt"
         "private/universe.rkt"
         "private/universe-image.rkt"
         ;; 
         (only-in "private/launch-many-worlds.rkt" launch-many-worlds launch-many-worlds/proc)
         (only-in "private/stop.rkt" make-stop-the-world)
         (only-in "private/check-aux.rkt" sexp?)
         (only-in "private/pad.rkt" pad-event? pad=?)
         htdp/error
         (rename-in lang/prim (first-order->higher-order f2h)))

(define-primitive stop-with make-stop-the-world)

(provide stop-with) ;; World -> STOP

(provide
 ;; (launch-many-worlds e1 ... e2)
 ;; run expressions e1 through e2 in parallel, produce all values in same order
 launch-many-worlds
 ;; launch-many-worlds/proc : (-> Any) *-> [Listof Any]
 launch-many-worlds/proc
 )

(provide-primitive
 sexp?  ;; Any -> Boolean 
 )

(define new-world (create-world world0))
(define new-universe (create-universe universe0))

(define-keywords AllSpec '() define-all
  ;; -- on-tick must specify a tick handler: World -> World 
  ;; it may specify a clock-tick rate
  [on-tick DEFAULT #'#f
           (function-with-arity
            1
            #:except
            [(_ f rate)
             #'(list
                (proc> 'on-tick (f2h f) 1)
                (num> 'on-tick rate (lambda (x) (and (real? x) (positive? x)))
                      "positive number" "rate"))]
            [(_ f rate limit)
             #'(list
                (proc> 'on-tick (f2h f) 1)
                (num> 'on-tick rate (lambda (x) (and (real? x) (positive? x)))
                      "positive number" "rate")
                (num> 'on-tick limit (lambda (x) (and (integer? x) (positive? x)))
                      "positive integer" "limit"))])]
  ;; -- state specifies whether to display the current state 
  [state DEFAULT #'#f (expr-with-check any> "expected a boolean or a string")]
  ;; Any -> Boolean 
  ;; -- check-with: all states should specify this predicate 
  [check-with DEFAULT #'True (function-with-arity 1)])

;  (create-world world0)
(define-keywords WldSpec AllSpec create-world
  ;; (U #f (World -> Scene) (list (World -> Scene) Nat Nat))
  ;; on-draw must specify a rendering function; 
  ;;   it may specify dimensions
  [on-draw to-draw DEFAULT #'#f
           (function-with-arity
            1
            #:except
            [(_ f width height)
             #'(list (proc> 'to-draw (f2h f) 1)
                     (nat> 'to-draw width "width")
                     (nat> 'to-draw height "height"))])]
  ;; World Nat Nat MouseEvent -> World 
  ;; on-mouse must specify a mouse event handler 
  [on-mouse DEFAULT #f (function-with-arity 4)]
  ;; (U #f (World KeyEvent -> World))
  ;; on-key must specify a key event handler 
  [on-key DEFAULT #f (function-with-arity 2)]
  ;; (U #f (World PadEvent -> World))
  ;; on-pad must specify a pad event handler 
  [on-pad DEFAULT #f (function-with-arity 2)]
  ;; (U #f (World KeyEvent -> World))
  ;; on-release must specify a release event handler 
  [on-release DEFAULT #f (function-with-arity 2)]
  ;; (World S-expression -> World)
  ;; -- on-receive must specify a receive handler 
  [on-receive DEFAULT #'(lambda (w m) w) (function-with-arity 2)]
  ;; World -> Boolean 
  ;; -- stop-when must specify a predicate; it may specify a rendering function
  [stop-when DEFAULT #'False
             (function-with-arity
              1
              #:except
              [(_ stop? last-picture)
               #'(list (proc> 'stop-when (f2h stop?) 1)
                       (proc> 'stop-when (f2h last-picture) 1))])]
  ;; (U #f Any)
  ;; -- should the session be recorded and turned into PNGs and an animated GIF
  ;; -- if the value is a string and is the name of a local directory, use it! 
  [record? DEFAULT #'#f (expr-with-check any> "")]
  ;; (U #f String)
  ;; -- name specifies one string 
  [name DEFAULT #'#f (expr-with-check string-or-symbol> "expected a string")]
  ;; (U #f IP)  
  ;; -- register must specify the internet address of a host (e.g., LOCALHOST)
  [register DEFAULT #'#f (expr-with-check ip> "expected a host (ip address)")])

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


;                                     
;                                     
;                                     
;   ;   ;                  ;        ; 
;   ;   ;                  ;        ; 
;   ;   ;                  ;        ; 
;   ;   ;   ;;;   ; ;;     ;     ;;;; 
;   ;   ;  ;   ;  ;;  ;    ;    ;   ; 
;   ; ; ;  ;   ;  ;   ;    ;    ;   ; 
;   ;; ;;  ;   ;  ;        ;    ;   ; 
;   ;   ;  ;   ;  ;        ;    ;   ; 
;   ;   ;   ;;;   ;        ;;    ;;;; 
;                                     
;                                     
;                                     

(provide 
 big-bang     ;; <syntax> : see below 
 pad-handler  ;; <syntax> : see below 
 )

(provide-primitives
 make-package  ;; World Sexp -> Package
 package?      ;; Any -> Boolean 
 run-movie     ;; [r Positive] [m [Listof Image]] -> true
 ;; run movie m at rate r images per second 
 mouse-event?  ;; Any -> Boolean : MOUSE-EVTS
 mouse=?       ;; MOUSE-EVTS MOUSE-EVTS -> Boolean 
 key-event?    ;; Any -> Boolean : KEY-EVTS
 key=?         ;; KEY-EVTS KEY-EVTS -> Boolean
 pad-event?    ;; KeyEvent -> Boolean
 ;; is the given key-event also a pad-event? 
 pad=?         ;; PadEvent PadEvent -> Boolean 
 ;; ---
 ;; IP : a string that points to a machine on the net 
 )

(provide LOCALHOST     ;; IP
         )

(provide-higher-order-primitive
 run-simulation (create-scene) ; (Nat -> Scene) -> Nat
 )

(provide-higher-order-primitive
 animate (create-scene) ; (Nat -> Scene) -> Nat
 )

(define MOUSE-EVTS 
  '("button-down" 
    "button-up"
    "drag"
    "move"
    "enter"
    "leave"))

(define KEY-EVTS 
  '("left"
    "right"
    "up"
    "down"
    "start"
    "cancel"
    "clear"
    "shift"
    "rshift"
    "control"
    "rcontrol"
    "menu"
    "pause"
    "capital"
    "prior"
    "next"
    "end"
    "home"
    "escape"
    "select"
    "print"
    "execute"
    "snapshot"
    "insert"
    "help"
    "numpad0" "numpad1" "numpad2" "numpad3" "numpad4" 
    "numpad5" "numpad6" "numpad7" "numpad8" "numpad9" 
    "numpad-enter" "multiply" "add" "separator" "subtract" "decimal" "divide"
    "f1" "f2" "f3" "f4" "f5" "f6" "f7" "f8" "f9" "f10" "f11" "f12" "f13" 
    "f14" "f15" "f16" "f17" "f18" "f19" "f20" "f21" "f22" "f23" "f24"
    "numlock"
    "scroll"
    "wheel-up"
    "wheel-down"
    "wheel-left"
    "wheel-right"
    ))

(define-syntax (big-bang stx)
  (define world0 "expects an expression for the initial world and at least one clause")
  (syntax-case stx ()
    [(big-bang) (raise-syntax-error #f world0 stx)]
    [(big-bang w clause ...)
     (let* ([rec? #'#f]
            [->rec? 
             (lambda (kw E)
               (when (free-identifier=? kw #'record?)
                 (syntax-case E ()
                   [(V) (set! rec? #'V)]
                   [_ (err '#'record? stx)])))]
            [args 
             (->args 'big-bang stx #'w #'(clause ...) WldSpec ->rec?)]
            [dom (syntax->list #'(clause ...))])
       (cond
         [(and (not (contains-clause? #'to-draw dom)) (not (contains-clause? #'on-draw dom)))
          (raise-syntax-error #f "expects a [to-draw handler] clause, missing" stx)]
         [else
	   (syntax-property 
	     (stepper-syntax-property
	       #`(run-it ((new-world (if #,rec? aworld% world%)) w #,@args))
	       'stepper-skip-completely #t)
	     'disappeared-use (map (lambda (x) (car (syntax->list x))) dom))]))]))

(define-keywords Pad1Specs '() _init-not-needed
  [up    DEFAULT #'#f (function-with-arity 1)]
  [down  DEFAULT #'#f (function-with-arity 1)]
  [left  DEFAULT #'#f (function-with-arity 1)]
  [right DEFAULT #'#f (function-with-arity 1)]
  [space DEFAULT #'#f (function-with-arity 1)]
  [shift DEFAULT #'#f (function-with-arity 1)])

(define-syntax (pad-handler stx)
  (syntax-case stx ()
    [(pad1 clause ...)
     (let* ([args (->args 'pad-one-player stx #'w #'(clause ...) Pad1Specs void)]
            ; [_ (displayln args)]
            [keys (map (lambda (x) 
                         (syntax-case x () 
                           [(proc> (quote s) _f _d) (symbol->string (syntax-e #'s))]
                           [else "not present"]))
                       (filter values args))]
            [doms (map (lambda (x) (car (syntax->list x))) (syntax->list #'(clause ...)))])
       (syntax-property 
        (stepper-syntax-property
         #`(produce-handler '#,keys (list #,@args))
         'stepper-skip-completely #t)
        'disappeared-use doms))]))

(define (produce-handler keys args)
  (define (check k one other)
    (or (and (string=? k one) (not (member other keys)))
        (and (string=? k other) (not (member one keys)))))
  (define quasi-methods
    (for/fold ((m '())) ((k keys) (a args))
      (cond
        [(check k "w"     "up") (list* (cons "w" a) (cons "up" a) m)]
        [(check k "s"     "down") (list* (cons "s" a) (cons "down" a) m)]
        [(check k "a"     "left") (list* (cons "a" a) (cons "left" a) m)]
        [(check k "d"     "right") (list* (cons "d" a) (cons "right" a) m)]
        [(check k "shift" "rshift") (list* (cons "shift" a) (cons "rshift" a) m)]
        [else (cons (cons "space" a) m)])))
  (define quasi-object (make-immutable-hash quasi-methods))
  (define (the-handler* world key-event)
    ((hash-ref quasi-object key-event (lambda () values)) world))
  the-handler*)

(define (run-simulation f)
  (check-proc 'run-simulation f 1 "first" "one argument")
  (big-bang 0 (on-draw f) (on-tick add1)))

(define animate run-simulation)

(define (run-movie r m*)
  (check-arg 'run-movie (positive? r) "positive number" "first" r)
  (check-arg 'run-movie (list? m*) "list of images" "second" m*)
  (for-each (lambda (m) (check-image 'run-movie m "first" "list of images")) m*)
  (let* ([fst (car m*)]
         [wdt (image-width fst)]
         [hgt (image-height fst)])
    (big-bang 
     m* 
     (on-tick rest r) 
     (on-draw (lambda (m) (if (empty? m) (text "The End" 22 'red) (first m))))
     (stop-when empty?))))

(define (mouse-event? a) (and (string? a) (pair? (member a MOUSE-EVTS))))

(define (mouse=? k m)
  (check-arg 'mouse=? (mouse-event? k) 'MouseEvent "first" k)
  (check-arg 'mouse=? (mouse-event? m) 'MouseEvent "second" m)
  (string=? k m))

(define (key-event? k) 
  (and (string? k) (or (= (string-length k) 1) (pair? (member k KEY-EVTS)))))

(define (key=? k m)
  (check-arg 'key=? (key-event? k) 'KEY-EVTS "first" k)
  (check-arg 'key=? (key-event? m) 'KEY-EVTS "second" m)
  (string=? k m))

(define LOCALHOST "127.0.0.1")

;                                                          
;                                                          
;                                                          
;   ;   ;           ;                                      
;   ;   ;           ;                                      
;   ;   ;                                                  
;   ;   ;  ;;;;     ;    ;   ;   ;;;   ; ;;    ;;;    ;;;  
;   ;   ;  ;   ;    ;    ;   ;  ;   ;  ;;  ;  ;   ;  ;   ; 
;   ;   ;  ;   ;    ;     ; ;   ;;;;;  ;   ;   ;;;   ;;;;; 
;   ;   ;  ;   ;    ;     ; ;   ;      ;          ;  ;     
;   ;   ;  ;   ;    ;      ;    ;   ;  ;      ;   ;  ;   ; 
;    ;;;   ;   ;    ;      ;     ;;;   ;       ;;;    ;;;  
;                                                          
;                                                          
;                                                          

(provide-primitives
 ;; type World 
 iworld?    ;; Any -> Boolean 
 iworld=?   ;; World World -> Boolean 
 iworld-name ;; World -> Symbol 
 ;; type Bundle = (make-bundle [Listof World] Universe [Listof Mail]) 
 ;; type Mail = (make-mail World S-expression)
 make-bundle ;; [Listof World] Universe [Listof Mail] -> Bundle 
 bundle?     ;; is this a bundle? 
 make-mail   ;; World S-expression -> Mail 
 mail?       ;; is this a real mail? 
 )

(provide 
 iworld1    ;; sample worlds 
 iworld2
 iworld3
 universe    ;; <syntax> : see below 
 )

(define-syntax (universe stx)
  (syntax-case stx ()
    [(universe) (raise-syntax-error #f "expects an expression for the initial world" stx)]
    [(universe u) (raise-syntax-error #f "expects at least an on-new and an on-msg clause after the initial world" stx)]
    [(universe u bind ...)
     (let* ([args (->args 'universe stx #'u #'(bind ...) UniSpec void)]
            [dom (syntax->list #'(bind ...))])
       (cond
         [(not (contains-clause? #'on-new dom))
          (raise-syntax-error #f "expects a on-new clause, but found none" stx)]
         [(not (contains-clause? #'on-msg dom))
          (raise-syntax-error #f "expects a on-msg clause, but found none" stx)]
         [else ; (and (memq #'on-new dom) (memq #'on-msg dom))
          (syntax-property 
           #`(run-it ((new-universe universe%) u #,@args))
           'disappeared-use (map (lambda (x) (car (syntax->list x))) dom))]))]))

;                                          
;                                          
;                                          
;      ;               ;;;                 
;                     ;                    
;    ;;;    ;; ;;   ;;;;;;  ;; ;;;   ;;;;  
;      ;     ;;  ;    ;      ;;     ;    ; 
;      ;     ;   ;    ;      ;       ;;;;; 
;      ;     ;   ;    ;      ;      ;    ; 
;      ;     ;   ;    ;      ;      ;   ;; 
;    ;;;;;  ;;; ;;; ;;;;;;  ;;;;;    ;;; ;;
;                                          
;                                          
;                                          
;                                          

;; (-> Object) -> Any
(define (run-it o)
  (define esp (make-eventspace))
  (define thd (eventspace-handler-thread esp))
  (with-handlers ((exn:break? (lambda (x) (break-thread thd))))
    (define obj:ch (make-channel))
    (parameterize ([current-eventspace esp])
      (queue-callback (lambda () (channel-put obj:ch (o)))))
    (send (channel-get obj:ch) last)))
