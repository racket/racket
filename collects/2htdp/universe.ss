#lang scheme/gui

#| TODO: 
   -- yield instead of sync
   -- run callbacks in user eventspace
   -- make timer fire just once; restart after on-tick callback finishes
   -- take out counting; replace by 0.25 delay

   -- make window resizable :: why
   -- what if clauses are repeated in world and/or universe descriptions? 
   -- what if the initial world or universe state is omitted? the error message is bad then. 
|#

(require (for-syntax "private/syn-aux.ss" scheme/function)
         "private/syn-aux-aux.ss" 
         "private/syn-aux.ss"
         "private/check-aux.ss"
         "private/universe-image.ss"
         "private/world.ss"
         "private/universe.ss"
         "private/launch-many-worlds.ss"
         "private/stop.ss"
         htdp/error
         (rename-in lang/prim (first-order->higher-order f2h)))

(define-primitive stop-with make-stop-the-world)

(provide stop-with) ;; World -> STOP

(provide
 launch-many-worlds
 ;; (launch-many-worlds e1 ... e2)
 ;; run expressions e1 through e2 in parallel, produce all values in same order
 )

(provide-primitive
 sexp?  ;; Any -> Boolean 
 )

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

(provide big-bang     ;; <syntax> : see below 
         )

(provide-primitives
         make-package ;; World Sexp -> Package
         package?     ;; Any -> Boolean 
         run-movie    ;; [Listof Image] -> true 
         mouse-event?  ;; Any -> Boolean : MOUSE-EVTS
         mouse=?       ;; MOUSE-EVTS MOUSE-EVTS -> Boolean 
         key-event?    ;; Any -> Boolean : KEY-EVTS
         key=?         ;; KEY-EVTS KEY-EVTS -> Boolean
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
    "control"
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
    "wheel-down"))

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

(define-syntax (big-bang stx)
  (define world0 "big-bang needs at least an initial world")
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
             (->args 'big-bang stx (syntax (clause ...)) AllSpec WldSpec ->rec? "world")])
       #`(let* ([esp (make-eventspace)]
                [thd (eventspace-handler-thread esp)])
           (with-handlers ((exn:break? (lambda (x) (break-thread thd))))
             (parameterize ([current-eventspace esp])
               (let ([o (new (if #,rec? aworld% world%) [world0 w] #,@args)])
                 (send o last))))))]))

(define (run-simulation f)
  (check-proc 'run-simulation f 1 "first" "one argument")
  (big-bang 1 (on-draw f) (on-tick add1)))

(define animate run-simulation)

(define (run-movie r m*)
  (check-arg 'run-movie (positive? r) "positive number" "first" r)
  (check-arg 'run-movie (list? m*) "list (of images)" "second" m*)
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

(define-keywords UniSpec
  ;; -- on-new must specify what happens when a world joins the universe
  [on-new (function-with-arity 2)]
  ;; -- on-msg must specify what happens to a message from a world 
  [on-msg (function-with-arity 3)]
  ;; -- on-disconnect may specify what happens when a world drops out
  [on-disconnect (function-with-arity 2)]
  ;; -- to-string specifies how to render the universe as a string for display
  [to-string (function-with-arity 1)])

(define-syntax (universe stx)
  (define legal "not a legal clause in a universe description")
  (syntax-case stx ()
    [(universe) (raise-syntax-error #f "not a legal universe description" stx)]
    [(universe u) (raise-syntax-error #f "not a legal universe description" stx)]
    [(universe u bind ...)
     (let*
         ([args (->args 'universe stx (syntax (bind ...)) AllSpec UniSpec void "universe")]
          [domain (map (compose syntax-e car) args)])
       (cond
         [(not (memq 'on-new domain))
          (raise-syntax-error #f "missing on-new clause" stx)]
         [(not (memq 'on-msg domain))
          (raise-syntax-error #f "missing on-msg clause" stx)]
         [else ; (and (memq #'on-new domain) (memq #'on-msg domain))
          #`(let* ([esp (make-eventspace)]
                   [thd (eventspace-handler-thread esp)])
              (with-handlers ((exn:break? (lambda (x) (break-thread thd))))
                (parameterize ([current-eventspace esp])
                  (send (new universe% [universe0 u] #,@args) last))))]))]))

