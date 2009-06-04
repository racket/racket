#lang scheme/gui

#| TODO: 
   -- make window resizable :: why
|#

(require (for-syntax "private/syn-aux.ss")
         "private/syn-aux-aux.ss" 
         "private/syn-aux.ss"
         "private/check-aux.ss"
         "private/image.ss"
         "private/world.ss"
         "private/universe.ss"
         htdp/error
         (rename-in lang/prim (first-order->higher-order f2h))
         (only-in mzlib/etc evcase))

(provide (all-from-out "private/image.ss"))

(provide
 sexp?  ;; Any -> Boolean 
 scene? ;; Any -> Boolean 
 )

;; Spec = (on-tick Expr) 
;;      | (on-tick Expr Expr) 
;; -- on-tick must specify a tick handler; it may specify a clock-tick rate

(define-keywords AllSpec
  [on-tick (function-with-arity
            1 
            except
            [(x rate) 
             #'(list (proc> 'on-tick (f2h x) 1) 
                     (num> 'on-tick rate positive? "pos. number" "rate"))])])

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
         make-package ;; World Sexp -> Package
         package?     ;; Any -> Boolean 
         run-movie    ;; [Listof Image] -> true 
         
         ;; A MouseEventType is one of:
         ;; - 'button-down
         ;; - 'button-up
         ;; - 'drag
         ;; - 'move
         ;; - 'enter
         ;; - 'leave
         
         mouse-event?  ;; Any -> Boolean 
         mouse=?       ;; MouseEventType MouseEventType -> Boolean 
         
         ;; KeyEvent is one of: 
         ;; -- Char 
         ;; -- Symbol 
         
         key-event?    ;; Any -> Boolean
         key=?         ;; KeyEvent KeyEvent -> Boolean
         
         ;; IP : a string that points to a machine on the net 
         LOCALHOST     ;; IP
         )

(provide-higher-order-primitive
 run-simulation (create-scene) ; (Number Number Number (Nat -> Scene) -> true)
 )

;; Expr = (big-bang Expr WorldSpec ...)
;; WorldSpec = AllSpec 
;;      | (on-draw Expr)
;;      | (on-draw Expr Expr Expr)
;; -- on-draw must specify a rendering function; it may specify canvas dimensions
;;      | (on-key Expr)
;; -- on-key must specify a key event handler 
;;      | (on-mouse Expr) 
;; -- on-mouse must specify a mouse event handler 
;;      | (stop-when Expr)
;; -- stop-when must specify a boolean-valued function 
;;      | (register Expr)
;;      | (register Expr Expr)
;; -- register must specify the internet address of a host (including LOCALHOST)
;; -- it may specify a world's name 
;;      | (record? Expr)
;; -- should the session be recorded and turned into PNGs and an animated GIF
;;      | (on-receive Expr) 
;; -- on-receive must specify a receive handler 

(define-keywords WldSpec 
  [on-draw (function-with-arity 
            1 
            except
            [(f width height) 
             #'(list (proc> 'on-draw (f2h f) 1) 
                     (nat> 'on-draw width "width")
                     (nat> 'on-draw height "height"))])]
  [on-mouse (function-with-arity 4)]
  [on-key (function-with-arity 2)]
  [on-receive (function-with-arity 2)]
  [stop-when (function-with-arity 1)]
  [register (lambda (tag)
              (lambda (p)
                (syntax-case p ()
                  [(host) #`(ip> #,tag host)]
                  [_ (err tag p "expected a host (ip address)")])))]
  [name (lambda (tag)
          (lambda (p)
            (syntax-case p ()
              [(n) #`(symbol> #,tag n)]
              [_ (err tag p "expected a string for the current world")])))]
  [record? (lambda (tag)
             (lambda (p)
               (syntax-case p ()
                 [(b) #`(bool> #,tag b)]
                 [_ (err tag p "expected a boolean (to record or not to record?")])))])

(define-syntax (big-bang stx)
  (syntax-case stx ()
    [(big-bang) (raise-syntax-error #f "bad world description" stx)]
    [(big-bang w s ...)
     (let* ([Spec (append AllSpec WldSpec)]
            [kwds (map (lambda (x) (datum->syntax #'here x)) (map car Spec))]
            [rec? #'#f]
            [spec (map (lambda (stx) 
                         (syntax-case stx ()
                           [(kw . E)
                            (and (identifier? #'kw) 
                                 (for/or ([n kwds]) (free-identifier=? #'kw n)))
                            (begin
                              (when (free-identifier=? #'kw #'record?)
                                (syntax-case #'E ()
                                  [(V) (set! rec? #'V)]
                                  [_ (err 'record? stx)]))
                              (cons #'kw #;(syntax-e #'kw) (syntax E)))]
                           [_ (raise-syntax-error
                               'big-bang "not a legal big-bang clause" stx)]))
                       (syntax->list (syntax (s ...))))]
            ;; assert: all bind = (kw . E) and kw is constrained via Bind 
            [args (map (lambda (x) 
                         (define kw (car x))
                         (define co ;; patch from Jay to allow rename on import
                           (findf (lambda (n) (free-identifier=? kw (car n)))
                                  (map (lambda (k s) (cons k (cdr s))) 
                                       kwds Spec)))
                         (list (syntax-e (car co)) ((cadr co) (cdr x))))
                       spec)])
       #`(send (new (if #,rec? aworld% world%) [world0 w]  #,@args) last))]))


;                                                                 
;                                                                 
;                                                                 
;   ;   ;                  ;        ;          ;;;                
;   ;   ;                  ;        ;         ;   ;               
;   ;   ;                  ;        ;         ;   ;               
;   ;   ;   ;;;   ; ;;     ;     ;;;;         ;   ;  ;   ;  ;   ; 
;   ;   ;  ;   ;  ;;  ;    ;    ;   ;         ;;;;;  ;   ;   ; ;  
;   ; ; ;  ;   ;  ;   ;    ;    ;   ;         ;   ;  ;   ;    ;   
;   ;; ;;  ;   ;  ;        ;    ;   ;         ;   ;  ;   ;    ;   
;   ;   ;  ;   ;  ;        ;    ;   ;         ;   ;  ;  ;;   ; ;  
;   ;   ;   ;;;   ;        ;;    ;;;;         ;   ;   ;; ;  ;   ; 
;                                                                 
;                                                                 
;                                                                 

(define (run-simulation f)
  (check-proc 'run-simulation f 1 "first" "one argument")
  (big-bang 1 (on-tick add1) (on-draw f)))

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

(define ME (map symbol->string '(button-down button-up drag move enter leave)))

(define (mouse-event? a) (and (string? a) (pair? (member a ME))))

(define (mouse=? k m)
  (check-arg 'mouse=? (mouse-event? k) 'MouseEvent "first" k)
  (check-arg 'mouse=? (mouse-event? m) 'MouseEvent "second" m)
  (string=? k m))

(define (key-event? k) (string? k))

(define (key=? k m)
  (check-arg 'key=? (key-event? k) 'KeyEvent "first" k)
  (check-arg 'key=? (key-event? m) 'KeyEvent "second" m)
  (string=? k m))

(define LOCALHOST "127.0.0.1")

;; -----------------------------------------------------------------------------

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

(provide 
 ;; type World 
 iworld?    ;; Any -> Boolean 
 iworld=?   ;; World World -> Boolean 
 iworld-name ;; World -> Symbol 
 iworld1    ;; sample worlds 
 iworld2
 iworld3
 ;; type Bundle = (make-bundle [Listof World] Universe [Listof Mail]) 
 ;; type Mail = (make-mail World S-expression)
 make-bundle ;; [Listof World] Universe [Listof Mail] -> Bundle 
 bundle?     ;; is this a bundle? 
 make-mail   ;; World S-expression -> Mail 
 mail?       ;; is this a real mail? 
 universe    ;; <syntax> : see below 
 )

;; Expr = (universe Expr UniSpec)
;; UniSpec = AllSepc
;;      | (on-new Expr)
;; -- on-new must specify a 'new world" handler; what happens when a world joins
;;      | (on-msg Expr)
;; -- on-msg must specify a 'message' handler
;;      | (on-disconnect Expr)
;; -- on-disconnect may specify a handler for the event that a world is leaving
;;      | (to-string Expr) 
;; -- to-string specifies how to render the universe as a string for display 
;;    in the console 

(define-keywords UniSpec
  [on-new (function-with-arity 2)]
  [on-msg (function-with-arity 3)]
  [on-disconnect (function-with-arity 2)]
  [to-string (function-with-arity 1)])

(define-syntax (universe stx)
  (syntax-case stx ()
    [(universe) (raise-syntax-error #f "not a legal universe description" stx)]
    [(universe u) (raise-syntax-error #f "not a legal universe description" stx)]
    [(universe u bind ...)
     (let* ([Spec (append AllSpec UniSpec)]
            [kwds (map (lambda (x) (datum->syntax #'here x)) (map car Spec))]
            [spec (map (lambda (stx) 
                         (syntax-case stx ()
                           [(kw . E)
                            (and (identifier? #'kw) 
                                 (for/or ([n kwds]) (free-identifier=? #'kw n)))
                            (cons #'kw (syntax E))]
                           [(kw E)
                            (and (identifier? #'kw) 
                                 (for/or ([n kwds]) (free-identifier=? #'kw n)))
                            (list (syntax-e #'kw) (syntax E))]
                           [_ (raise-syntax-error
                               'universe "not a legal universe clause" stx)]))
                       (syntax->list (syntax (bind ...))))]
            ;; assert: all bind = (kw . E) and kw is constrained via Bind 
            [args (map (lambda (x) 
                         (define kw (car x))
                         (define co ;; patch from Jay to allow rename on import
                           (findf (lambda (n) (free-identifier=? kw (car n)))
                                  (map (lambda (k s) (cons k (cdr s))) 
                                       kwds Spec)))
                         (list (syntax-e (car co)) ((cadr co) (cdr x))))
                       spec)]
            #;
            [args (map (lambda (x) 
                         (define kw (car x))
                         (define co (assq kw Spec))
                         (list kw ((cadr co) (cdr x))))
                       spec)]
            [domain (map car args)])
       (cond
         [(not (memq 'on-new domain))
          (raise-syntax-error #f "missing on-new clause" stx)]
         [(not (memq 'on-msg domain))
          (raise-syntax-error #f "missing on-msg clause" stx)]
         [else ; (and (memq #'on-new domain) (memq #'on-msg domain))
          #`(send (new universe% [universe0 u] #,@args) last)]))]))

