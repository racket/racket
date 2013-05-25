#lang racket/gui

;; ---------------------------------------------------------------------------------------------------
;; provides the universe functionality (distributed worlds)

(require "checked-cell.rkt"
         "check-aux.rkt"
         "timer.rkt"    
         "last.rkt"
         "clauses-spec-aux.rkt"
         "stop.rkt"
         htdp/error
         (only-in mzlib/etc evcase)
         string-constants)

(provide 
 universe%
 ;; --- sample worlds and function on worlds ---
 iworld?  ;; Any -> Boolean 
 iworld=? ;; World World -> Boolean 
 iworld-name ;; World -> Symbol 
 iworld1  ;; sample worlds 
 iworld2
 iworld3
 ;; --- sending 'mail' to worlds ---
 ;; type Bundle = (make-bundle [Listof World] Universe [Listof Mail]) 
 ;; type Mail = (make-mail World S-expression)
 make-bundle ;; [Listof World] Universe [Listof Mail] -> Bundle 
 bundle?     ;; is this a bundle? 
 make-mail   ;; World S-expression -> Mail 
 mail?       ;; is this a real mail? 
 )

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

(define universe%
  (last-mixin
   (clock-mixin
    (class* object% (start-stop<%>) 
      (inspect #f) 
      (super-new)
      (init-field        ;; type Result
       ; = (make-bundle [Listof World] Universe [Listof Mail])
       universe0         ;; the initial state of the universe
       on-new            ;; Universe World -> Result
       on-msg            ;; Universe World Message -> Result
       ;; tick              ;; Universe -> Result
       (state #f)        ;; Boolean 
       (on-disconnect    ;; Universe World -> Result
        (lambda (u w) (make-bundle u '() '())))
       (to-string #f)    ;; Universe -> String 
       (check-with True) ;; Any -> Boolean 
       )
      
      (field 
       [universe 
        (new checked-cell% [value0 universe0] [ok? check-with] 
             [display (if (string? state) state (and state  "your server's state"))])])
 
      ;; -----------------------------------------------------------------------
      ;; dealing with events
      (define-syntax-rule 
        ;; A B ... -> Void
        (def/cback pub (pname a ...) 
          ;; Universe A B ... -> (cons Universe Mail)
          ;; effect: change server state, broadcast mails 
          name body ...)
        (begin
          (pub pname)
          (define (pname a ...) 
            (define (handler e) (stop! e))
            (with-handlers ([exn? handler])
              (define ___  (begin 'dummy body ...))
	      (define n (if (object-name name) (object-name name) name))
              (define nxt (name (send universe get) a ...))
              (define-values (u mails bad)
                (if (stop-the-world? nxt)
		    (stop! (stop-the-world-world nxt))
                    (bundle> n nxt)))
              (send universe set (format "value returned from ~a" 'name) u)
              (unless (boolean? to-string) (send gui add (to-string u)))
              (broadcast mails)
              (for-each (lambda (iw) (kill iw "disconnected ~a")) bad)))))
      
      ;; [Listof Mail] -> Void
      ;; send payload of messages to designated worlds 
      (define/private (broadcast lm)
        (for-each (lambda (p+m) 
                    (define w (mail-to p+m))
                    (define p (mail-content p+m))
                    (define n (iworld-name w))
                    (if (memq w iworlds)
                        (with-handlers ((exn:fail? (lambda (e) (kill w "broadcast failed to ~a"))))
                          (send gui add (format "-> ~a: ~a" n p))
                          (iworld-send w p))
                        (send gui add (format "~s not on list" n))))
                  lm))
      
      (def/cback private (pnew iworld) on-new
        (set! iworlds (cons iworld iworlds))
        (send gui add (format "~a signed up" (iworld-name iworld))))
      
      (def/cback private (pmsg iworld r) on-msg
        (send gui add (format "~a ->: ~a" (iworld-name iworld) r)))
      
      (def/cback private (pdisconnect iworld) on-disconnect
        (kill iworld "~a !! closed port"))
      
      ;; tick, tock : deal with a tick event for this world 
      (def/cback pubment (ptock) (let ([on-tick (lambda (w) (pptock w))]) on-tick))
      (define/public (pptock w) (void))
      (define/public (name-of-tick-handler)
        "the on-tick-handler")
      
      ;; IWorld FormatString -> Void 
      ;; effect: remove from given iworld from iworlds 
      ;;         and shut down all connections 
      (define/private (kill w msg)
        (iworld-close w)
        (set! iworlds (remq w iworlds))
        (send gui add (format msg (iworld-name w)))
        (when (null? iworlds) (restart)))
      
      ;; -----------------------------------------------------------------------
      ;; start and stop server, start and stop the universe
      
      (field [iworlds   '()] ;; [Listof World]
             [gui      (new gui%
                            [stop-server (lambda () (stop! (send universe get)))] 
                            [stop-and-restart (lambda () (restart))])]
             [dr:custodian  (current-custodian)]
             [the-custodian (make-custodian)])
      
      ;; start the universe, enable registrations and message exchanges
      (define/public (start!)
        (set! the-custodian (make-custodian))
        (parameterize ([current-custodian the-custodian])
          (define (loop)
            (apply sync 
                   (handle-evt (tcp-accept-evt tcp-listener) add-iworld)
                   (map(lambda (p) (handle-evt (iworld-in p) (process-message p))) iworlds)))
          ;;; WHERE 
          (define tcp-listener 
            (with-handlers ((exn:fail:network? (lambda (x) (stop! x))))
              (tcp-listen SQPORT 4 #t)))
          ;; (list IPort OPort) -> Void 
          (define (add-iworld in-out)
            (define in (first in-out))
            (define out (second in-out))
            ;; is it possible to kill the server with lots of bad connections?
            (with-handlers ((tcp-eof? (lambda _ (loop)))
                            (exn? (lambda (e)
                                    (printf "process registration failed!\n~a" 
                                            (exn-message e))
                                    (loop))))
              (tcp-process-registration
               in out (lambda (info) (pnew (create-iworld in out info))))
              (loop)))
          ;; IWorld -> [IPort -> Void]
          (define (process-message p)
            (lambda (in)
              (define (disc e)
                (pdisconnect p)
                (loop))
              (with-handlers ((tcp-eof? disc))
                (pmsg p (tcp-receive in))
                (loop))))
          ;; --- go universe go ---
          (set! iworlds '())
          (send universe set "initial expression" universe0)
          (send gui add "a new universe is up and running")
          (thread loop)))
      
      (define/private (restart)
        ;; I am running in a custodian that is about to be killed, 
        ;; so let's switch to one up in the hierarchy
        (let ([old-t (current-thread)]
              [go (make-semaphore)])
          (parameterize ([current-custodian dr:custodian])
            (thread (lambda ()
                      (sync old-t go)
                      (start!))))
          (send gui add "stopping the universe")
          (send gui add "----------------------------------")
          (for-each iworld-close iworlds)
          (custodian-shutdown-all the-custodian)
          (semaphore-post go)))
      
      (define/public (stop! msg) 
        (send gui show #f)
        (custodian-shutdown-all the-custodian))
      
      ;; -----------------------------------------------------------------------
      ;; initialize the universe and run 
      (send gui show #t)
      (start!)))))


;                                            
;                                            
;                                            
;   ;   ;                  ;        ;        
;   ;   ;                  ;        ;        
;   ;   ;                  ;        ;        
;   ;   ;   ;;;   ; ;;     ;     ;;;;   ;;;  
;   ;   ;  ;   ;  ;;  ;    ;    ;   ;  ;   ; 
;   ; ; ;  ;   ;  ;   ;    ;    ;   ;   ;;;  
;   ;; ;;  ;   ;  ;        ;    ;   ;      ; 
;   ;   ;  ;   ;  ;        ;    ;   ;  ;   ; 
;   ;   ;   ;;;   ;        ;;    ;;;;   ;;;  
;                                            
;                                            
;                                            

;; --- the server representation of a world --- 
(define-struct iworld (in out name info) #; #:transparent)
;; World = (make-iworld IPort OPort Symbol [Listof Sexp])

(define (iw* n) (make-iworld (current-input-port) (current-output-port) n '()))
(define iworld1 (iw* "iworld1"))
(define iworld2 (iw* "iworld2"))
(define iworld3 (iw* "iworld3"))

(define (iworld=? u v)
  (check-arg 'iworld=? (iworld? u) 'iworld "first" u)
  (check-arg 'iworld=? (iworld? v) 'iworld "second" v)
  (eq? u v))

;; IWorld -> Void
(define (iworld-close p)
  (close-output-port (iworld-out p))
  (close-input-port (iworld-in p)))

;; IPort OPort Sexp -> IWorld 
(define (create-iworld i o info)
  (make-iworld i o info "info field not available"))

;; Player S-exp -> Void
(define (iworld-send p sexp)
  (tcp-send (iworld-out p) sexp))

;                       
;                       
;                       
;    ;;;   ;   ;    ;   
;   ;   ;  ;   ;    ;   
;   ;      ;   ;    ;   
;   ;      ;   ;    ;   
;   ;  ;;  ;   ;    ;   
;   ;   ;  ;   ;    ;   
;   ;   ;  ;   ;    ;   
;   ;   ;  ;   ;    ;   
;    ;;;    ;;;     ;   
;                       
;                       
;                       

;; effect: create and show a gui with two buttons and an editor for logging
(define gui%
  (class frame%
    (init stop-server stop-and-restart)
    (inherit show)
    (define/augment (on-close) (end))
    (super-new [label "Universe"][width 500][height 300][style '(metal)])
    (field
     [end (lambda _ (show #f) (stop-server))]
     [panel (new horizontal-panel% [parent this] [stretchable-height #f]
                 [alignment '(center center)])]
     [stop  (new button% [parent panel] [label "stop"] [callback end])]
     [s&re  (new button% [parent panel] [label "stop and restart"] 
                 [callback (lambda (but evt) (stop-and-restart))])]
     [text  (new text%)]
     [edit  (new editor-canvas% [parent this] [editor text]
                 [style '(no-border no-hscroll auto-vscroll)])])
    
    ;; add lines to the end of the text 
    (define/public (add str)
      (queue-callback 
       (lambda () 
         (send text lock #f)
         (send text insert (format "~a\n" str) (send text last-position))
         (send text lock #t))))
    
    ;; -------------------------------------------------------------------------
    ;; add menu, lock, and show 
    (copy-and-paste this)
    (send text lock #t)))

;; -----------------------------------------------------------------------------
;; Frame Text -> Void
;; add menu bar to frame for copying all of the text 
(require string-constants)

(define (copy-and-paste frame)
  (define mb (new menu-bar% [parent frame]))
  (define edit (new menu%
                    [label (string-constant edit-menu-label)]
                    [parent mb]))
  (new menu-item%
       [label (string-constant copy-menu-item)]
       [parent edit]
       [shortcut #\c]
       [callback (lambda (m e)
                   (define t (send frame get-focus-object))
                   (when (is-a? t editor<%>)
                     (send t copy)))])
  (new menu-item%
       [label (string-constant select-all-menu-item)]
       [parent edit]
       [shortcut #\a]
       [callback (lambda (m e)
                   (define t (send frame get-focus-object))
                   (when (is-a? t text%)
                     (send t set-position 0 (send t last-position))))])
  (void))

;                              
;                              
;  ;;; ;;;          ;     ;;   
;   ;; ;;                  ;   
;   ;; ;;   ;;;   ;;;      ;   
;   ; ; ;  ;   ;    ;      ;   
;   ; ; ;   ;;;;    ;      ;   
;   ;   ;  ;   ;    ;      ;   
;   ;   ;  ;   ;    ;      ;   
;  ;;; ;;;  ;;;;; ;;;;;  ;;;;; 
;                              
;                              
;                              
;                              

(define-struct bundle (state mails bad) #:transparent)

(set! make-bundle
      (let ([make-bundle make-bundle])
        (lambda (state mails bads)
          (check-arg-list 'make-bundle mails mail? "mail" "second")
          (check-arg-list 'make-bundle bads iworld? "iworld" "third")
          (make-bundle state mails bads))))

;; Symbol Any (Any -> Boolean) String String -> Void 
;; raise a TP exception if low is not a list of world? elements 
(define (check-arg-list tag low iworld? msg rank)
  (check-arg tag (list? low) (format "list [of ~as]" msg) rank low)
  (for-each (lambda (c) 
              (check-arg tag (iworld? c) msg (format "(elements of) ~a" rank) c))
            low))

;; Symbol Any ->* Universe [Listof Mail] [Listof IWorld]
(define (bundle> tag r)
  (unless (bundle? r) 
    (tp-error tag "expected the ~a function to return a bundle, but it returned ~e" tag r))
  (values (bundle-state r) (bundle-mails r) (bundle-bad r)))

(define-struct mail (to content) #:transparent)

(set! make-mail
      (let ([make-mail make-mail])
        (lambda (to content)
          (check-arg 'make-mail (iworld? to) 'iworld "first" to)
          (check-arg 'make-mail (sexp? content) 'S-expression "second" content)
          (make-mail to content))))
