#lang scheme/gui

(require (for-syntax "syn-aux.ss")
         "check-aux.ss"
         "timer.ss"    
         "last.ss"
         scheme/match
         htdp/error
         (only-in mzlib/etc evcase)
         string-constants)

(provide universe%)

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
    (class* object% (start-stop<%>) (inspect #f) (super-new)
      (init-field        ;; type Result
                         ; = (make-bundle [Listof World] Universe [Listof Mail])
       universe0         ;; the initial state of the universe
       on-new            ;; Universe World -> Result
       on-msg            ;; Universe World Message -> Result
       tick              ;; Universe -> Result
       (on-disconnect    ;; Universe World -> Result
        (lambda (u w) (list u)))
       (to-string #f)    ;; Universe -> String 
       )
      
      (field [universe universe0])
      
      ;; -----------------------------------------------------------------------
      ;; dealing with events
      (define-syntax-rule 
        ;; A B ... -> Void
        (def/cback pub (pname a ...) 
          ;; Universe A B ... -> (cons Universe Mail)
          ;; effect: change server state, broadcast mails 
          name)
        (begin
          (pub pname)
          (define (pname a ...) 
            (define (handler e) (stop! e))
            (with-handlers ([exn? handler])
              (define r (check-state-x-mail 'name (name iworlds universe a ...)))
              (define u (bundle-state r))
              (set! iworlds (bundle-low r))
              (set! universe u)
              (unless (boolean? to-string) (send gui add (to-string iworlds u)))
              (broadcast (bundle-mails r))))))
      
      (def/cback private (pmsg iworld received) on-msg)
      
      (def/cback private (pdisconnect iworld) on-disconnect)
      
      (def/cback private (pnew iworld) ppnew)
      
      (define/private (ppnew low uni p)
        (iworld-send p 'okay)
        (on-new low uni p))
      
      (def/cback public (ptock) tick)
      
      ;; Symbol Any -> Result
      ;; check that r is Result
      ;; effect: stop the server if the callbacks perform badly 
      (define/private (check-state-x-mail tag r)
        (with-handlers ((exn? (lambda (x) (stop! x))))
          (define s (format "expected from ~a, given: " tag))
          (define f "(make-bundle [Listof World] Universe [Listof Mail]) ~a~e")
          (unless (bundle? r) 
            (error tag (format f s r)))
          r))
      
      ;; -----------------------------------------------------------------------
      ;; start and stop server, start and stop the universe
      
      (field [iworlds   '()] ;; [Listof World]
             [gui      (new gui%
                            [stop-server (lambda () (stop! universe))] 
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
                   (map iworld-wait-for-msg iworlds)))
          (define (add-iworld in-out)
            (with-handlers ((tcp-eof? (lambda _ (loop))))
              (define in (first in-out))        
              (define next (tcp-receive in))
              (match next
                [(cons 'REGISTER info)
                 (let* ([w (create-iworld in (second in-out) info)])
                   ; (set! iworlds (cons w iworlds))
                   (pnew w)
                   (send gui add (format "~a signed up" info))
                   (loop))]
                [else (loop)])))
          (define (iworld-wait-for-msg p)
            (handle-evt (iworld-in p) 
                        (lambda (in)
                          (with-handlers 
                              ((tcp-eof? 
                                (lambda (e) 
                                  (handler p e 
                                           (lambda () 
                                             (if (null? iworlds)
                                                 (restart)
                                                 (loop)))))))
                            (define r (tcp-receive in))
                            (send gui add (format "~a ->: ~a" (iworld-name p) r))
                            (pmsg p r)
                            (loop)))))
          (define tcp-listener 
            (with-handlers ((exn:fail:network? (lambda (x) (stop! x))))
              (tcp-listen SQPORT 4 #t)))
          ;; --- go universe go ---
          (set! iworlds '())
          (set! universe universe0)
          (send gui add "a new universe is up and running")
          (thread loop)))
      
      ;; World Exn (-> X) -> X
      (define/private (handler p e cont)
        (close-output-port (iworld-out p))
        (close-input-port (iworld-in p))
        (send gui add (format "~a !! closed port" (iworld-name p)))
        (pdisconnect p)
        (cont))
      
      ;; [Listof Mail] -> Void
      ;; send payload of messages to designated worlds 
      (define/private (broadcast lm)
        ;;; --- why the heck is there no exception handler -------------
        (for-each (lambda (p+m) 
                    ;; what exception should I catch 
                    ;; remove the world from the list 
                    ;; factor out from elsewhere 
                    ;; can this mean I perform a callback during a callback? 
                    ;; collect 'bad' worlds instead and disconnect them later?
                    ;; (handler 
                    (with-handlers ((exn? (lambda (e) (printf "\n\n*** to be done ***\n\n"))))
                      (define w (mail-to p+m))
                      (define n (iworld-name w))
                      (define p (mail-content p+m))
                      (unless (memq w iworlds)
                        (send gui add (format "~s not on list" n)))
                      (when (memq w iworlds)
                        (iworld-send w p)
                        (send gui add (format "-> ~a: ~a" n p)))))
                  lm))
      
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
          (for-each (lambda (w)
                      (close-input-port (iworld-in w))
                      (close-output-port (iworld-out w)))
                    iworlds)
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

(provide 
 iworld?  ;; Any -> Boolean 
 iworld=? ;; World World -> Boolean 
 iworld-name ;; World -> Symbol 
 iworld1  ;; sample worlds 
 iworld2
 iworld3)

;; --- the server representation of a world --- 
(define-struct iworld (in out name info) #:transparent)
;; World = (make-iworld IPort OPort Symbol [Listof Sexp])

(define iworld1 (make-iworld (current-input-port) (current-output-port) 'iworld1 '()))
(define iworld2 (make-iworld (current-input-port) (current-output-port) 'iworld2 '()))
(define iworld3 (make-iworld (current-input-port) (current-output-port) 'iworld3 '()))

(define (iworld=? u v)
  (check-arg 'iworld=? (iworld? u) 'iworld "first" u)
  (check-arg 'iworld=? (iworld? v) 'iworld "second" v)
  (eq? u v))

;; IPort OPort Sexp -> Player 
(define (create-iworld i o info)
  (if (and (pair? info) (symbol? (car info)))
      (make-iworld i o (car info) (cdr info))
      (make-iworld i o (gensym 'iworld) info)))

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

(provide
 ;; type Bundle = (make-bundle [Listof World] Universe [Listof Mail]) 
 ;; type Mail = (make-mail World S-expression)
 make-bundle ;; [Listof World] Universe [Listof Mail] -> Bundle 
 bundle?     ;; is this a bundle? 
 make-mail   ;; World S-expression -> Mail 
 mail?       ;; is this a real mail? 
 )

(define-struct bundle (low state mails) #:transparent)

(set! make-bundle
      (let ([make-bundle make-bundle])
        (lambda (low state mails)
          (check-arg-list 'make-bundle low iworld? "iworld" "first")
          (check-arg-list 'make-bundle mails mail? "mail" "third")
          (make-bundle low state mails))))

;; Symbol Any (Any -> Boolean) String String -> Void 
;; raise a TP exception if low is not a list of world? elements 
(define (check-arg-list tag low iworld? msg rank)
  (check-arg tag (list? low) (format "list [of ~as]" msg) rank low)
  (for-each (lambda (c) 
              (check-arg tag (iworld? c) msg (format "(elements of) ~a" rank) c))
            low))

(define-struct mail (to content) #:transparent)

(set! make-mail
      (let ([make-mail make-mail])
        (lambda (to content)
          (check-arg 'make-mail (iworld? to) 'iworld "first" to)
          (check-arg 'make-mail (sexp? content) 'S-expression "second" content)
          (make-mail to content))))
