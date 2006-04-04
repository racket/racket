(module mztake-structs mzscheme
  (require (lib "match.ss")
           (lib "etc.ss")
           (lib "base-gm.ss" "frtime"))
  
  (provide (all-defined))
  
  (define require-spec?
    (match-lambda
      [(? string?) true]
      [('file (? string?)) true]
      [('lib (? string?) (? string?) ...) true]
      [('planet . arg) true]
      [else false]))
  
  ;    ;;;;;   ;                          ;          
  ;   ;     ;  ;                          ;          
  ;  ;         ;                          ;          
  ;  ;        ;;;;;; ;;; ;      ;   ;;;  ;;;;; ;;;;  
  ;  ;         ;   ;;    ;      ;  ;   ;  ;   ;    ; 
  ;   ;;;      ;   ;     ;      ; ;       ;   ;      
  ;      ;;;   ;   ;     ;      ; ;       ;   ;      
  ;         ;  ;   ;     ;      ; ;       ;    ;;;;  
  ;         ;  ;   ;     ;      ; ;       ;        ; 
  ;         ;  ;   ;     ;      ; ;       ;        ; 
  ;  ;     ;   ;   ;      ;    ;;  ;   ;  ;   ;    ; 
  ;   ;;;;;     ;;;;       ;;;; ;   ;;;    ;;; ;;;;  
  
  (define-struct trace-struct (evnt-rcvr thunk))           ; frp:event-receiver
  
  (define-struct debug-client (modpath          ; complete-path of the module
                               tracepoints      ; hash-table of traces
                               line-col->pos    ; memoized O(n) function to map line/col -> byte offset
                               pattern->pos
                               process))        ; parent debug-process
  
  (define-struct debug-process (custodian     ; If you shutdown-all it will kill the debugger process
                                run-semaphore ; When you post to this the debuggee will continue executing
                                running-e     ; #t on this event resumes, #f pauses
                                run-manager   ; saves behavior that actually pauses/resumes from GC
                                pause-requested?
                                resume-requested?
                                policy

                                exited?       ; FrTime cell receives #t when the target exits
                                exceptions    ; (an event stream) Exceptions thrown during the evaluation of the target
                                main-client   ; the main client module that will be run
                                clients       ; list of all the clients attached to this process
                                top-level
                                
                                where         ; a behavior signaling each position where we pause
                                marks))       ; while paused, the marks at the point of the pause (else false)
  
  (define-struct loc (reqspec after?))
  (define-struct (loc/lc loc) (line col))
  (define-struct (loc/p loc) (pattern))
                      
  ;###########################################################################################################
  
  
  
  
  ;    ;;;;;   ;                          ;           ;;;;;                         ;                         
  ;   ;     ;  ;                          ;         ;;     ;                        ;                         
  ;  ;         ;                          ;         ;                               ;                         
  ;  ;        ;;;;;; ;;; ;      ;   ;;;  ;;;;;     ;         ; ;;;   ;;;     ;;;;  ;;;;;  ;;;;   ; ;;;  ;;;;  
  ;  ;         ;   ;;    ;      ;  ;   ;  ;        ;         ;;     ;   ;   ;    ;  ;    ;    ;  ;;    ;    ; 
  ;   ;;;      ;   ;     ;      ; ;       ;        ;         ;     ;     ;       ;  ;   ;      ; ;     ;      
  ;      ;;;   ;   ;     ;      ; ;       ;        ;         ;     ;     ;   ;;;;;  ;   ;      ; ;     ;      
  ;         ;  ;   ;     ;      ; ;       ;        ;         ;     ;;;;;;;  ;    ;  ;   ;      ; ;      ;;;;  
  ;         ;  ;   ;     ;      ; ;       ;        ;         ;     ;       ;     ;  ;   ;      ; ;          ; 
  ;         ;  ;   ;     ;      ; ;       ;         ;        ;     ;       ;     ;  ;   ;      ; ;          ; 
  ;  ;     ;   ;   ;      ;    ;;  ;   ;  ;         ;;     ; ;      ;    ; ;    ;;  ;    ;    ;  ;     ;    ; 
  ;   ;;;;;     ;;;;       ;;;; ;   ;;;    ;;;        ;;;;;  ;       ;;;;   ;;;; ;   ;;;  ;;;;   ;      ;;;;  
  
  (define (create-empty-debug-client)
    (make-debug-client null        ; modpath
                       (make-hash) ; tracepoints
                       null        ; line-col->pos function
                       null
                       null))      ; process
  
  ;###########################################################################################################
  )