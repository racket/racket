(module mztake-structs mzscheme
  (require (prefix frp: (lib "frp.ss" "frtime"))
           (lib "more-useful-code.ss" "mztake" "private"))
  
  (provide (all-defined))
  
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
                               process))        ; parent debug-process
  
  (define-struct debug-process (custodian     ; If you shutdown-all it will kill the debugger process
                                run-semaphore ; When you post to this the debuggee will continue executing
                                running?      ; Is the program (supposed-to-be) currently running
                                exited?       ; FrTime cell receives #t when the target exits
                                exceptions    ; (an event stream) Exceptions thrown during the evaluation of the target
                                runtime       ; Behavior with current runtime in milliseconds
                                main-client   ; the main client module that will be run
                                clients       ; list of all the clients attached to this process
                                marks))       ; while paused, the marks at the point of the pause (else false)
  
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
  
  (define (create-empty-debug-process)
    (make-debug-process (make-custodian)
                        null                 ; run-semaphore - null so we know it has never started
                        #f                   ; running?
                        (frp:new-cell)       ; exited?
                        (frp:event-receiver) ; exceptions
                        null                 ; runtime
                        null                 ; main-client
                        empty                ; clients
                        false))              ; marks
  
  (define (create-empty-debug-client)
    (make-debug-client null        ; modpath
                       (make-hash) ; tracepoints
                       null        ; line-col->pos function
                       null))      ; process
  
  ;###########################################################################################################
  )