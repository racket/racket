(module mztake-structs mzscheme
  (require (lib "mred.ss" "mred")
           (prefix frp: (lib "frp.ss" "frtime"))
           "private/more-useful-code.ss")
  
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
  
  (define-struct trace-struct (evnt-rcvr))           ; frp:event-receiver
  
  (define-struct (break-trace trace-struct) ())
  (define-struct (bind-trace trace-struct)
                 (variable-to-bind))          ; symbol
  
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
                                clients))     ; list of all the clients attached to this process
  
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
  
  ; Creates a trace that binds to the value of a variable in scope
  (define (create-bind-trace sym-to-bind) ; ((union (listof symbol?) symbol?) . -> . trace?)
    (make-bind-trace (frp:event-receiver) sym-to-bind))
  
  ; Creates a trace that simply pauses the program
  (define (create-break-trace) ; (void? . -> . trace?)
    (make-break-trace (frp:event-receiver)))
  
  (define (create-empty-debug-process)
    (make-debug-process (make-custodian)
                        null                 ; run-semaphore - null so we know it has never started
                        #f                   ; running?
                        (frp:new-cell)       ; exited?
                        (frp:event-receiver) ; exceptions
                        null                 ; runtime
                        empty))              ; clients
  
  (define (create-empty-debug-client)
    (make-debug-client null        ; modpath
                       (make-hash) ; tracepoints
                       null        ; line-col->pos function
                       null))      ; process
  
  ;###########################################################################################################
  )