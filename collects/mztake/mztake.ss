(module mztake mzscheme
  
  (define mztake-version "rev. 8/6/2004")
  
  (require (lib "match.ss")
           (lib "contract.ss")
           (lib "marks.ss" "mztake" "private")
           (prefix frp: (lib "frp.ss" "frtime"))
           (lib "useful-code.ss" "mztake" "private")
           (lib "more-useful-code.ss" "mztake" "private") ; mostly for hash- bindings
           "mztake-structs.ss"
           (lib "load-annotator.ss" "mztake" "private")
           "annotator.ss"
           )
  
  (provide/contract [start/resume (debug-process? . -> . void?)]
                    [kill (debug-process? . -> . void?)]
                    [kill-all (-> void?)]
                    [pause (debug-process? . -> . void?)]
                    [rename debug-process-exceptions
                            process:exceptions
                            (debug-process? . -> . frp:event?)]
                    [rename runtime/seconds
                            process:runtime/seconds
                            (debug-process? . -> . frp:behavior?)]
                    [rename runtime/milliseconds
                            process:runtime/milliseconds
                            (debug-process? . -> . frp:behavior?)]
                    [rename debug-process-exited?
                            process:exited?
                            (debug-process? . -> . frp:behavior?)])
  
  #| DISABLED - BROKEN
  [process:running? (debug-process? . -> . frp:behavior?)]
  [rename time-per-event/milliseconds
  process:time-per-event/milliseconds
  (debug-process? frp:behavior? . -> . frp:behavior?)]
  |#
  
  
  ;              ;           ;                 ;                                      
  ;     ;;;;;;   ;           ;                 ;       ;       ;                      
  ;   ;;      ;  ;           ;                 ;       ;       ;                      
  ;   ;          ;           ;                 ;        ;     ;                       
  ;  ;           ;    ;;;;   ; ;;;;     ;;;;   ;        ;     ;    ;;;;  ; ;;;  ;;;;  
  ;  ;           ;   ;    ;  ;;    ;   ;    ;  ;        ;     ;   ;    ; ;;    ;    ; 
  ;  ;           ;  ;      ; ;      ;       ;  ;         ;   ;         ; ;     ;      
  ;  ;    ;;;;;  ;  ;      ; ;      ;   ;;;;;  ;         ;   ;     ;;;;; ;     ;      
  ;  ;        ;  ;  ;      ; ;      ;  ;    ;  ;         ;   ;    ;    ; ;      ;;;;  
  ;  ;        ;  ;  ;      ; ;      ; ;     ;  ;          ; ;    ;     ; ;          ; 
  ;   ;       ;  ;  ;      ; ;      ; ;     ;  ;          ; ;    ;     ; ;          ; 
  ;   ;;      ;  ;   ;    ;  ;     ;  ;    ;;  ;          ; ;    ;    ;; ;     ;    ; 
  ;     ;;;;;;   ;    ;;;;   ;;;;;;    ;;;; ;  ;           ;      ;;;; ; ;      ;;;;  
  
  ;Keeps track of all debugging processes
  (define all-debug-processes null)
  
  ; turns debug output on and off
  (define debugging? #f)
  
  ;###########################################################################################################
  
  
  
  
  ;             ;                                                    ;                               ; 
  ;  ;;;;;;     ;                            ;;;;;;                  ;                               ; 
  ;  ;     ;;   ;                            ;     ;                 ;                               ; 
  ;  ;       ;  ;                            ;     ;                 ;                               ; 
  ;  ;       ;  ; ;;;;     ;;;;;; ; ;;;      ;     ;    ;;;;    ;;;  ;    ;    ;;;   ; ;;;;     ;;;;;; 
  ;  ;        ; ;;    ;   ;     ; ;;         ;    ;    ;    ;  ;   ; ;   ;    ;   ;  ;;    ;   ;     ; 
  ;  ;        ; ;      ; ;      ; ;          ;;;;;;         ; ;      ;  ;    ;     ; ;      ; ;      ; 
  ;  ;        ; ;      ; ;      ; ;          ;     ;    ;;;;; ;      ; ;     ;     ; ;      ; ;      ; 
  ;  ;        ; ;      ; ;      ; ;          ;      ;  ;    ; ;      ;;;     ;;;;;;; ;      ; ;      ; 
  ;  ;       ;  ;      ; ;      ; ;          ;      ; ;     ; ;      ;  ;    ;       ;      ; ;      ; 
  ;  ;       ;  ;      ; ;      ; ;          ;      ; ;     ; ;      ;   ;   ;       ;      ; ;      ; 
  ;  ;     ;;   ;     ;   ;    ;; ;          ;     ;  ;    ;;  ;   ; ;    ;   ;    ; ;      ;  ;    ;; 
  ;  ;;;;;;     ;;;;;;     ;;;; ; ;          ;;;;;;    ;;;; ;   ;;;  ;     ;   ;;;;  ;      ;   ;;;; ; 
  ;                             ;                                                                      
  ;                       ;    ;                                                                       
  ;                        ;;;;                                                                        
  
  
  (define (kill-all)
    (unless (empty? all-debug-processes)
      (for-each (lambda (p) (kill p)) all-debug-processes)
      (display "All debug processes have been killed.")))
  
  
                                        ; wrapper for errors related to the script only
  (define (script-error err)
    (raise-syntax-error 'mztake:script-error (format "~a" err))
    (kill-all))
  
  
  (define (client-error err)
    (display (format "mztake:client-error: ~a~n---~n" err))
    (kill-all))
  
  
  (define (print-debug str)
    (when debugging?
      (display (format "mztake:debug: ~a~n---~n" str))))
  
  
  (define (print-info str)
    (display (format "mztake: ~a~n---~n" str)))
  
  
  (define create-trace
    (case-lambda
      [(client line col type args)
       (case type
         ['bind  (trace/bind client line col args)]
         ['entry (trace/entry client line col)]
         [else (script-error (format "Invalid trace type: `~a' in client: `~a'"
                                     (symbol->string type)
                                     (debug-client-modpath client)))])]
      
      [(client line col type)
       (create-trace client line col type null)]))
  
  
                                        ; takes a single trace, looks up what it needs to do, and returns an frp-event to publish
  (define (trace->frp-event client top-mark marks trace)
    (match trace
           [($ entry-trace evnt-rcvr)
            (list evnt-rcvr #t)]
           
           [($ bind-trace evnt-rcvr variable-to-bind)
            (let* ([vars (if (list? variable-to-bind) variable-to-bind
                             (list variable-to-bind))]
                   [values (map
                            (lambda (var)
                              (let ([val (bindings top-mark marks var)])
                                (if (empty? val)
                                    (script-error
                                     (format "Variable not found at the syntax location for the BIND: `~a'" var))
                                    (cadar (bindings top-mark marks var)))))
                            vars)])
              (list evnt-rcvr
                    (if (list? variable-to-bind) values
                        (first values))))]))  
  
                                        ; returns a memoized function that takes (line column) -> position
                                        ; line-col->pos : (debug-file? . -> . (number? number? . -> . (union void? number?)))
  (define (line-col->pos filename)
                                        ; produces a nested list of (line column offset) for all addressable syntax
    (define (unwrap-syntax stx)
      (let ([elt (list (syntax-line stx)
                       (syntax-column stx)
                       (sub1 (syntax-position stx)))])
        (syntax-case stx ()
          [(item ...) (cons elt (map unwrap-syntax (syntax->list stx)))]
          [x elt])))
    
    (let ([pos-list
           (flatten (parameterize ([port-count-lines-enabled #t])
                      (let ([port (open-input-file filename)])
                        (begin0
                            (let loop ([stx (read-syntax filename port)])
                              (if (eof-object? stx) '()
                                  (cons (unwrap-syntax stx)
                                        (loop (read-syntax filename port)))))
                          (close-input-port port)))))])
      (lambda (line col)
        (let loop ([lst pos-list]
                   [last-coord (first pos-list)])
          (cond
                                        ; none is found
           [(empty? lst)
            (raise (format "No syntax found for trace at line/column ~a:~a in client `~a'" line col filename))]
           
                                        ; if first is correct line and correct column
           [(and (= line (caar lst))
                 (= col (cadar lst)))
            (third (first lst))]
           
           [else (loop (rest lst)
                       (first lst))])))))
  
  
  ;###########################################################################################################
  
  
  
  ;  ;;;;;;                                                    ;;;;;;;                                 
  ;  ;     ;                                                   ;                                       
  ;  ;      ;                                                  ;                                       
  ;  ;      ; ; ;;;   ;;;;     ;;;    ;;;    ;;;;   ;;;;       ;       ;      ; ; ;;;;     ;;;   ;;;;  
  ;  ;      ; ;;     ;    ;   ;   ;  ;   ;  ;    ; ;    ;      ;       ;      ; ;;    ;   ;   ; ;    ; 
  ;  ;      ; ;     ;      ; ;      ;     ; ;      ;           ;;;;;;; ;      ; ;      ; ;      ;      
  ;  ;     ;  ;     ;      ; ;      ;     ; ;      ;           ;       ;      ; ;      ; ;      ;      
  ;  ;;;;;;   ;     ;      ; ;      ;;;;;;;  ;;;;   ;;;;       ;       ;      ; ;      ; ;       ;;;;  
  ;  ;        ;     ;      ; ;      ;            ;      ;      ;       ;      ; ;      ; ;           ; 
  ;  ;        ;     ;      ; ;      ;            ;      ;      ;       ;      ; ;      ; ;           ; 
  ;  ;        ;      ;    ;   ;   ;  ;    ; ;    ; ;    ;      ;        ;    ;; ;      ;  ;   ; ;    ; 
  ;  ;        ;       ;;;;     ;;;    ;;;;   ;;;;   ;;;;       ;         ;;;; ; ;      ;   ;;;   ;;;;  


  (define (find-client process modpath) 
    (printf "find-client ~s ~s ~n" (map debug-client-modpath (debug-process-clients process)) modpath) 
   (cond
     [(memf (lambda (c) (equal? (debug-client-modpath c) modpath))
            (debug-process-clients process)) => first]
     [else false]))
  
  (define (break? process client)
    (printf "break? ~a ~a~n" client (debug-client-tracepoints client))
    (let ([tracepoints (and client (debug-client-tracepoints client))])
      (if tracepoints
          (lambda (pos) 
            (begin0/rtn
             (hash-get tracepoints (sub1 pos) (lambda () false))
             (printf "break? ~a~n" rtn)))
          (lambda (pos) false))))
  
  (define (receive-result process client top-mark marks)
    (printf "receive-result~n")
    (let* ([byte-offset (sub1 (syntax-position (mark-source top-mark)))]
           [traces (hash-get (debug-client-tracepoints client) byte-offset)])
      
      (assert (not (empty? traces))
              (format "There are no traces at offset ~a, but a trace point is defined!~n"
                      (number->string byte-offset)))
      
                                        ; Run all traces at this trace point               
      (let ([to-send (map (lambda (t) (trace->frp-event client top-mark marks t)) traces)])
        (printf "frp:send-synchronous-events ~a~n" to-send)
        (frp:send-synchronous-events to-send))
      
                                        ; Now that we processed the trace, do we want to pause ojr continue
      (unless (running-now? process)
        (semaphore-wait (debug-process-run-semaphore process)))))
  
  
  
  
  (define ((break-after process client) top-mark marks . vals)
    (receive-result process client top-mark marks) ; TODO: have access to return value
    (apply values vals)) ; TODO: allow modification of the return value
  
  (define ((break-before process client) top-mark marks)
    (receive-result process client top-mark marks) ; TODO: allow substitute value
    false)
  
  (define (run* process)
    (require/sandbox+annotations
     (debug-process-custodian process)
     ;; error-display-handler :
     (let ([orig-err-disp (error-display-handler)])
       (lambda (msg exn)
         (frp:send-event (debug-process-exceptions process) exn)
         (orig-err-disp msg exn)))
     `(file ,(debug-client-modpath (debug-process-main-client process)))
     ;; annotate-module?
     (lambda (filename module-name)
       (begin0/rtn
        (memf (lambda (c) (equal? (debug-client-modpath c) (path->string filename)));; TODO: harmonize path & string
              (debug-process-clients process))
        (printf "annotate-module? ~s ~s ~s : ~s~n"
                (map debug-client-modpath (debug-process-clients process))
                filename module-name rtn)))
     ;; annotator?
     (lambda (stx)
       (let ([client (find-client process (syntax-source stx))])
         (if (not client)
             stx
             (let-values ([(annotated-stx pos-list)
                           (annotate-for-single-stepping 
                            stx 
                            (break? process client)
                            (break-before process client)
                            (break-after process client)
                            (lambda (kind bound binding) (void)))])
               annotated-stx))))))
     
  (define (start-debug-process process)    
                                        ; initialize the semaphore
    (set-debug-process-run-semaphore! process (make-semaphore))
                                        ; set initial state of exit predicate
    (frp:set-cell! (debug-process-exited? process) #f)    
    
    (thread (lambda ()
              (thread-wait (thread (lambda () (run* process))))
                                        ; program terminates
              (stop process)
              (print-info (format "process exited: ~a" (main-client-name process))))))
  
  
  ; predicate - is the debugee supposed to be running now?
  (define (running-now? process)
    (and (not (null? (debug-process-run-semaphore process)))
         (frp:value-now (debug-process-running? process))))
  
  
  (define (main-client-name process)
    (let-values ([(_ name __)
                  (split-path (debug-client-modpath (debug-process-main-client process)))])
      name))
  
  ; Switches the running state on or off
  ; (debug-process? boolean? . -> . void?)
  (define (set-running! process run?)
    (set-debug-process-running?! process run?)
    
    ; start the debugger if needed
    (when (null? (debug-process-run-semaphore process))
      (print-info (format "starting debugger for ~a" (main-client-name process)))
      (start-debug-process process))
    
    (when run?
      (semaphore-post (debug-process-run-semaphore process)))
    (void))
  
  
  (define (pause process)
    (print-info (format "pausing debugger for ~a" (main-client-name process)))
    (set-running! process #f))
  
  
  (define (start/resume process)
    (let ([val (frp:value-now (debug-process-exited? process))])
      (when (not (null? (debug-process-run-semaphore process)))
        (print-info (format "resuming debugger for ~a" (main-client-name process))))
      
      ; only start the debugger once for each process
      (if ((not (equal? val frp:undefined)) . and . val)
          (print-info (format "Cannot restart a process once it has exited (~a). Try restarting the script."
                              (main-client-name process)))
          (set-running! process #t))))
  
  ; Kills and prints out a message stating it
  (define (kill process)
    (print-info (format "killing debugger for ~a" (main-client-name process)))
    (stop process))
  
  ; Kills the debugger process immediately and permanently
  (define (stop process)
    ; remove the process from the process list
    (set! all-debug-processes (remq process all-debug-processes))
    
    (set-running! process #f)
    ; shutdown the custodian
    (custodian-shutdown-all (debug-process-custodian process))
    ; set the exit predicate to 'exited'
    (frp:set-cell! (debug-process-exited? process) #t))
  
  
  ; creates and initializes a debug process
  (define (create-debug-process)
    (let ([p (create-empty-debug-process)])
      (set-debug-process-runtime! p (runtime p))
      (set! all-debug-processes (cons p all-debug-processes))
      p))
  
  
  ; returns a behavior that keeps track of runtime
  (define (runtime process)
    (frp:hold
     ((frp:changes
       (frp:accum-b
        ((frp:changes frp:milliseconds)
         . frp:-=> .
         (match-lambda [(prev sum)
                        (if (frp:value-now (debug-process-running? process))
                            (list (frp:value-now frp:milliseconds)
                                  (+ (- (frp:value-now frp:milliseconds) prev) sum))
                            (list (frp:value-now frp:milliseconds) sum))]))
        (list (frp:value-now frp:milliseconds) 0)))
      . frp:==> .
      cadr) ; take the second element
     0))
  
  ;###########################################################################################################
  
  
  
  
  ;    ;;;;;                ;            ;        ;;;;;;;                                 
  ;   ;     ;               ;            ;        ;                                       
  ;  ;                                   ;        ;                                       
  ;  ;          ;;;  ; ;;;  ;  ; ;;;;   ;;;;;     ;       ;      ; ; ;;;;     ;;;   ;;;;  
  ;  ;         ;   ; ;;     ;  ;;    ;   ;        ;       ;      ; ;;    ;   ;   ; ;    ; 
  ;   ;;;     ;      ;      ;  ;      ;  ;        ;;;;;;; ;      ; ;      ; ;      ;      
  ;      ;;;  ;      ;      ;  ;      ;  ;        ;       ;      ; ;      ; ;      ;      
  ;         ; ;      ;      ;  ;      ;  ;        ;       ;      ; ;      ; ;       ;;;;  
  ;         ; ;      ;      ;  ;      ;  ;        ;       ;      ; ;      ; ;           ; 
  ;         ; ;      ;      ;  ;      ;  ;        ;       ;      ; ;      ; ;           ; 
  ;  ;     ;   ;   ; ;      ;  ;     ;   ;        ;        ;    ;; ;      ;  ;   ; ;    ; 
  ;   ;;;;;     ;;;  ;      ;  ;;;;;;     ;;;     ;         ;;;; ; ;      ;   ;;;   ;;;;  
  ;                            ;                                                          
  ;                            ;                                                          
  ;                            ;                                                          
  
  
  #;(define (running? process)
      (script-error "client-running? is broken")
      (and (running-now? process)
           (not (debug-process-exited? process))))
  
  #;(define (time-per-event/milliseconds process behavior)
      (frp:lift (truncate (/ (frp:value-now (debug-process-runtime process))
                             (add1 (frp:value-now (count-e (frp:changes behavior))))))))
  
  (define (runtime/milliseconds process)
    (debug-process-runtime process))
  
  (define (runtime/seconds process)
    (frp:hold ((frp:changes (debug-process-runtime process))
               . frp:==> .
               (lambda (t) (truncate (/ t 1000))))
              0))
  
  
  ; Creates a debugger client
  ; (debug-process? require-path. -> . debug-file?)
  (define (create-debug-client process filename)
    ; throwaway namespace so the module-name-resolver doesn't load an unannotated module
    (parameterize ([current-namespace (make-namespace)])
      (with-handlers ([exn:fail?
                       (lambda (exn)
                         (client-error (format "Expected a module in client file: ~a" filename)))])
        
        (let* ([build-module-filename ; taken from module-overview.ss
                (lambda (str)
                  (let ([try (lambda (ext)
                               (let ([tst (string-append str ext)])
                                 (and (file-exists? tst) tst)))])
                    (or (try ".ss") (try ".scm") (try "") str)))]
               
               [modpath (symbol->string ((current-module-name-resolver) filename #f #f))]
               [modpath (build-module-filename
                         (if (regexp-match #rx"^," modpath)
                             (substring modpath 1 (string-length modpath))
                             modpath))]
               
               [client (create-empty-debug-client)])
          (for-each (lambda (c)
                      (when (equal? modpath (debug-client-modpath c))
                        (raise-syntax-error 'mztake:script-error:create-debug-client
                                            (format "A client for `~a' is already defined for this process." modpath))))
                    (debug-process-clients process))
          
          (print-debug (format "'~a' -> '~a'" filename modpath))
          
          (set-debug-client-modpath! client modpath)
          (set-debug-client-process! client process)
          (set-debug-client-line-col->pos! client (line-col->pos modpath))
          (set-debug-process-clients! process
                                      (append (list client) (debug-process-clients process)))
          
          ; set the main module if it has not been set
          ; this implies that the first client created is always the main module
          (when (null? (debug-process-main-client process))
            (set-debug-process-main-client! process client))
          
          client))))
  
  
  ; (client (offset | line column) (symbol | listof symbols) -> (frp:event-receiver)
  ; (debug-client? number? number? (union symbol? (listof symbol?)) . -> . frp:event?)
  (define (trace/bind client line col binding-symbol)
    (when (empty? binding-symbol)
      (script-error (format "No symbols defined in BIND for client: `~a'"
                            (debug-client-modpath client))))
    
    (with-handlers ([(lambda (exn) #t)
                     (lambda (exn) (raise-syntax-error 'mztake:script-error:trace/bind exn))])
      (let ([trace-hash (debug-client-tracepoints client)]
            [trace (create-bind-trace binding-symbol)]
            [pos ((debug-client-line-col->pos client) line col)])
        ; add the trace to the list of traces for that byte-offset
        (hash-put! trace-hash pos
                   (append (hash-get trace-hash pos (lambda () '()))
                           (list trace)))
        (trace-struct-evnt-rcvr trace))))
  
  
  ;(debug-file? number? number? . -> . frp:event?)
  (define (trace/entry client line col)
    (let ([trace-hash (debug-client-tracepoints client)]
          [trace (create-entry-trace)]
          [pos ((debug-client-line-col->pos client) line col)])
      (hash-put! trace-hash pos
                 (append (hash-get trace-hash pos (lambda () '()))
                         (list trace)))
      (trace-struct-evnt-rcvr trace)))
  
  
  (provide create-trace
           create-debug-process
           create-debug-client
           mztake-version)
  
  ;###########################################################################################################
  
  )
