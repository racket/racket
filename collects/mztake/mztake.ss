#| TODO

:::::::::LOAD/ANNOTATOR BUGS::::::::::::::
* catch oops exception
* catch the other two exceptions that my loaders throw
* detect if the source code for a certain module is missing and throw an exception
* do I want to parameterize it over a given namespace?
* does this handle module prefixes?
* what happens if two modules have the same name in different directories
* MAKE SURE THERE WONT BE COLLISIONS WHEN EVAL'NG MODULES...GIVE THEM UNIQUE NAMES BASED ON PATH!
::::::::::::::::::::::::::::::::::::::::::


DEMOS---------------------------------------------------------------------------------------
* Data structure examples
  Binary search over a tree, show which node is being examined, or the most commonly taken path
  Parse, graph the AST -- show OR and AND precedence getting messed up

* MST example

* something with multiple threads doing something and draw the threads in different colors in a window

* FIX heap example -- give greg new heap.ss


SCRIPT--------------------------------------------------------------------------------------
* document history-e; provide a variant of history which takes no n, and keeps a complete history

* process:time-per-event/milliseconds is broken
  (printf-b "~a ms per event" (time-per-event/milliseconds p (changes (hold sin/x-trace))))

* process:running? is broken

* Make script errors highlight the location of the error

* Let traces take a line number without offset and find the first bindable location.

* Provide a body to bind instead or returning an eventstream, like (list x y)
  Write a nested syntax for bind so that you can take a first-class function that defines a way to return variables, not just as a list

* Maybe take a thunk to do when a break-point is hit?

* Way to turn printouts on and off like (print-struct #t), or should we have an output window? (mztake-verbose) (parameterize it?)


OPTIMIZATIONS-------------------------------------------------------------------------------
* improve speed of lookup for line-col->pos; load them into a hashtable?  not important since this is just startup time for the script.

* improve speed of load/annotate

* improve speed of functions in (run)


ERROR-CHECKING/HANDLING---------------------------------------------------------------------
* Make (script-error) map to some exception stream for script errors only.

* Make all exposed cells and evstreams read-only by lifting the identity function on them

* Turn script errors into syntax errors (i.e. what happens when you bind to variables that don't exist)
    --take the syntax when the binding is made and save it in a hashtable

* Offer a way to install a special handler for exceptions -- somehow identify which client an exceptions comes from


TESTING/CAPABILITIES------------------------------------------------------------------------
* Does user interaction work?  Can we step through loops one line at a time waiting for input?  GUIs?

* We want a way to interactively step through code one line at a time when we hit a breakpoint.
  Provide way to check bindings at the same time -- EVEN IF NOT BOUND USING TRACE/BIND

* What kind of interface do we want to dig into frames

* Need to know where the program breaks at -- need to know *when* it breaks too -- print something out

* What do we do about binding to a variable and following it EVERYWHERE it goes.  Even if it is assigned to something else.

* Find a way to bind to the result of ananonymous expression: here->(add1 2)
|#

(module mztake mzscheme
  (require (lib "match.ss")
           (lib "unit.ss")
           (lib "contract.ss")
           (lib "marks.ss" "stepper" "private")
           (prefix frp: (lib "frp.ss" "frtime"))
           "private/useful-code.ss"
           "private/more-useful-code.ss" ; mostly for hash- bindings
           "mztake-structs.ss"
           "debugger-model.ss")
  
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
  
  ; 
  (define mztake-version "Rev. Wed Aug 4, 2004 - 17:06:00")
  
  ;###########################################################################################################
    
  
  ;                     ;   ;  ;                       ;              
  ;     ;;;;;           ;   ;  ;                       ;              
  ;   ;;     ;          ;   ;  ;                       ;              
  ;   ;                 ;   ;  ;                       ;              
  ;  ;           ;;;;   ;   ;  ; ;;;;     ;;;;    ;;;  ;    ;   ;;;;  
  ;  ;          ;    ;  ;   ;  ;;    ;   ;    ;  ;   ; ;   ;   ;    ; 
  ;  ;               ;  ;   ;  ;      ;       ; ;      ;  ;    ;      
  ;  ;           ;;;;;  ;   ;  ;      ;   ;;;;; ;      ; ;     ;      
  ;  ;          ;    ;  ;   ;  ;      ;  ;    ; ;      ;;;      ;;;;  
  ;  ;         ;     ;  ;   ;  ;      ; ;     ; ;      ;  ;         ; 
  ;   ;        ;     ;  ;   ;  ;      ; ;     ; ;      ;   ;        ; 
  ;   ;;     ; ;    ;;  ;   ;  ;     ;  ;    ;;  ;   ; ;    ;  ;    ; 
  ;     ;;;;;   ;;;; ;  ;   ;  ;;;;;;    ;;;; ;   ;;;  ;     ;  ;;;;  
  ;                                                                   
  
  ; Callback for when a breakpoint (tracepoint) is hit by the model
  ; ((client) breakpoint-struct) -> ()
  (define ((receive-result process) result)
    
    ; Before we process the trace, see if we are supposed to pause
    (unless (running-now? process)
      (semaphore-wait (debug-process-run-semaphore process)))
    
    (match result
      ; regular breakpoint
      [($ normal-breakpoint-info (top-mark rest-mark ...) client)
       (let* ([byte-offset (sub1 (syntax-position (mark-source top-mark)))]
              [traces (hash-get (debug-client-tracepoints client) byte-offset)])
         
         (assert (not (empty? traces))
                 (format "There are no traces at offset ~a, but a breakpoint is defined!~n"
                         (number->string byte-offset)))
         
         ; Run all traces at this breakpoint               
         (let ([to-send (map (lambda (t) (trace->frp-event client result t)) traces)])
           (frp:send-synchronous-events to-send))
         
         ; Now that we processed the trace, do we want to pause or continue
         (when (running-now? process)
           (semaphore-post (debug-process-run-semaphore process))))]
      
      [($ error-breakpoint-info (source exn))
       ; all errors and raises from the TARGET program will be caught here
       ; FrTime errors from the script have their own eventstream
       (frp:send-event (debug-process-exceptions process) exn)
       (client-error (format "source: ~a | exception: ~a" source (if (exn? exn) (exn-message exn) exn)))]))
  
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
    (raise-syntax-error 'mztake:script-error: (format "~a" err))
    (kill-all))
  
  
  (define (client-error err)
    (display (format "mztake:client-error: ~a~n---~n" err))
    (kill-all))
  
  
  (define (print-debug str)
    (when debugging?
      (display (format "mztake:debug: ~a~n---~n" str))))
  
  
  (define (print-info str)
    (display (format "mztake: ~a~n---~n" str)))
  
  
  ; retreives the binding of a variable from a breakpoint event
  (define (binding event sym)
    (define (do-n-times fn n arg)
      (foldl (lambda (x arg) (fn arg)) arg (build-list n (lambda (x) x))))
    
    (let ([mark-list (normal-breakpoint-info-mark-list event)]
          [current-frame-num 0])
      (map (lambda (binding) (list (mark-binding-binding binding)
                                   (mark-binding-value binding)))
           (lookup-all-bindings (lambda (id) (eq? (syntax-e id) sym))
                                (do-n-times cdr current-frame-num mark-list)))))
  
  
  (define create-trace
    (case-lambda
      [(client line col type args)
       (case type
         ['bind  (trace/bind client line col args)]
         ['break (trace/break client line col)])]
      
      [(client line col type)
       (create-trace client line col type null)]))
  
  
  ; takes a single trace, looks up what it needs to do, and returns an frp-event to publish
  (define (trace->frp-event client event trace)
    (match trace
      [($ break-trace evnt-rcvr)
       (list evnt-rcvr #t)]
      
      [($ bind-trace evnt-rcvr variable-to-bind)
       (let* ([vars (if (list? variable-to-bind) variable-to-bind
                        (list variable-to-bind))]
              [values (map (lambda (var)
                             (let ([val (binding event var)])
                               (if (empty? val)
                                   (script-error (format "trace/bind: No binding found in trace for symbol '~a" var))
                                   (cadar (binding event var)))))
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
             (raise (format "No syntax found for trace at line/column ~a:~a in ~a" line col filename))]
            
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
  
  (define (start-debug-process receive-result process)    
    (assert (not (null? (debug-process-main-client process)))
            "main-client not defined for one of the processes!")
    
    ; initialize the semaphore
    (set-debug-process-run-semaphore! process (make-semaphore))
    ; set initial state of exit predicate
    (frp:set-cell! (debug-process-exited? process) #f)    
    
    (thread (lambda ()
              ; connect to the debugger-model@ unit
              (define-values/invoke-unit (run) debugger-model@ #f receive-result process)
              ; run the process
              (thread-wait (thread (lambda () (run))))
              ; program terminates
              (stop process)
              (print-info (format "process exited normally: ~a" (main-client-name process))))))
  
  
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
      (start-debug-process (receive-result process) process))
    
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
      (with-handlers ([exn:module?
                       (lambda (exn)
                         (client-error (format "Expected a module in client: ~a" filename)))])
        
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
    (with-handlers ([(lambda (exn) #t)
                     (lambda (exn) (raise-syntax-error 'mztake:script-error:trace/bind exn))])
      (let ([trace-hash (debug-client-tracepoints client)]
            [trace (create-bind-trace binding-symbol)]
            [pos ((debug-client-line-col->pos client) line col)])
        ; add the trace to the list of traces for that byte-offset
        (hash-put! trace-hash pos
                   (cons trace
                         (hash-get trace-hash pos (lambda () '()))))
        (trace-struct-evnt-rcvr trace))))
  
  
  ;(debug-file? number? number? . -> . frp:event?)
  (define (trace/break client line col)
    (let ([trace-hash (debug-client-tracepoints client)]
          [trace (create-break-trace)]
          [pos ((debug-client-line-col->pos client) line col)])
      (hash-put! trace-hash pos
                 (cons trace
                       (hash-get trace-hash pos (lambda () '()))))
      (trace-struct-evnt-rcvr trace)))
  
  
  (provide create-trace
           create-debug-process
           create-debug-client
           mztake-version)
  
  ;###########################################################################################################
  
  )