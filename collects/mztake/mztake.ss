#| TODO

offer a way to install a special handler for exceptions -- somehow identify which client an exceptions comes from

CONTRACT ALL SCRIPT FUNCTIONS

-------------------
does this work on binary drscheme files?

create client takes either a lib or relative or absolute path string

need client-error to throw an exception, they are all fatal

all errors are fatal now -- you can do this when

make all exposed cells and evstreams read-only by lifting the identity function on them

does this handle module prefixes?

what happens if two modules have the same name in different directories

WHY CANT REQUIRE TAKE AN ABSOLUTE PATH?
(require (lib "file.ss"))
  (find-relative-path (current-directory) "C:/Files/Desktop/debugger/src/collects/mztake/mztake.ss")

Need to find a way to map the absolute paths taken in from clients to the function that determines what to annotate.

MAKE SURE THERE WONT BE COLLISIONS WHEN EVAL'NG MODULES...GIVE THEM UNIQUE NAMES BASED ON PATH!
----------------



Remove client from runtime setters

------------------------------------------------------------------------------------------------------------------------
Problem:
When debugging multiple files ...

It is trivial to retrive values from from multiple files ... of course, you don't necessarily know the order that things will happen, and it may be difficult to relate the values correctly without writing a good debug script.  For instance, if debugging vector.ss, it may be used in multiple contexts in the file being debugged.  Can I provide a mechanism to turn breaks on and off based on other variables?  i.e. only register the breakpoint or send values on the eventstream IF a certain condition is met.

what happens when a break happens in multiple files -- how do you know when it happens -- what does it mean when it happens -- in a single thread I assume that if a break happens deep in execution, all execution halts there until the semaphore gets posted to...

Of course, you don't know where or why.

With multiple threads... It is hard to differentiate between the threads since we do simple annotation that doesn't send which namespace, thread, custodian, eventspace... it came from.  More importantly, how can multiple threads hold on the same semaphore?  When you post to it ... which gets the post ... should one or all continue?  Does it make sense for only one to continue, or should they all get posted too (most likely).  When you want to pause the program, do you want to pause all the threads? (yes...)  When you resume, resume all?

Otherwise if you stack them up, interact with one, resume, interact, resume... could be tedious.  Also, one may negate the necessity for the other.  What does it even mean to pause in vector.ss?

Problem with synchronysity in multiple threads too... Maybe we need to have the threaded breakpoints run in lockstep... Put a 'semaphore' *before* processing breakpoints to ensure that there is ever only one breakpoint happening at a time, and each thread waits in a queue.  Big performance impact though... Does this solve all of our other problems -- I think we would only need one semaphore to continue then, and pausing will be global.  Yes, if each thread causes a pause, then we will pause possibly annoyingly each time.  We need a way to turn forced breaks on and off dynamically then -- what do we do about behaviors in set-running!? (is set-running! global then?).  (does it make sense to be able to turn bind traces on and off too?)

Performance-wise, this turns a multi-threaded program into a single thread -- if there are t threads over an arbitrary number of modules, worst case is that you have t threads in the queue.

Last issue: Could this queue potentially cause a condition where threads A and B always get their breakpoints evaluated in the same order, a race condition, and they can't exit this until B comes before A?  Should I have some sort of random insertion that guarantees that everyone will get a chance, but does not guarantee order?
------------------------------------------------------------------------------------------------------------------------

With the syntax for debugging, you will not have to provide ways to create clients... they can only happen in one place at one time.!!!

Need to know where the program breaks at -- need to know *when* it breaks too -- print something out

I will want to be able to take "(lib ...)" as a path to the file being debugged

exceptions thrown in anonymous threads spawned by the target, are caught by the default drs handler, and not by frtime or mztake. they get printed out in the interaction window and there is nothing we can do about them for now -- if you want you can parameterize and rethrow the exceptions. just be aware of that.
RETHROW EXCEPTIONS CAUGHT ON THE STREAM FOR A CLIENT -- OFFER A WAY TO DISABLE IT
WHO is catching (thread (lambda () (raise 'first-raise)))?  It never gets to the exn evstream

CAN I CATCH FRTIME EXCEPTIONS AND RETHROW THOSE TOO?

code like
(set-running! client (or (elapsed . < . 5) (elapsed . >= . 10)))
(set-running! client #t)
will break the behavior ... it gets set!'d.  Fix this!
set-running! behaviors can go in a list which is and'd to check if all are satisfied


DEMOS---------------------------------------------------------------------------------------
Data structure examples
Binary search over a tree, show which node is being examined, or the most commonly taken path
Parse, graph the AST -- show OR and AND precedence getting messed up

MST example

Code something with multiple threads doing something and draw the threads in different colors in a window

code the heap example and copy the set-running! coolness to it from sine-test.ss

SCRIPT--------------------------------------------------------------------------------------
provide a running? behavior for the scripts, which actually works.

make (script-error) map to some exception stream for script errors only.
For now it is a synonym for (display)
Find a way to signal the error outside of FrTime's eventspace so that it doesnt loop the error endlessly

make script errors highlight the location of the error

let traces take a line number without offset and find the first bindable location.



OPTIMIZATIONS-------------------------------------------------------------------------------
improve speed of lookup for line-col->pos; load them into a hashtable?  not important since this is just startup time for the script.

improve speed of load/annotate



ERROR-CHECKING------------------------------------------------------------------------------
Make sure that you do not define more than one client for the same file.

Test what happens when you bind to variables that don't exist.

This throws an exception where it says something like random210 is an undefined variable
The script does not tell you something went wrong though, and the solution (as-is/unchecked) is not obvious.
(require (as-is mzscheme random random-seed))
(random 100)


TESTING/CAPABILITIES------------------------------------------------------------------------
Does user interaction work?  Can we step through loops one line at a time waiting for input?  GUIs?

We want a way to interactively step through code one line at a time when we hit a breakpoint.  Provide way to check bindings at the same time -- EVEN IF NOT BOUND USING TRACE/BIND

trace/bind what kind of interface do we want to dig into frames
write a nested syntax for bind so that you can take a first-class function that defines a way to return variables, not just as a list

What do we do about binding to a variable and following it EVERYWHERE it goes.  Even if it is assigned to something else.  Need to talk to Shriram, Greg, and Guillaume about this.

Find a way to bind to the result of ananonymous expression: here->(add1 2)
|#

(module mztake mzscheme
  (require (lib "match.ss")
           (lib "unit.ss")
           (lib "contract.ss")
           (lib "marks.ss" "stepper/private")
           (prefix frp: (lib "frp.ss" "frtime"))
           "private/useful-code.ss"
           "private/more-useful-code.ss" ; mostly for hash- bindings
           "mztake-structs.ss"
           "debugger-model.ss")
  
  ; Provides come from the script section at the bottom of the code
  (provide create-debug-process
           create-debug-client
           trace/bind
           trace/break
           start/resume
           kill
           kill-all
           pause
           (rename debug-process-exceptions process:exceptions)
           (rename runtime/seconds process:runtime/seconds)
           (rename runtime/milliseconds process:runtime/milliseconds)
           (rename debug-process-exited? process:exited?)
           #|
           set-running!
           process:running? ; disabled until it works
|#
           )
  
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
    (match result
      ; regular breakpoint
      [($ normal-breakpoint-info (top-mark rest-mark ...) kind)
       (print-debug "breakpoint hit")
       (void)]
      #|
       (let* ([byte-offset (sub1 (syntax-position (mark-source top-mark)))]
              [trace-hash (debug-file-tracepoints client)]
              [traces (hash-get trace-hash byte-offset)])
         
         (assert (not (empty? traces))
                 (format "There are no traces at offset ~a, but a breakpoint is defined!~n"
                         (number->string byte-offset)))
         
         ; Run all traces at this breakpoint               
         (let ([to-send (map (lambda (t) (trace->frp-event client result t)) traces)])
           (frp:send-synchronous-events to-send)))|#
      ; now, breakpoint-halt message should be sent by the debugger model
      
      ;TODO eventually remove this from debugger-model.ss
      [($ error-breakpoint-info (source exn))
       ; all errors and raises from the TARGET program will be caught here
       ; FrTime errors from the script have their own eventstream
       (frp:send-event (debug-process-exceptions process) exn)
       (client-error (if (exn? exn)
                         (format "source: ~a | exception: ~a" source (exn-message exn))
                         exn))]
      
      ;end of a statement
      [($ breakpoint-halt)
       ; do we want to pause interactive debugging
       (when (running-now? process)
         (semaphore-post (debug-process-run-semaphore process)))]
      
      ;when a top level expression finishes
      [($ expression-finished return-val-list) (void)]))
  
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
    (for-each (lambda (p) (kill p)) all-debug-processes)
    (display "All debug processes have been killed."))
  
  
  ; wrapper for errors related to the script only
  (define (script-error err)
    ; TODO I made this a syntax error so that the little 'goto' clickable box wouldnt show up
    ; it could easily be an (error)
    (display (format "mztake:script-error: ~a~n---~n" err))
    (kill-all))
  
  
  (define (client-error err)
    ; TODO I made this a syntax error so that the little 'goto' clickable box wouldnt show up
    ; it could easily be an (error)
    (display (format "mztake:client-error: ~a~n---~n" err))
    (kill-all))
  
  
  (define (print-debug str)
    (display (format "mztake:debug: ~a~n---~n" str)))
  
  
  ; retreives the binding of a variable from a breakpoint event
  (define (binding event sym)
    (let ([mark-list (normal-breakpoint-info-mark-list event)]
          [current-frame-num 0])
      (map (lambda (binding) (list (mark-binding-binding binding)
                                   (mark-binding-value binding)))
           (lookup-all-bindings (lambda (id) (eq? (syntax-e id) sym))
                                (do-n-times cdr current-frame-num mark-list)))))
  
  ; does something for (binding)
  (define (do-n-times fn n arg)
    (foldl (lambda (x arg) (fn arg)) arg (build-list n (lambda (x) x))))
  
  ; takes a single trace, looks up what it needs to do, and returns an frp-event to publish
  (define (trace->frp-event client event trace)
    (match trace
      [($ break-trace evnt-rcvr)
       #;(pause client)
         (list evnt-rcvr #t)]
      
      [($ bind-trace evnt-rcvr variable-to-bind)
       (let* ([vars (if (list? variable-to-bind) variable-to-bind
                        (list variable-to-bind))]
              [values (map (lambda (var)
                             (let ([val (binding event var)])
                               (if (empty? val)
                                   (script-error (format "No binding found in trace for symbol '~a" var))
                                   (cadar (binding event var)))))
                           vars)])
         (list evnt-rcvr
               (if (list? variable-to-bind) values
                   (first values))))]))
  
  ; TODO improve program expander
  (define ((program-expander filename) init callback)
    ;; (init) ; TODO now that's a bit of a hack.
    (parameterize ([port-count-lines-enabled #t])
      (let ([port (open-input-file filename)])
        (begin0
          (let loop ([stx (read-syntax filename port)])
            (unless (eof-object? stx)
              #;(print-debug (format "expanding: ~a" (syntax-object->datum (expand stx))))
              (callback
               (expand stx)
               (lambda () (loop (read-syntax filename port))))))
          (close-input-port port)))))
  
  
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
             (script-error (format "No syntax found for trace at line/column ~a:~a in ~a" line col filename))]
            
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
  
  (define (start-debug-process process)
    (let* ([receive-result (receive-result process)]
           ; connect to the debugger-model@ unit
           [run (invoke-unit debugger-model@ receive-result process)])
      
      ; initialize the semaphore
      (set-debug-process-run-semaphore! process (make-semaphore))
      ; set initial state of exit predicate
      (frp:set-cell! (debug-process-exited? process) #f)
      
      ; run the process
      (let ([evaluation-thread (thread (lambda () (run)))])
        (thread (lambda ()
                  (thread-wait evaluation-thread)
                  ; program terminates
                  (kill process))))))
  
  
  ; predicate - is the debugee supposed to be running now?
  (define (running-now? process)
    (and (not (null? (debug-process-run-semaphore process)))
         (frp:value-now (debug-process-running? process))))
  
  
  ; Switches the running state on or off
  ; ((union frp:behavior? boolean?) . -> . void?)
  (define (set-running! process run?)
    (define (update)
      ; start the debugger if needed
      (when (null? (debug-process-run-semaphore process))
        (start-debug-process process))
      
      (when run?
        (semaphore-post (debug-process-run-semaphore process)))
      
      (frp:value-now run?))
    
    (cond [(frp:behavior? run?)
           (script-error "set-running! can't take behaviors right now!")]
          ;(set! debugger:running? (frp:proc->signal update run?))]
          
          [else (set-debug-process-running?! process run?)
                (update)])
    (void))
  
  
  (define (pause process) (set-running! process #f))
  
  (define (start/resume process)
    (let ([val (frp:value-now (debug-process-exited? process))])
      ; only start the debugger once
      (if ((not (equal? val frp:undefined)) . and . val)
          (script-error "Cannot restart program once it has exited. Try restarting the script.")
          (set-running! process #t))))
  
  
  ; Kills the debugger process immediately
  (define (kill process)
    (pause process)
    ; shutdown the custodian
    (custodian-shutdown-all (debug-process-custodian process))
    ; set the exit predicate to 'exited'
    (frp:set-cell! (debug-process-exited? process) #t))
  
  
  ; creates and initializes a debug process
  (define (create-debug-process)
    (let ([p (create-empty-debug-process)])
      (set-debug-process-runtime! p (runtime p))
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
  
  
  (define (running? process)
    (script-error "client-running? is broken")
    (and (running-now? process)
         (not (debug-process-exited? process))))
  
  (define (runtime/milliseconds process)
    (debug-process-runtime process))
  
  ;TODO dont forget to contract this
  (define (runtime/seconds process)
    (frp:hold ((frp:changes (runtime/milliseconds process))
               . frp:==> .
               (lambda (t) (truncate (/ t 1000))))
              0))
  
  
  ; Creates a debugger client
  ; ((union (listof (union string? symbol?)) string?) . -> . debug-file?)
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
               
               [modsymbol ((current-module-name-resolver) filename #f #f)]
               
               [modpath (symbol->string modsymbol)]
               [modpath (build-module-filename
                         (if (regexp-match #rx"^," modpath)
                             (substring modpath 1 (string-length modpath))
                             modpath))]
               
               [client (create-empty-debug-client)])
          
          ;TODO remove me
          (print-debug (format "'~a' -> '~a'" filename modpath))
          
          (set-debug-client-modpath! client modpath)
          (set-debug-client-modsymbol! client modsymbol)
          (set-debug-client-process! client process)
          (set-debug-client-line-col->pos! client (line-col->pos filename))
          (set-debug-process-clients! process
                                      (cons client (debug-process-clients process)))
          
          client))))
  
  
  ; (client (offset | line column) (symbol | listof symbols) -> (frp:event-receiver)
  ; (debug-client? number? number? (union symbol? (listof symbol?)) . -> . frp:event?)
  (define (trace/bind client line col binding-symbol)
    (let ([trace-hash (debug-client-tracepoints client)]
          [trace (create-bind-trace binding-symbol)]
          [pos ((debug-client-line-col->pos client) line col)])
      ; add the trace to the list of traces for that byte-offset
      (hash-put! trace-hash pos
                 (cons trace
                       (hash-get trace-hash pos (lambda () '()))))
      (trace-struct-evnt-rcvr trace)))
  
  
  ;(debug-file? number? number? . -> . frp:event?)
  (define (trace/break client line col)
    (let ([trace-hash (debug-client-tracepoints client)]
          [trace (create-break-trace)]
          [pos ((debug-client-line-col->pos client) line col)])
      (hash-put! trace-hash pos
                 (cons trace
                       (hash-get trace-hash pos (lambda () '()))))
      (trace-struct-evnt-rcvr trace)))
  
  ;###########################################################################################################
  
  
  ;                                                 
  ;    ;;;;;                    ;                   
  ;   ;     ;                   ;                   
  ;  ;                          ;                   
  ;  ;        ;     ; ; ;;;;   ;;;;;  ;;;;  ;     ; 
  ;  ;        ;     ; ;;    ;   ;    ;    ;  ;   ;  
  ;   ;;;      ;   ;  ;      ;  ;         ;   ; ;   
  ;      ;;;   ;   ;  ;      ;  ;     ;;;;;   ; ;   
  ;         ;  ;   ;  ;      ;  ;    ;    ;    ;    
  ;         ;   ; ;   ;      ;  ;   ;     ;   ; ;   
  ;         ;   ; ;   ;      ;  ;   ;     ;   ; ;   
  ;  ;     ;     ;    ;      ;  ;   ;    ;;  ;   ;  
  ;   ;;;;;      ;    ;      ;   ;;; ;;;; ; ;     ; 
  ;              ;                                   
  ;              ;                                   
  ;              ;                                   
  
  #|
  (define-syntax bind
    (syntax-rules ()
      [(_ (arg ...) body ...)
       (trace () (arg ...) body ...)]
      
      [(_ (arg ...))
       (trace () (arg ...))]))
  
    
  (define-syntax trace
    (syntax-rules ()
      [(trace client line col . type)
       (print type)]))
  
  (define-syntax debugger
    (syntax-rules ()
      [(debug
        (process process-name
                 [client-name mod-path] ...)
        (traces [trace-name trace-client-name
                            (trace-type . trace-args) (trace-body ...)] ...)
        (run process-name 
             body ...))
        
        (printf "clients: ~a~nrun: ~a~nbody: ~a~n"
                '(clients [client-name client-path (traces [trace-name trace-client trace-type . trace-args] ...)] ...)
                '(run run-client-name)
                '(body ...))]))
  |#
  
  
  
  
  ;###########################################################################################################
  
  )