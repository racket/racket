#| TODO
I will want to be able to take "(lib ...)" as a path to the file being debugged

exceptions thrown in anonymous threads spawned by the target, are caught by the default drs handler, and not by frtime or mztake. they get printed out in the interaction window and there is nothing we can do about them for now -- if you want you can parameterize and rethrow the exceptions. just be aware of that.

CAN I CATCH FRTIME EXCEPTIONS AND RETHROW THOSE TOO?


When lifting for debugging a frtime program, you cannot lift in the target program, you have to lift in the script itself.  Everything else should be as-is/unchecked.

code like
(set-running! client (or (elapsed . < . 5) (elapsed . >= . 10)))
(set-running! client #t)
will break the behavior ... it gets set!'d.  Fix this!

rethink start/resume and kill -- do we want people to be able to restart after a kill or the progam exits (we have a behavior to check for that)?

RETHROW EXCEPTIONS CAUGHT ON THE STREAM FOR A CLIENT -- OFFER A WAY TO DISABLE IT
WHO is catching (thread (lambda () (raise 'first-raise)))?  It never gets to the exn evstream

You cannot annotate across requires. Test threads and multiple clients. Two files at the same time if they use each other?
DrScheme has hooks which should let me bind to the 'load's of libraries and force loading source, then annotating after expansion.

DEMOS---------------------------------------------------------------------------------------
Data structure examples
Binary search over a tree, show which node is being examined, or the most commonly taken path
Parse, graph the AST -- show OR and AND precedence getting messed up

MST example

Code something with multiple threads doing something and draw the threads in different colors in a window


SCRIPT--------------------------------------------------------------------------------------
provide a running? behavior for the scripts, which actually works.

make (script-error) map to some exception stream for script errors only.
For now it is a synonym for (display)
Find a way to signal the error outside of FrTime's eventspace so that it doesnt loop the error endlessly

add a trace/break function.

make script errors highlight the location of the error

let traces take a line number without offset and find the first bindable location.



OPTIMIZATIONS-------------------------------------------------------------------------------
get rid of references to stepper and move files like marks.ss over to debugger

improve speed of lookup for line-col->pos; load them into a hashtable?  not important since this is just startup time for the script.



ERROR-CHECKING------------------------------------------------------------------------------
Test what happens when you bind to variables that don't exist.

This throws an exception where it says something like random210 is an undefined variable
The script does not tell you something went wrong though, and the solution (as-is/unchecked) is not obvious.
(require (as-is mzscheme random random-seed))
(random 100)


TESTING/CAPABILITIES------------------------------------------------------------------------
Does user interaction work?  Can we step through loops one line at a time waiting for input?  GUIs?

Verify that when killing the debugger, all the memory and bindings that need to be released are released.

code the heap example and copy the set-running! coolness to it from sine-test.ss

We want a way to interactively step through code one line at a time when we hit a breakpoint.  Provide way to check bindings at the same time -- EVEN IF NOT BOUND USING TRACE/BIND

trace/bind what kind of interface do we want to dig into frames
write a nested syntax for bind so that you can take a first-class function that defines a way to return variables, not just as a list

What do we do about binding to a variable and following it EVERYWHERE it goes.  Even if it is assigned to something else.  Need to talk to Shriram, Greg, and Guillaume about this.

Find a way to bind to the result of ananonymous expression: here->(add1 2)
|#

(module mztake mzscheme
  (require (lib "match.ss")
           (lib "contract.ss")
           (lib "unitsig.ss")
           (lib "debugger-model.ss" "stepper" "private")
           (lib "marks.ss" "stepper" "private")
           "private/useful-code.ss"
           "private/more-useful-code.ss" ; mostly for hash- bindings
           (prefix frp: (lib "frp.ss" "frtime")))
  
  ; Provides come from the script section at the bottom of the code
  (provide (rename script:kill kill)
           (rename script:pause pause)
           (rename script:trace/bind trace/bind)
           (rename script:trace/break trace/break)
           (rename script:set-running! set-running!)
           (rename client-exited? client:exited?)
           (rename script:start/resume start/resume)
           (rename script:create client:create)
           (rename client-exceptions client:exceptions)
           (rename script:runtime-seconds client:runtime/seconds)
           (rename script:runtime-milliseconds client:runtime/milliseconds))
  ;(rename script-running? client-running?)) ; disabled until it works
  
  
  ;########################  STRUCTS  ########################
  
  (define-struct trace (evnt-rcvr))    ; frp:event-receiver
  
  (define-struct (break-trace trace) ())
  (define-struct (bind-trace trace)
                 (variable-to-bind))    ; symbol
  
  (define-struct client (filename       ; string
                         tracepoints    ; hash-table of traces
                         running?       ; boolean - is the program (supposed-to-be) currently running
                         custodian      ; if you shutdown-all it will kill the debugger
                         run-semaphore  ; when you post to this the debuggee will continue executing
                         exceptions     ; (an event stream) exceptions thrown during the evaluation of the target
                         exited?        ; (an cell) receives #t when the target exits
                         runtime        ; behavior with current runtime in milliseconds
                         line-col->pos)); memoized O(n) function to map line/col -> byte offset
  
  ;####################  STRUCT-BUILDERS  #####################
  
  ; Creates a trace that binds to the value of a variable in scope
  (define (create-bind-trace sym-to-bind) ; ((union (listof symbol?) symbol?) . -> . trace?)
    (make-bind-trace (frp:event-receiver) sym-to-bind))
  
  ; Creates a trace that simply pauses the program
  (define (create-break-trace) ; (void? . -> . trace?)
    (make-break-trace (frp:event-receiver)))
  
  ;#######################  CALLBACKS  #######################
  
  ; Callback for when a breakpoint (tracepoint) is hit by the model
  ; ((client) breakpoint-struct) -> ()
  (define ((receive-result client) result)
    (match result
      ; regular breakpoint
      [($ normal-breakpoint-info (top-mark rest-mark ...) kind)
       
       (let* ([byte-offset (sub1 (syntax-position (mark-source top-mark)))]
              [trace-hash (client-tracepoints client)]
              [traces (hash-get trace-hash byte-offset)])
         
         (assert (not (empty? traces))
                 (format "There are no traces at offset ~a, but a breakpoint is defined!"
                         (number->string byte-offset)))
         
         ; Run all traces at this breakpoint               
         (let ([to-send (map (lambda (t) (trace->frp-event client result t)) traces)])
           (frp:send-synchronous-events to-send)))]
      ; now, breakpoint-halt message should be sent by the debugger model
      
      ;TODO eventually remove this from debugger-model.ss
      [($ error-breakpoint-info message)
       (assert false)]
      
      ;end of a statement
      [($ breakpoint-halt)
       ; do we want to pause interactive debugging
       (when (running-now? client)
         (semaphore-post (client-run-semaphore client)))]
      
      ;when a top level expression finishes
      [($ expression-finished return-val-list) (void)]
      
      [else-struct
       (assert false)(printf "something else hit: ~a~n" else-struct)]))
  
  
  ;###################  DEBUGGER BACKEND  ####################
  
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
  
  ; wrapper for errors related to the script only
  (define (script-error err)
    ; TODO I made this a syntax error so that the little 'goto' clickable box wouldnt show up
    ; it could easily be an (error)
    (display (format "mztake:script-error: ~a~n" err)))
  
  (define (client-error err)
    ; TODO I made this a syntax error so that the little 'goto' clickable box wouldnt show up
    ; it could easily be an (error)
    (display (format "mztake:client-error: ~a~n" err)))
  
  (define (fatal-script-error err client)
    (script-error err)
    (script:kill client))
  
  ; takes a single trace, looks up what it needs to do, and returns an frp-event to publish
  (define (trace->frp-event client event trace)
    (match trace
      [($ break-trace evnt-rcvr)
       (script:pause client)
       (list evnt-rcvr #t)]
      
      [($ bind-trace evnt-rcvr variable-to-bind)
       (let* ([vars (if (list? variable-to-bind) variable-to-bind
                        (list variable-to-bind))]
              [values (map (lambda (var)
                             (let ([val (binding event var)])
                               (if (empty? val)
                                   (fatal-script-error (format "No binding found in trace for symbol '~a" var)
                                                       client)
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
              (printf "expanding: ~a~n~n" (syntax-object->datum (expand stx)))
              (callback
               (expand stx)
               (lambda () (loop (read-syntax filename port))))))
          (close-input-port port)))))
  
  
  (define (start-debugger client)
    (let* ([breakpoint-origin (client-filename client)]
           [breakpoints (hash-keys (client-tracepoints client))]
           [program-expander (program-expander breakpoint-origin)]
           [receive-result (receive-result client)])
      
      ; connect to the debugger-model@ unit
      (define-values/invoke-unit/sig (go go-semaphore user-custodian)
                                     debugger-model@
                                     #f ; prefix
                                     (receive-result)
                                     (program-expander)
                                     ; breakpoint-origin = filename from thunk of (program-expander)
                                     (breakpoints breakpoint-origin))
      
      ; set initial state of exit predicate
      (frp:set-cell! (client-exited? client) #f)
      
      (set-client-run-semaphore! client go-semaphore)
      
      (set-client-custodian! client user-custodian)
      
      ; we run the program under its own custodian so we can easily kill it...that's IT
      
      
      (let ([evaluation-thread
             (parameterize ([current-custodian user-custodian])
               (thread
                (lambda ()
                  ; all errors and raises from the TARGET program will be caught here
                  ; FrTime errors from the script have their own eventstream
                  (with-handlers ([(lambda (exn) #t)
                                   (lambda (exn)
                                     (frp:send-event (client-exceptions client) exn)
                                     (client-error (if (exn? exn)
                                                       (format "exception: ~a" (exn-message exn))
                                                       exn)))])
                    (go)))))])
        (thread (lambda () 
                  (thread-wait evaluation-thread)
                  ; program terminates
                  (script:kill client))))))
  
  
  ; returns a memoized function that takes (line column) -> position
  (define/contract line-col->pos (client? . -> . (number? number? . -> . (union void? number?)))
    (lambda (client)
      (let ([filename (client-filename client)])
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
                 (fatal-script-error (format "No syntax found for trace at line/column ~a:~a in ~a" line col filename)
                                     client)]
                
                ; if first is correct line and correct column
                [(and (= line (caar lst))
                      (= col (cadar lst)))
                 (third (first lst))]
                
                [else (loop (rest lst)
                            (first lst))])))))))
  
  ; predicate - is the debugee supposed to be running now?
  (define (running-now? client)
    (and (not (null? (client-run-semaphore client)))
         (frp:value-now (client-running? client))))
  
  ; returns a behavior for a client counting runtime
  ; this is set!'d into the client struct so that it is always accurate
  (define (runtime c)
    (frp:hold
     ((frp:changes
       (frp:accum-b
        ((frp:changes frp:milliseconds)
         . frp:-=> .
         (match-lambda [(prev sum)
                        (if (frp:value-now (client-running? c))
                            (list (frp:value-now frp:milliseconds)
                                  (+ (- (frp:value-now frp:milliseconds) prev) sum))
                            (list (frp:value-now frp:milliseconds) sum))]))
        (list (frp:value-now frp:milliseconds) 0)))
      . frp:==> .
      second)
     0))
  
  ;####################  SCRIPT FUNCTIONS  ###################
  
  
  ; Switches the running state on or off
  ; (client [boolean]) -> ()
  (define/contract script:set-running! (client? (union frp:behavior? boolean?) . -> . void?)
    (lambda (client run?)
      (define (update)
        ; (re)start the debugger if needed
        (when (null? (client-run-semaphore client)) (start-debugger client))
        (when run? (semaphore-post (client-run-semaphore client)))
        (frp:value-now run?))
      
      (cond [(frp:behavior? run?)
             (set-client-running?! client (frp:proc->signal update run?))]
            [else (set-client-running?! client run?)
                  (update)])
      (void)))
  
  
  ;dont forget to contract this
  (define script:running?
    (lambda (client)
      (print "client-running? is broken")
      (and (running-now? client)
           (not (client-exited? client)))))
  
  (define/contract script:runtime-milliseconds (client? . -> . frp:behavior?)
    (lambda (client)
      (client-runtime client)))
  
  (define/contract script:runtime-seconds (client? . -> . frp:behavior?)
    (lambda (client)
      (frp:hold ((frp:changes (client-runtime client))
                 . frp:==> .
                 (lambda (t) (truncate (/ t 1000))))
                0)))
  
  ; Creates a debugger client
  ; (string) -> (client)
  (define/contract script:create (string? . -> . client?)
    (lambda (filename)
      (let ([c (make-client filename (make-hash) #f null null
                            (frp:event-receiver) (frp:new-cell) null null)])
        ; set curried line-col->pos function for client
        (set-client-line-col->pos! c (line-col->pos c))
        ; set the runtime info (runtime-evs, time-behavior)
        (set-client-runtime! c (runtime c))
        c)))
  
  (define (script:pause c) (script:set-running! c #f))
  (define (script:start/resume c) (script:set-running! c #t))
  
  ; Kills the debugger immediately
  ; (client) -> ()
  (define/contract script:kill (client? . -> . void?)
    (lambda (client)
      (script:pause client)
      
      ; shutdown the custodian
      (custodian-shutdown-all (client-custodian client))
      (set-client-custodian! client null)
      (set-client-run-semaphore! client null)
      ; set the exit predicate to 'exited'
      (frp:set-cell! (client-exited? client) #t)))
  
  ; (client (offset | line column) (symbol | listof symbols) -> (frp:event-receiver)
  (define/contract script:trace/bind (client? number? number? (union symbol? (listof symbol?)) . -> . frp:event?)
    (lambda (client line col binding-symbol)      
      (let ([trace-hash (client-tracepoints client)]
            [trace (create-bind-trace binding-symbol)]
            [pos ((client-line-col->pos client) line col)])
        ; add the trace to the list of traces for that byte-offset
        (hash-put! trace-hash pos
                   (cons trace
                         (hash-get trace-hash pos (lambda () '()))))
        (trace-evnt-rcvr trace))))
  
  (define/contract script:trace/break (client? number? number? . -> . frp:event?)
    (lambda (client line col)
      (let ([trace-hash (client-tracepoints client)]
            [trace (create-break-trace)]
            [pos ((client-line-col->pos client) line col)])
        (hash-put! trace-hash pos
                   (cons trace
                         (hash-get trace-hash pos (lambda () '()))))
        (trace-evnt-rcvr trace))))
  )