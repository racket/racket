(module mztake mzscheme
  (require (lib "contract.ss")
           (prefix frp: (lib "lang-ext.ss" "frtime"))
           (rename (lib "frtime.ss" "frtime") frp:list list)
           (rename (lib "frtime.ss" "frtime") frp:value-nowable? value-nowable?)
           (rename (lib "frtime.ss" "frtime") frp:behaviorof behaviorof)
           "mztake-structs.ss"
           (lib "etc.ss")
           (lib "list.ss")
           "marks.ss"
           "engine.ss")

  ;; Turn struct printing on for MzTake users.
  (print-struct true)

  (provide loc$
           trace
           bind
           define/bind
           define/bind-e
           [rename mztake-top #%top])
  
  (provide/contract [loc-reqspec (loc? . -> . require-spec?)]
                    [loc-line (loc? . -> . number?)]
                    [loc-col (loc? . -> . number?)]
                    [rename loc/opt-col loc
                            ((any/c number?) (number?) . opt-> . loc?)]
                    [exceptions (() (debug-process?) . opt-> . frp:event?)]
                    [exited? (() (debug-process?) . opt-> . frp:behavior?)]
                    [kill (() (debug-process?) . opt-> . void?)]
                    [kill-all (-> void?)]
                    [set-running-e! ((frp:event?) (debug-process?) . opt-> . any)]
                    [set-running! ((frp:value-nowable?) (debug-process?) . opt-> . any)]
                    [where (() (debug-process?) . opt-> . frp:event?)]
                    [current-policy (case-> (-> any)
                                            (any/c . -> . void?))]
                    [current-process (case-> (-> debug-process?)
                                             (debug-process? . -> . void?))]
                    [create-debug-process (-> debug-process?)]
                    [set-main! ((require-spec?) (debug-process?) . opt-> . void?)]
                    [trace* (debug-process? loc? (-> any) . -> . frp:event?)]
                    [bind* (debug-process? symbol? . -> . any)])
  
  (define loc/opt-col
    (opt-lambda (reqspec line [col #f])
      (loc reqspec line col)))
  
  (define exceptions
    (opt-lambda ([p (current-process)])
      (debug-process-exceptions p)))  
  
  (define exited?
    (opt-lambda ([p (current-process)])
      (debug-process-exited? p)))
  
  (define kill
    (opt-lambda ([p (current-process)])
      (unless (debug-process-exited? p)
        (process:->dead p))))
  
  (define (kill-all)
    (unless (empty? all-debug-processes)
      (for-each (lambda (p) (kill p)) all-debug-processes)
      (display "All debug processes have been killed.")))
    
  (define set-running-e!
    (opt-lambda (e [process (current-process)])
      (resume process)
      (frp:set-cell! (debug-process-running-e process) e)))
  
  (define set-running!
    (opt-lambda (b [process (current-process)])
      (if (frp:value-now b) (resume process) (pause process))
      (frp:set-cell! (debug-process-running-e process) (frp:changes b))))
    
  (define where
    (opt-lambda ([p (current-process)])
      (unless (debug-process-where p)
        (set-debug-process-where! p (frp:event-receiver)))
      (debug-process-where p)))
  
  (define current-process (make-parameter (create-debug-process)))
  
  (define set-main!
    (opt-lambda (reqspec [p (current-process)])
      (process:set-main! p reqspec)))
  
  (define-syntax trace
    (syntax-rules ()
      [(_ loc)
       (trace* (current-process) loc (lambda () true))]
      [(_ loc body ...)
       (trace* (current-process) loc (lambda () body ...))]))
  
  (define-syntax (mztake-top stx)
    (syntax-case stx () 
      [(_ . name) 
       (begin 
         #'(with-handlers
               ([exn:fail?
                 (lambda (exn)
                   (with-handlers
                       ([exn:fail? (lambda (exn2) (raise exn))])
                     (bind* (current-process) 'name)))])
             (#%top . name)))]))
  
  (define (bind* p name)
    (unless (debug-process-marks p)
      (error "Bind called but the target process is not paused."))

    (let ([bs (lookup-all-bindings
               (lambda (id) (eq? (syntax-e id) name))
               (debug-process-marks p))])
      (when (empty? bs)
        (error 'bind "variable `~a' not found in target at the current location" name))
      
      (mark-binding-value (first bs))))
  
  (define-syntax bind
    (syntax-rules ()
      [(_ (name ...) body0 body ...)
       (let* ([p (current-process)]
              [name (bind* p 'name)]
              ...)
         body0 body ...)]))
  
  (define-syntax define/bind-e
    (syntax-rules ()
      [(_ loc name ...) 
       (begin 
         (define here loc)
         (define name (trace here (bind (name) name)))
         ...)]))
  
  (define-syntax define/bind
    (syntax-rules ()
      [(_ loc name ...) 
       (begin 
         (define here loc)
         (define name (frp:hold (trace here (bind (name) name))))
         ...)]))
  
  
  )                                                                                                                     