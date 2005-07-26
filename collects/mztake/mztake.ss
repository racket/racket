(module mztake mzscheme
  (require (lib "contract.ss")
           (prefix frp: (lib "frp.ss" "frtime"))
           (rename (lib "frtime.ss" "frtime") frp:value-nowable? value-nowable?)
           (rename (lib "frtime.ss" "frtime") frp:behaviorof behaviorof)
           "mztake-structs.ss"
           (lib "etc.ss")
           (lib "list.ss")
           (lib "marks.ss" "mztake" "private")
           "engine.ss")
  
  (provide loc$ loc loc-reqspec loc-line loc-col
           trace trace* bind define/bind define/bind-e where set-main!)
  (provide/contract [kill (() (debug-process?) . opt-> . void?)]
                    [kill-all (-> void?)]
                    [set-running-e! (frp:event? . -> . void?)]
                    [set-running! (frp:value-nowable? . -> . void?)]
                    [exceptions (() (debug-process?) . opt-> . frp:event?)]
                    [exited? (() (debug-process?) . opt-> . frp:behavior?)])
  
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
        (set-debug-process-where! p (frp:new-cell empty)))
      (debug-process-where p)))
  
  (define current-process (make-parameter (create-debug-process)))
  
  (define set-main!
    (opt-lambda (reqspec [p (current-process)])
      (process:set-main! p reqspec)))
  
  (define (hold-b b)
    (frp:hold (frp:filter-e (lambda (ev) (not (frp:undefined? ev))) (frp:changes b))))
  
  (define-syntax trace
    (syntax-rules ()
      [(_ loc)
       (trace* (current-process) loc (lambda () true))]
      [(_ loc body ...)
       (trace* (current-process) loc (lambda () body ...))]))
  
  (define (bind* p name)
    (mark-binding-value
     (first (lookup-all-bindings
             (lambda (id) (eq? (syntax-e id) name))
             (debug-process-marks p)))))
  
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