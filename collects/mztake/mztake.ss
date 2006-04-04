(module mztake mzscheme
  (require (lib "contract.ss")
           (lib "match.ss")
           (prefix frp: (lib "lang-ext.ss" "frtime"))
           (rename (lib "frtime.ss" "frtime") frp:list list)
           (rename (lib "frtime.ss" "frtime") frp:value-nowable? value-nowable?)
           (rename (lib "frtime.ss" "frtime") frp:behaviorof behaviorof)
           "mztake-structs.ss"
           (lib "base-gm.ss" "frtime")
           (lib "etc.ss")
           (lib "list.ss")
           "marks.ss"
           "engine.ss")

  ;; Turn struct printing on for MzTake users.
  (print-struct true)

  (provide (rename loc loc$)
           debug-process-running-e 
           loc/r
           trace
           bind
           define/bind
           define/bind-e
           [rename loc/opt-col loc]
           [rename mztake-top #%top])
  
  (provide/contract [exceptions (() (debug-process?) . opt-> . frp:event?)]
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
                    [current-reqspec (case-> (-> string?)
                                          (string? . -> . void?))]  
                    [create-debug-process (-> debug-process?)]
                    [set-main! ((require-spec?) (debug-process?) . opt-> . void?)]
                    [trace* (debug-process? loc? (-> any) . -> . frp:event?)]
                    [bind* (debug-process? symbol? . -> . any)])
  
  (define (loc* after?)
    (define (set-r r) (current-reqspec r))
    (match-lambda*
      [(arg) ((loc* after?) (current-reqspec) arg)]
      [((and (not (? require-spec?)) arg) args ...) (apply (loc* after?) (current-reqspec) arg args)]
      [((? require-spec? r) (? number? line)) (set-r r) (make-loc/lc r after? line false)]
      [((? require-spec? r) (? number? line) (? number? col)) (set-r r) (make-loc/lc r after? line col)]
      [((? require-spec? r) pattern) (set-r r) (make-loc/p r after? pattern)]))
  
  (define loc/r (loc* true))
  
  (define loc/opt-col (loc* false))
  
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
  (define current-reqspec (make-parameter false))
  
  (define set-main!
    (opt-lambda (reqspec [p (current-process)])
      (current-reqspec  reqspec )
      (process:set-main! p reqspec)))
  
  (define-syntax trace
    (syntax-rules (=>)
      [(_ loc)
       (let ([loc* loc])
         (if (loc-after? loc*)
             (trace* (current-process) loc* identity)
             (trace* (current-process) loc* (lambda () true))))]
      [(_ loc => proc)
       (trace* (current-process) loc proc)]
      [(_ loc body ...)
       (trace* (current-process) loc (lambda () body ...))]))
  
  (define (mztake-top* name thunk)
    (if (debug-process-marks (current-process))
        (with-handlers
            ([exn:fail?
              (lambda (exn)
                (with-handlers
                    ([exn:fail? (lambda (exn2) (raise exn2))])
                  (bind* (current-process) name)))])
          (thunk))
        (thunk)))
  
  (define-syntax (mztake-top stx)
    (syntax-case stx () 
      [(_ . name) 
       #'(mztake-top* 'name (lambda () (#%top . name)))]))
  
  (define (lookup-in-top-level p name)
    (let/ec success
      (define (try m)
        (let/ec fail
          (define (fail*) (fail false))
          (success (hash-get (hash-get (debug-process-top-level p) m fail*) name fail*))))
      (for-each try (map mark-module-name (debug-process-marks p)))
      (hash-for-each (debug-process-top-level p) (lambda (m ns) (try m)))
      (error 'bind "variable `~a' not found in target at the current location" name)))
  
  (define (bind* p name)
    (unless (debug-process-marks p)
      (error "Bind called but the target process is not paused."))

    (let ([bs (lookup-all-bindings
               (lambda (id) (eq? (syntax-e id) name))
               (debug-process-marks p))])
      (if (empty? bs)
          (lookup-in-top-level p name)
          (mark-binding-value (first bs)))))
  
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