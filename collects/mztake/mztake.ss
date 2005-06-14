(module mztake mzscheme
  
  (define mztake-version "rev. 8/6/2004")
  
  (require (lib "match.ss")
           (lib "contract.ss")
           (lib "marks.ss" "mztake" "private")
           (prefix frp: (lib "frp.ss" "frtime"))
           (rename (lib "frtime.ss" "frtime") frp:value-nowable? value-nowable?)
           (rename (lib "frtime.ss" "frtime") frp:behaviorof behaviorof)
           (lib "useful-code.ss" "mztake" "private")
           (lib "more-useful-code.ss" "mztake" "private") ; mostly for hash- bindings
           "mztake-structs.ss"
           (lib "load-annotator.ss" "mztake" "private")
           "annotator.ss"
           )
  
  (provide/contract [kill (() (debug-process?) . opt-> . void?)]
                    [kill-all (-> void?)]
                    [pause (() (debug-process?) . opt-> . void?)]
                    [resume (debug-process? . -> . void?)]
                    [set-running-e! (frp:event? . -> . void?)]
                    [set-running! (frp:value-nowable? . -> . void?)]
                    [rename debug-process-exceptions
                            process:exceptions
                            (() (debug-process?) . opt-> . frp:event?)]
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
  
  
  (define (kill process)
    (unless (debug-process-exited? process)
      (process:->dead process)))

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
  
  
  ;; returns a memoized function that takes (line column) -> position
  ;; line-col->pos : (debug-file? . -> . (number? number? . -> . (union void? number?)))
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
    (cond
     [(memf (lambda (c) (equal? (debug-client-modpath c) (path->string modpath)))
            (debug-process-clients process)) => first]
     [else false]))
 
  (define (break? process client)
    (let ([tracepoints (and client (debug-client-tracepoints client))])
      (lambda (pos)
        (or (debug-process-pause-requested? process)
            (and tracepoints
                 (hash-get tracepoints (sub1 pos) (lambda () false)))))))
  
  (define (receive-result process client top-mark rest-marks)
    (let* ([byte-offset (sub1 (syntax-position (mark-source top-mark)))]
           [traces (hash-get (debug-client-tracepoints client) byte-offset (lambda () empty))]
           [marks (cons top-mark (continuation-mark-set->list rest-marks debug-key))]
           [w (map (compose syntax-local-infer-name mark-source) marks)]
           [where-event ((frp:signal-thunk (debug-process-where process)) #t)])
      
      (set-debug-process-marks! process marks)

      (if (empty? traces)

          (frp:send-synchronous-event where-event w)

                                        ; Run all traces at this trace point               
          (let* ([to-send (map (lambda (t)
                                 (list (trace-struct-evnt-rcvr t)
                                       ((trace-struct-thunk t))))
                               traces)])
            (frp:send-synchronous-events (cons (list where-event w) to-send))))
      
                                        ; Now that we processed the trace, do we want to pause or continue
      (when (debug-process-pause-requested? process)
        (set-debug-process-pause-requested?! process false)
        (let loop ()
          (unless (debug-process-resume-requested? process)
            (semaphore-wait (debug-process-run-semaphore process))
            (loop)))
        (set-debug-process-resume-requested?! process false))

      (set-debug-process-marks! process false)))
  
  
  
  
  (define ((break-after process client) top-mark marks . vals)
    (receive-result process client top-mark marks) ; TODO: have access to return value
    (apply values vals)) ; TODO: allow modification of the return value
  
  (define ((break-before process client) top-mark marks)
    (receive-result process client top-mark marks) ; TODO: allow substitute value
    false)
  
  (define (launch-sandbox process)

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
        (memf (lambda (c) (equal? (debug-client-modpath c) (path->string filename)));; TODO: harmonize path & string
              (debug-process-clients process)))
     ;; annotator
     (lambda (stx)
       (let ([client (and (syntax-source stx)
                          (find-client process (syntax-source stx)))])
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
  
  (define (process:new->running process)
    (set-debug-process-run-semaphore! process (make-semaphore))
    
    (thread (lambda ()
              (launch-sandbox process)
              (process:running->finished process))))
  
  (define (process:running->finished process)
    (process:->dead process))

  (define (process:->dead process)
    (set! all-debug-processes (remq process all-debug-processes))
    (custodian-shutdown-all (debug-process-custodian process))
    (frp:set-cell! (debug-process-exited? process) true))

  (define set-running-e!
    (opt-lambda (e [process (current-process)])
      (resume process)
      (frp:set-cell! (debug-process-running-e process) e)))

  (define set-running!
    (opt-lambda (b [process (current-process)])
      (if (frp:value-now b) (resume process) (pause process))
      (frp:set-cell! (debug-process-running-e process) (frp:changes b))))

  (define (process:running->paused process)
    (set-debug-process-pause-requested?! process true))

  (define (process:paused->running process)
    (set-debug-process-resume-requested?! process true)
    (semaphore-post (debug-process-run-semaphore process)))

  (define (main-client-name process)
    (let-values ([(_ name __)
                  (split-path (debug-client-modpath (debug-process-main-client process)))])
      name))
  
  (define (pause process)
    (when (and (debug-process-run-semaphore process)
               (not (frp:value-now (debug-process-exited? process)))
               (not (debug-process-pause-requested? process)))
      (process:running->paused process)))

  (define (resume process)
    (cond
     [(not (debug-process-run-semaphore process)) (process:new->running process)]
     [(and (not (frp:value-now (debug-process-exited? process)))
           (not (debug-process-resume-requested? process)))
      (process:paused->running process)]))
           
  (define (create-debug-process)

    (letrec ([running-e (frp:new-cell frp:never-e)]
             [run-manager (running-e . frp:==> .
                           (lambda (r)
                             (if r (resume process) (pause process))))]
             [process (make-debug-process (make-custodian)
                                          false ; run-semaphore - false so we know it has never started
                                          running-e ; running-e
                                          run-manager ; run-manager
                                          false ; pause-requested?
                                          false ; resume-requested?
                                          (frp:new-cell false) ; exited?
                                          (frp:event-receiver) ; exceptions
                                          false ; main-client
                                          empty ; clients
                                          (frp:new-cell empty) ; where
                                          false)]) ; marks
      (set! all-debug-processes (cons process all-debug-processes))
      process))
  
  (define where
    (frp:new-cell empty))

  (define current-process 
    (let* ([proc (create-debug-process)]
           [p (make-parameter proc)])
      (frp:set-cell! where (debug-process-where proc))
      (case-lambda
        [() (p)]
        [(new-p) (frp:set-cell! where (debug-process-where new-p)) (p new-p)])))


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
  
  
  
  ; Creates a debugger client
  ; (debug-process? require-path. -> . debug-file?)
  (define (create-debug-client process modpath)
    ; throwaway namespace so the module-name-resolver doesn't load an unannotated module
    (parameterize ([current-namespace (make-namespace)])
      (let ([client (create-empty-debug-client)])
        (for-each (lambda (c)
                    (when (equal? modpath (debug-client-modpath c))
                      (raise-syntax-error 'mztake:script-error:create-debug-client
                                          (format "A client for `~a' is already defined for this process." modpath))))
                  (debug-process-clients process))
          
        (set-debug-client-modpath! client modpath)
        (set-debug-client-process! client process)
        (set-debug-client-line-col->pos! client (line-col->pos modpath))
        (set-debug-process-clients! process
                                    (append (list client) (debug-process-clients process)))
          
                                        ; set the main module if it has not been set
                                        ; this implies that the first client created is always the main module
        (unless (debug-process-main-client process)
          (set-debug-process-main-client! process client))
          
        client)))

  (define (set-main! mod-path)
    (let ([client (find-client (current-process) mod-path)])
      (if client
          (set-debug-process-main-client! (current-process) client)
          (create-debug-client (current-process) mod-path))))

  (define (hold-b b)
    (frp:hold (frp:filter-e (lambda (ev) (not (frp:undefined? ev))) (frp:changes b))))

  (define (expand-module-filename filename)
    (define (build-module-filename str) ; taken from module-overview.ss
      (let ([try (lambda (ext)
                   (let ([tst (string-append str ext)])
                     (and (file-exists? tst) tst)))])
        (or (try ".ss") (try ".scm") (try "") str)))
               
    (let ([modpath (symbol->string ((current-module-name-resolver) filename #f #f))])
      (build-module-filename
       (if (regexp-match #rx"^," modpath)
           (substring modpath 1 (string-length modpath))
           modpath))))

  (define (trace* loc thunk)
    (let* ([modpath (expand-module-filename (loc-modpath loc))]
           [clients (filter (lambda (c)
                              (equal? modpath (debug-client-modpath c)))
                            (debug-process-clients (current-process)))]
           [client (if (empty? clients)
                       (create-debug-client (current-process) modpath)
                       (first clients))]
           [trace-hash (debug-client-tracepoints client)]
           [trace (make-trace-struct (frp:event-receiver) thunk)]
           [pos ((debug-client-line-col->pos client) (loc-line loc) (loc-col loc))])
                                        ; add the trace to the list of traces for that byte-offset
      (hash-put! trace-hash pos
                 (append (hash-get trace-hash pos (lambda () '()))
                         (list trace)))
      (trace-struct-evnt-rcvr trace)))

  (define-syntax trace
    (syntax-rules ()
      [(_ loc)
       (trace* loc (lambda () true))]
      [(_ loc body ...)
       (trace* loc (lambda () body ...))]))

  (define-syntax bind
    (syntax-rules ()
      [(_ (name ...) body0 body ...)
       (let ([name (mark-binding-value
                    (first (lookup-all-bindings
                            (lambda (id) (eq? (syntax-e id) 'name))
                            (debug-process-marks (current-process)))))] ...)
         body0 body ...)]))

  (define-syntax define/bind
    (syntax-rules ()
      [(_ loc name ...) 
       (begin 
         (define here loc)
         (define name (frp:hold (trace here (bind (name) name))))
         ...)]))

  (define (syntax-local-infer-name stx)
    (or (syntax-property stx 'inferred-name)
        (let ([s (syntax-source stx)])
          (and s
               (let ([s (cond
                         [(path? s) (path->string s)]
                         [else s])]
                     [l (syntax-line stx)]
                     [c (syntax-column stx)])
                 (if l
                     (string->symbol (format "~a:~a:~a" s l c))
                     (let ([p (syntax-position stx)])
                       (string->symbol (format "~a::~a" s p)))))))))

  (provide loc$ loc loc-modpath loc-line loc-col
           trace trace* bind define/bind create-debug-process
           create-debug-client where set-main!
           mztake-version)
  
  ;###########################################################################################################
  
  )
