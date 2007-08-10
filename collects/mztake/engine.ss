(module engine mzscheme
  (require "marks.ss"
           (lib "etc.ss")
           (lib "list.ss")
           (lib "match.ss")
           (prefix frp: (lib "lang-ext.ss" "frtime"))           
           (rename (lib "frp-core.ss" "frtime")
                   frp:signal-thunk signal-thunk)
           "useful-code.ss"
           (lib "base-gm.ss" "frtime")
           "mztake-structs.ss"
           "load-sandbox.ss"
           "annotator.ss"
           (lib "match.ss"))
  
  (provide process:set-main!
           current-policy
           all-debug-processes
           create-debug-process
           process:->dead 
           process:new->running
           process:running->finished
           process:running->paused
           process:paused->running 
           pause
           resume
           trace*
           
           read-all-syntax
           pattern->pos)
  
  ;Keeps track of all debugging processes
  (define all-debug-processes null)  
  
  ; produces a nested list of (line column offset span) for all addressable syntax    
  (define (unwrap-syntax stx)
    (let ([elt (list (syntax-line stx)
                     (syntax-column stx)
                     (sub1 (syntax-position stx))
                     (syntax-span stx))])
      (syntax-case stx ()
        [(item ...) (cons elt (map unwrap-syntax (syntax->list stx)))]
        [x elt])))
  
  (define (read-all-syntax filename)
    (parameterize ([port-count-lines-enabled #t])
      (let ([port (open-input-file filename)])
        (begin0
          (let loop ([stx (read-syntax filename port)])
            (if (eof-object? stx) '()
                (cons stx 
                      (loop (read-syntax filename port)))))
          (close-input-port port)))))
  
  ;; returns a memoized function that takes (line column) -> (list/c position span)
  ;; line-col->pos : (debug-file? . -> . (number? number? . -> . (union void? number?)))
  (define (line-col->pos stx)
    (let ([pos-list (flatten (map unwrap-syntax stx))])
      (lambda (line maybe-col)
        (let loop ([lst pos-list])
          (cond
            [(empty? lst)
             (error 'loc
                    "No syntax found for trace at line/column ~a:~a in `~a'"
                    line maybe-col (syntax-source stx))]
            [(and (<= line (first (first lst)))
                  (or (not maybe-col)
                      (<= maybe-col (second (first lst)))))
             (list (third (first lst)) (fourth (first lst)))]
            
            [else (loop (rest lst))])))))
  
  (define (pattern->pos stx-lst)
    (define (quote-symbols lst)
      (match lst
        [(? symbol? s) `(quote ,s)]
        [() empty]
        [('_ tl ...) `(_ ... ,@(quote-symbols tl))]
        [(hd tl ...) (cons (quote-symbols hd)
                           (quote-symbols tl))]
        [item item]))
    
    (define (collect-locations h stx lst)
      (let loop ([stx stx] [lst lst])
        (cond 
          [(empty? stx) (void)]
          [(pair? stx)
           (loop (first stx) (first lst))
           (loop (rest stx) (rest lst))]
          [else 
           (when (syntax-line stx)
             (hash-put! h lst (list (sub1 (syntax-position stx)) (syntax-span stx))))
           (when (pair? (syntax-e stx)) 
             (loop (first (syntax-e stx)) (first lst))
             (loop (rest (syntax-e stx)) (rest lst)))])))
    
    (define stx2line-col (make-hash))
    (define lst (map syntax-object->datum stx-lst))
    
    (for-each (lambda (s l) (collect-locations stx2line-col s l))
              stx-lst lst)
    
    (lambda (pattern) 
      (let ([pred (eval `(begin (require (lib "match.ss"))
                                (lambda (v) 
                                  (match v 
                                    [,(quote-symbols pattern) #t]
                                    [_ #f]))))])
        (let loop ([lst lst])
          (let ([sub (if (pair? lst)
                         (append (loop (first lst))
                                 (loop (rest lst)))
                         empty)])
            (if (and (hash-mem? stx2line-col lst) (pred lst))
                (cons (hash-get stx2line-col lst) sub)
                sub))))))
  
  
  (define (find-client process modpath)
    (cond
      [(memf (lambda (c) (equal? (debug-client-modpath c) modpath))
             (debug-process-clients process)) => first]
      [else false]))
  
  (define (find-client/create process modpath)
    (or (find-client process modpath)
        (create-debug-client process modpath)))
  
  (define (process:set-main! p reqspec)
    (let* ([modpath (reqspec->modpath reqspec)]
           [client (find-client/create p modpath)])
      (set-debug-process-main-client! p client)))
  
  
  (define (break? process client)
    (let ([tracepoints (and client (debug-client-tracepoints client))])
      (lambda (pos)
        (or (debug-process-pause-requested? process)
            (and tracepoints
                 (hash-get tracepoints (sub1 pos) (lambda () false)))))))
  
  (define (traces->events traces vals)
    (map (lambda (t)
           (list (trace-struct-evnt-rcvr t)
                 (apply (trace-struct-thunk t) vals)))
         traces))
  
  (define (receive-result process client byte-offset top-mark rest-marks vals)
    (let* ([traces (hash-get (debug-client-tracepoints client) byte-offset (lambda () empty))]
           [no-traces? (empty? traces)]
           [has-single-trace? (and (not no-traces?) (empty? (rest traces)))]
           [no-where? (not (debug-process-where process))]
           [no-events? (and no-traces? no-where?
                            (not (debug-process-pause-requested? process)))])
      
      
      (unless no-events?
        (let* ([marks (cons top-mark (continuation-mark-set->list rest-marks debug-key))])
          (set-debug-process-marks! process marks)
          
          (if no-where?
              ;; No where event to generate
              (cond [has-single-trace?
                     ;; fast-path
                     (let* ([t (first traces)]
                            [e (apply (trace-struct-thunk t) vals)])
                       (frp:send-synchronous-event (trace-struct-evnt-rcvr t) e))]
                    [no-traces? (void)]
                    [else (frp:send-synchronous-events (traces->events traces vals))])
              
              ;; With a where event to generate
              (let ([where-event (debug-process-where process)]
                    [w (map (compose syntax-local-infer-name mark-source) marks)])
                (if no-traces?
                    (frp:send-synchronous-event where-event w)
                    
                    (let* ([where-event (list where-event w)]
                           [trace-events (traces->events traces vals)])
                      (frp:send-synchronous-events (cons where-event trace-events))))))
          
          ;; Now that we processed the trace, do we want to pause or continue
          (when (debug-process-pause-requested? process)
            (let loop ()
              (unless (debug-process-resume-requested? process)
                (semaphore-wait (debug-process-run-semaphore process))
                (loop)))
            (set-debug-process-pause-requested?! process false)
            (set-debug-process-resume-requested?! process false))
          
          (set-debug-process-marks! process false)))))
  
  
  
  
  (define ((break-after process client) top-mark marks . vals)
    (let* ([stx (mark-source top-mark)]
           [byte-offset (+ (syntax-position stx) (syntax-span stx) -2)])
      (receive-result process client byte-offset top-mark marks vals)
      (apply values vals))) ; TODO: allow modification of the return value
  
  (define ((break-before process client) top-mark marks)
    (let ([byte-offset (sub1 (syntax-position (mark-source top-mark)))])
      (receive-result process client byte-offset top-mark marks empty) ; TODO: allow substitute value
      false))
  
  (define (unbuild-path path)
    (let-values ([(base name _) (split-path path)])
      (if base
          (append (unbuild-path base) (list name))
          empty)))
  
  (define (head lst n)
    (if (= n 0)
        empty
        (cons (first lst) (head (rest lst) (sub1 n)))))
  
  (define (dir-contains? dir filename)
    (let ([dir-lst (unbuild-path dir)]
          [file-lst (unbuild-path filename)])
      (and (< (length dir-lst) (length file-lst))
           (equal? dir-lst (head file-lst (length dir-lst))))))
  
  (define (map-policy-tag tag)
    (cond [(eq? tag 'fast) false]
          [(eq? tag 'debuggable) true]
          [else (error 'map-policy-tag "unknown policy tag ~a" tag)]))
  
  (define (policy-requests-annotatation? policy filename)
    (if (empty? policy)
        true
        (let ([tag (first (first policy))]
              [collect-paths (second (first policy))])
          (map-policy-tag tag) ;; complains if the tag doesn't exists
          (if (or (eq? collect-paths 'everything-else)
                  (ormap (lambda (dir) (dir-contains? dir filename))
                         (if (list? collect-paths)
                             collect-paths
                             (list collect-paths))))
              (map-policy-tag tag)
              (policy-requests-annotatation? (rest policy) filename)))))
  
  (define (process-has-file? process filename)
    (and
     (memf (lambda (c) (equal? (debug-client-modpath c)
                               (path->string filename)));; TODO: harmonize path & string
           (debug-process-clients process))
     true))
  
  (define ((record-top-level-id process) module-name var-name val)
    (let* ([modules (debug-process-top-level process)]
           [bindings (hash-get modules module-name (lambda () (make-hash)))])
      (unless (hash-mem? modules module-name)
        (hash-put! modules module-name bindings))
      (hash-put! bindings var-name val)))
  
  (define (launch-sandbox process)
    (unless (debug-process-main-client process)
      (error 'launch-sandbox
             "No main file specified. Use TRACE or SET-MAIN! to indicate where to start execution"))
    
    (parameterize ([current-inspector (make-inspector)])
      (require/sandbox+annotations
       (debug-process-custodian process)
       ;; error-display-handler :
       (let ([orig-err-disp (error-display-handler)])
         (lambda (msg exn)
           (frp:send-event (debug-process-exceptions process) exn)
           (orig-err-disp msg exn)))
       
       ;; target file
       `(file ,(debug-client-modpath (debug-process-main-client process)))
       
       ;; annotate-module?
       (lambda (filename module-name)
         #;(print-each "annotate-module?"
                       filename
                       (process-has-file? process filename)
                       (policy-requests-annotatation? (debug-process-policy process) filename))
         
         (or (process-has-file? process filename)
             (policy-requests-annotatation? (debug-process-policy process) filename)))
       
       ;; annotator
       (lambda (stx)
         (if (not (syntax-source stx))
             stx
             (let*-values ([(client)
                            (find-client/create
                             process
                             (path->string (syntax-source stx)))]
                           [(annotated-stx pos-list)
                            (annotate-for-single-stepping 
                             stx 
                             (lambda (_) (break? process client))
                             (break-before process client)
                             (break-after process client)
                             (lambda (kind bound binding) (void))
                             (record-top-level-id process))])
               annotated-stx)))))) 
  
  (define (process:new->running process)
    (printf "mztake: starting ~a~n" (debug-client-modpath (debug-process-main-client process)))
    (set-debug-process-run-semaphore! process (make-semaphore))
    (set-debug-process-policy! process (current-policy))
    
    (thread (lambda ()
              (launch-sandbox process)
              (process:running->finished process))))
  
  (define (process:running->finished process)
    (process:->dead process))
  
  (define (process:->dead process)
    (printf "mztake: finished ~a~n" (debug-client-modpath (debug-process-main-client process)))
    (set! all-debug-processes (remq process all-debug-processes))
    (custodian-shutdown-all (debug-process-custodian process))
    (frp:set-cell! (debug-process-exited? process) true))
  
  (define (process:running->paused process)
    (set-debug-process-pause-requested?! process true))
  
  (define (process:paused->running process)
    (set-debug-process-resume-requested?! process true)
    (semaphore-post (debug-process-run-semaphore process)))
  
  (define (pause process)
    (when (and (debug-process-run-semaphore process)
               (not (frp:value-now (debug-process-exited? process)))
               (not (debug-process-pause-requested? process)))
      (process:running->paused process)))
  
  (define (resume process)
    (cond
      [(not (debug-process-run-semaphore process)) (process:new->running process)]
      [(and (not (frp:value-now (debug-process-exited? process)))
            (debug-process-pause-requested? process)
            (not (debug-process-resume-requested? process)))
       (process:paused->running process)]))
  
  (define current-policy (make-parameter `((fast ,(current-library-collection-paths))
                                           (debuggable everything-else))))
  
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
                                          false ; policy
                                          
                                          (frp:new-cell false) ; exited?
                                          (frp:event-receiver) ; exceptions
                                          false ; main-client
                                          empty ; clients
                                          (make-hash 'equal ) ; top-level
                                          
                                          false ; where
                                          false)]) ; marks
      (set! all-debug-processes (cons process all-debug-processes))
      process))
  
  ; Creates a debugger client
  ; (debug-process? require-path. -> . debug-file?)
  (define (create-debug-client process modpath)
    ; throwaway namespace so the module-name-resolver doesn't load an unannotated module
    (parameterize ([current-namespace (make-namespace)])
      (let ([client (create-empty-debug-client)]
            [stx (read-all-syntax modpath)])
        (for-each (lambda (c)
                    (when (equal? modpath (debug-client-modpath c))
                      (raise-syntax-error 'mztake:script-error:create-debug-client
                                          (format "A client for `~a' is already defined for this process." modpath))))
                  (debug-process-clients process))
        
        (set-debug-client-modpath! client modpath)
        (set-debug-client-process! client process)
        (set-debug-client-line-col->pos! client (line-col->pos stx))
        (set-debug-client-pattern->pos! client (pattern->pos stx))
        (set-debug-process-clients! process
                                    (append (list client) (debug-process-clients process)))
        
        ; set the main module if it has not been set
        ; this implies that the first client created is always the main module
        (unless (debug-process-main-client process)
          (set-debug-process-main-client! process client))
        
        client)))
  
  (define (reqspec->modpath filename)
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
  
  (define (loc->positions client loc)
    (let ([pos&spans
           (if (loc/lc? loc)
               (list ((debug-client-line-col->pos client) (loc/lc-line loc) (loc/lc-col loc)))
               ((debug-client-pattern->pos client) (loc/p-pattern loc)))])
      (if (loc-after? loc)
          (map (lambda (p&s) (+ (first p&s) (second p&s) -1)) pos&spans)
          (map first pos&spans))))
  
  (define (trace* p loc thunk)
    (let* ([modpath (reqspec->modpath (loc-reqspec loc))]
           [client (find-client/create p modpath)]
           [trace-hash (debug-client-tracepoints client)]
           [trace (make-trace-struct (frp:event-receiver) thunk)]
           [positions (loc->positions client loc)])      
      ; add the trace to the list of traces for these byte-offsets
      (for-each (lambda (pos) 
                  (hash-put! trace-hash pos
                             (append (hash-get trace-hash pos (lambda () '()))
                                     (list trace))))
                positions)
      (trace-struct-evnt-rcvr trace)))
  
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
  
  
  )