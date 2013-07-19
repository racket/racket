#lang racket/base

(require racket/file
         racket/future
         racket/place
         racket/port
         racket/fasl
         racket/match
         racket/path
         racket/class
         racket/stxparam
         (for-syntax syntax/parse
                     racket/base))

(provide parallel-do
         current-executable-path
         current-collects-path
         match-message-loop
         send/success
         send/error
         send/report
         send/msg
         recv/req
         worker/die
         work-queue<%>
         define/class/generics
         list-queue)

(define-syntax-rule (mk-generic func clss method args ...)
  (begin
    (define g (generic clss method))
    (define (func obj args ...)
      (send-generic obj g args ...))))

(define-syntax-rule (define/class/generics class (func method args ...) ...)
  (begin
    (mk-generic func class method args ...) ...))

(define-syntax-rule (define/class/generics/provide class (func method args ...) ...)
  (begin
    (begin
      (mk-generic func class method args ...)
      (provide func)) ...))

(define-syntax-rule (DEBUG_COMM a ...)
  (void)
;  (begin a ...)
)

(define worker<%> 
  (interface ()
    spawn
    send/msg
    kill
    break
    wait
    recv/msg
    read-all
    get-id
    get-out))

(define worker% 
  (class* object% (worker<%>)
    (field [id 0]
           [process-handle null]
           [out null]
           [in null]
           [err null]
           [module-path null]
           [funcname null])

    (define/public (spawn _id _module-path _funcname [initialmsg #f])
      (set! module-path _module-path)
      (set! funcname _funcname)
      (define worker-cmdline-list (list (current-executable-path) "-X" (path->string (current-collects-path)) "-e" "(eval(read))"))
      (define dynamic-require-cmd `((dynamic-require (string->path ,module-path) (quote ,funcname)) #f))
      (let-values ([(_process-handle _out _in _err) (apply subprocess #f #f (current-error-port) worker-cmdline-list)])
        (set! id _id)
        (set! process-handle _process-handle)
        (set! out _out)
        (set! in _in)
        (set! err _err)
        (send/msg dynamic-require-cmd)
        (when initialmsg (send/msg (initialmsg id)))))
    (define/public (send/msg msg) 
      (with-handlers ([exn:fail?
                       (lambda (x)
                         (eprintf "While sending message to parallel-do worker: ~a ~a\n"
                                  id (exn-message x))
                         (exit 1))])
        (DEBUG_COMM (eprintf "CSENDING ~v ~v\n" id msg))
        (write msg in) (flush-output in)))
    (define/public (recv/msg)
      (with-handlers ([exn:fail?
                       (lambda (x)
                         (eprintf "While receiving message from parallel-do worker ~a ~a\n"
                                  id (exn-message x))
                         (exit 1))])
        (define r (read out))
        (DEBUG_COMM (eprintf "CRECEIVNG ~v ~v\n" id r))
        r))
    (define/public (read-all) (port->string out))
    (define/public (get-id) id)
    (define/public (get-out) out)
    (define/public (kill)
      (DEBUG_COMM (eprintf "KILLING WORKER ~a\n" id))
      (close-output-port in)
      (close-input-port out)
      (subprocess-kill process-handle #t))
    (define/public (break) (kill))
    (define/public (kill/respawn worker-cmdline-list [initialmsg #f])
      (kill)
      (spawn id module-path funcname [initialmsg #f]))
    (define/public (wait) (subprocess-wait process-handle))
    (super-new)))

(define place-worker% 
  (class* object% (worker<%>)
    (init-field [id 0]              
                [pl null])
               
    (define/public (spawn _id module-path funcname [initialmsg #f])
      (set! id _id)
      (set! pl (dynamic-place (string->path module-path) funcname))
      (when initialmsg (send/msg (initialmsg id))))
    (define/public (send/msg msg)
      (DEBUG_COMM (eprintf "CSENDING ~v ~v\n" pl msg))
      (place-channel-put pl msg))
    (define/public (recv/msg)
      (define r (place-channel-get pl))
      (DEBUG_COMM (eprintf "CRECEIVNG ~v ~v\n" pl r))
      r)
    (define/public (read-all) "")
    (define/public (get-id) id) 
    (define/public (get-out) pl)
    (define/public (kill) #f)
    (define/public (break) (place-break pl))
    (define/public (wait) (place-wait pl))
    (super-new))) 

(define work-queue<%> 
  (interface ()
    get-job
    work-done
    has-jobs?
    jobs-cnt
    get-results))

(define/class/generics/provide worker<%>
  (wrkr/spawn spawn id worker-cmdline-list initialcode initialmsg)
  (wrkr/send  send/msg msg)
  (wrkr/kill  kill)
  (wrkr/break break)
  (wrkr/recv  recv/msg)
  (wrkr/read-all  read-all)
  (wrkr/id    get-id)
  (wrkr/out   get-out)
  (wrkr/wait  wait))


(define/class/generics/provide work-queue<%>
  (queue/get        get-job wrkrid)
  (queue/work-done  work-done node wrkr msg)
  (queue/has        has-jobs?)
  (queue/count      jobs-cnt)
  (queue/results    get-results))

(define (current-executable-path) 
 (parameterize ([current-directory (find-system-path 'orig-dir)])
  (find-executable-path (find-system-path 'exec-file) #f)))

(define (current-collects-path)
 (let ([p (find-system-path 'collects-dir)])
  (if (complete-path? p)
      p
      (path->complete-path p (or (path-only (current-executable-path))
                                 (find-system-path 'orig-dir))))))

(define (parallel-do-event-loop module-path funcname initialmsg work-queue nprocs [stopat #f])
  (define use-places? (place-enabled?)) ; set to #f to use processes instead of places
  
  (define (spawn id)
    ;; spawns a new worker
    (define wrkr (if use-places? (new place-worker%) (new worker%)))
    (wrkr/spawn wrkr id module-path funcname initialmsg)
    wrkr)

  (define workers null)
  (define (spawn! id)
    ;; spawn a worker and add it to the list;
    ;; disable breaks because we want to make sure
    ;; that a new worker is added to the list of workers
    ;; before a break exception is raised:
    (parameterize-break 
     #f
     (let ([w (spawn id)])
       (set! workers (cons w workers))
       w)))
  (define (unspawn! wkr)
    (wrkr/kill wkr)
    (set! workers (remq wkr workers)))

  (define (jobs?) (queue/has work-queue))
  (define (empty?) (not (queue/has work-queue)))

  ;; If any exception (including a break exception) happens before
  ;; the work loop ends, then send a break to interrupt each worker;
  ;; the `normal-finish?' flag is set to #t when the working loop ends
  ;; normally.
  (define normal-finish? #f)

  (define log-exn (lambda (exn [msg #f])
                    (log-error (let ([s (if (exn? exn)
                                            (let ([p (open-output-string)])
                                              (parameterize ([current-error-port p])
                                                ((error-display-handler) (exn-message exn) exn))
                                              (get-output-string p))
                                            (format "exception: ~v" exn))])
                                 (if msg
                                     (format "~a; ~a" msg s)
                                     s)))))

  (dynamic-wind
    (lambda () (void))
    (lambda ()
      (define (check-error-threshold x) 
        (when (x . >= . 4)
          (error 'parallel-do "error count reached ~a, exiting" x)))
      (for/list ([i (in-range nprocs)]) 
        (spawn! i))
      (let loop ([idle workers]
                 [inflight null]
                 [count 0]
                 [error-count 0])
        (check-error-threshold error-count)
        (cond 
          ;; Reached stopat count STOP
          [(and stopat (= count stopat)) ; ???
           (log-error "done at limit")]
          ;; Queue empty and all workers idle, we are all done
          [(and (empty?) (null? inflight)) 
           ;; done
           (void)]
          ;; Send work to idle worker
          [(and (jobs?) (pair? idle))
           (match-define (cons wrkr idle-rest) idle)
           (define-values (job cmd-list) (queue/get work-queue (wrkr/id wrkr)))
           (let retry-loop ([wrkr wrkr]
                            [error-count error-count]) 
             (check-error-threshold error-count)
             (with-handlers* ([exn:fail? (lambda (e) 
                                           (log-exn e (format "error writing to worker: ~v" 
                                                              (wrkr/id wrkr)))
                                           (unspawn! wrkr)
                                           (retry-loop (spawn! (wrkr/id wrkr)) (add1 error-count)))])
               (wrkr/send wrkr cmd-list))
             (loop idle-rest (cons (list job wrkr) inflight) count error-count))]
          [else
           (define (kill/remove-dead-worker node-worker wrkr)
             (DEBUG_COMM (printf "KILLING ~v\n" (wrkr/id wrkr)))
             (unspawn! wrkr)
             (loop (cons (spawn! (wrkr/id wrkr)) idle)
                   (remove node-worker inflight)
                   count
                   (add1 error-count))) 
           (define (gen-node-handler node-worker)
             (match node-worker
               [(list node wrkr)
                (handle-evt
                 (wrkr/out wrkr) 
                 (λ (e)
                    (let ([msg
                           (with-handlers* ([exn:fail? (lambda (e) 
                                                         (log-exn e (format "error reading from worker: ~v"
                                                                            (wrkr/id wrkr)))
                                                         (kill/remove-dead-worker node-worker wrkr))])
                             (if use-places? e (wrkr/recv wrkr)))])
                      (if (pair? msg)
                          (if (queue/work-done work-queue node wrkr msg)
                              (loop (cons wrkr idle) (remove node-worker inflight) (add1 count) error-count)
                              (loop idle inflight count error-count))
                          (begin
                            (queue/work-done work-queue node wrkr (string-append msg (wrkr/read-all wrkr)))
                            (kill/remove-dead-worker node-worker wrkr))))))]
               [else 
                (log-error (format "parallel-do-event-loop match node-worker failed trying to match: ~e" 
                                   node-worker))]))
           (DEBUG_COMM (printf "WAITING ON WORKERS TO RESPOND\n"))
           (apply sync (map gen-node-handler inflight))]))
      ;; Ask workers to stop:
      (for ([p workers]) 
        (wrkr/send p (list 'DIE)))
      ;; Finish normally:
      (set! normal-finish? #t))
    (lambda () 
      (define (break-all)
        (for ([p workers]) 
          (with-handlers ([exn? log-exn])
            (wrkr/break p))))
      (unless normal-finish?
        ;; There was an exception, so tell workers to stop:
        (break-all))
      ;; Wait for workers to complete; pass any break request on
      ;; to the worker places, in case they ignored an earlier
      ;; break for some reason:
      (let loop ()
        (with-handlers* ([exn:break? (lambda (exn)
                                       (break-all)
                                       (loop))])
          (parameterize-break
           #t
           (for ([p workers]) 
             (with-handlers ([exn:fail? log-exn])
               (wrkr/wait p)))))))))
  
(define list-queue% 
  (class* object% (work-queue<%>)
    (init-field queue create-job-thunk success-thunk failure-thunk [report-proc display])
    (field [results null])

    (define/public (work-done work workerid msg)
      (match msg
        [(list (list 'REPORT msg) stdout stderr)
         (report-proc msg)
         #f]
        [(list (list 'DONE result) stdout stderr)
         (set! results (cons (success-thunk work result stdout stderr) results))
         #t]
        [(list (list 'ERROR errmsg) stdout stderr)
         (failure-thunk work errmsg stdout stderr)
         #t]))
    (define/public (get-job workerid)
      (match queue
        [(cons h t)
          (set! queue t)
          (values h (create-job-thunk h workerid))]))
    (define/public (has-jobs?) (not (null? queue)))
    (define/public (get-results) (reverse results))
    (define/public (jobs-cnt) (length queue))
    (super-new)))

(define (list-queue list-of-work create-job-thunk job-success-thunk job-failure-thunk)
  (make-object list-queue% list-of-work create-job-thunk job-success-thunk job-failure-thunk))

(define-syntax-rule (define-parallel-keyword-error d x)
  (d x (lambda (stx) (raise-syntax-error 'x "only allowed inside parallel worker definition" stx))))
(define-syntax-rule (define-syntax-parameter-error x) (define-parallel-keyword-error define-syntax-parameter x))

(define-parallel-keyword-error define match-message-loop)
(define-syntax-parameter-error send/msg)
(define-syntax-parameter-error send/success)
(define-syntax-parameter-error send/error)
(define-syntax-parameter-error send/report)
(define-syntax-parameter-error recv/req)
(define-syntax-parameter-error worker/die)

(define-for-syntax (gen-worker-body globals-list globals-body work-body channel)
  (with-syntax ([globals-list globals-list]
                [(globals-body ...) globals-body]
                [([work work-body ...] ...) work-body]
                [ch channel])
    #'(do-worker
       ch
       (lambda (msg per-loop-body)
         ;; single starting message:
         (match msg
           [globals-list
            globals-body ...
            ;; bind per-worker-set procedures:
            (per-loop-body
             (lambda (send/msgp recv/reqp die-k)
               (syntax-parameterize ([send/msg (make-rename-transformer #'send/msgp)]
                                     [recv/req (make-rename-transformer #'recv/reqp)]
                                     [worker/die (make-rename-transformer #'die-k)])
                 ;; message handler:
                 (lambda (msg send/successp send/errorp send/reportp)
                   (syntax-parameterize ([send/success (make-rename-transformer #'send/successp)]
                                         [send/error (make-rename-transformer #'send/errorp)]
                                         [send/report (make-rename-transformer #'send/reportp)])
                     (match msg
                       [work work-body ...]
                       ...))))))])))))

(define (do-worker ch setup-proc)
  (define orig-err (current-error-port))
  (define orig-out (current-output-port))
  (define orig-in  (current-input-port))
  (define (raw-send msg)
    (cond 
     [ch (place-channel-put ch msg)]
     [else (write msg orig-out)
           (flush-output orig-out)]))
  (define (raw-recv)
    (cond 
     [ch (place-channel-get ch)]
     [else (read orig-in)]))
  (define (pdo-send msg)
    (with-handlers ([exn:fail?
                     (lambda (x)
                       (log-error (format "WORKER SEND MESSAGE ERROR: ~a" (exn-message x)))
                       (exit 1))])
      (DEBUG_COMM (fprintf orig-err "WSENDING ~v\n" msg))
      (raw-send msg)))
  (define (pdo-recv)
    (with-handlers ([exn:fail?
                     (lambda (x)
                       (log-error (format "WORKER RECEIVE MESSAGE ERROR: ~a" (exn-message x)))
                       (exit 1))])
      (define r (raw-recv))
      (DEBUG_COMM (fprintf orig-err "WRECVEIVED ~v\n" r))
      r))
  
  (setup-proc (pdo-recv)
              (lambda (set-proc)
                (let/ec die-k
                  (define (recv/reqp) (pdo-recv))
                  (define (send/msgp msg)
                    (pdo-send msg))
                  (let ([msg-proc (set-proc send/msgp recv/reqp die-k)])
                    (let loop ([i 0])
                      (DEBUG_COMM (fprintf orig-err "WAITING ON CONTROLLER TO RESPOND  ~v ~v\n" orig-in i))
                      (let ([out-str-port (open-output-string)]
                            [err-str-port (open-output-string)])
                        (define (send/resp type)
                          (pdo-send (list type (get-output-string out-str-port) (get-output-string err-str-port))))
                        (define (send/successp result)
                          (send/resp (list 'DONE result)))
                        (define (send/errorp message)
                          (send/resp (list 'ERROR message)))
                        (define (send/reportp message)
                          (send/resp (list 'REPORT message)))
                        ((with-handlers* ([exn:fail? (lambda (x) 
                                                       (define sp (open-output-string))
                                                       (parameterize ([current-error-port sp])
                                                         ((error-display-handler) (exn-message x) x))
                                                       (send/errorp (get-output-string sp))
                                                       (lambda () (loop (add1 i))))])
                           (parameterize ([current-output-port out-str-port]
                                          [current-error-port err-str-port])
                             (let ([msg (pdo-recv)])
                               (match msg
                                 [(list 'DIE) void]
                                 [_ (msg-proc msg send/successp send/errorp send/reportp)
                                    (lambda () (loop (add1 i)))]))))))))))))

(define-syntax (lambda-worker stx)
  (syntax-parse stx #:literals (match-message-loop)
    [(_ (globals-list:id ...)
      globals-body:expr ...
      (match-message-loop
        [work:expr work-body:expr ...] ...))

      (with-syntax ([body (gen-worker-body #'(list globals-list ...) #'(globals-body ...) #'([work work-body ...] ...) #'ch)])
        #'(lambda (ch) body))]))

(define-syntax (parallel-do stx)
  (syntax-case stx (define-worker)
    [(_ worker-count initalmsg work-queue (define-worker (name args ...) body ...))
     (begin
       (with-syntax ([interal-def-name (syntax-local-lift-expression #'(lambda-worker (args ...) body ...))])
         (syntax-local-lift-provide #'(rename interal-def-name name)))
       #'(let ([wq work-queue])
           (define module-path (path->string (resolved-module-path-name (variable-reference->resolved-module-path (#%variable-reference)))))
           (parallel-do-event-loop module-path 'name initalmsg wq worker-count)
           (queue/results wq)))]))
