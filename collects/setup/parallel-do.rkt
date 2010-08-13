#lang racket/base

(require racket/file
         racket/future
         scheme/fasl
         scheme/match
         racket/path
         scheme/serialize
         unstable/generics
         racket/stxparam
         (for-syntax syntax/parse
                     racket/base))

(provide parallel-do
         parallel-do-event-loop
         parallel-do-default-error-handler
         current-executable-path
         current-collects-path
         match-message-loop
         send/success
         send/error
         jobqueue
         prop:jobqueue)

(define-generics (jobqueue prop:jobqueue jobqueue?)
  (work-done jobqueue work workerid msg)
  (get-job jobqueue workerid)
  (has-jobs? jobqueue)
  (jobs-cnt jobqueue))

(define-struct worker (id process-handle out in err))
(define (current-executable-path) 
  (parameterize ([current-directory (find-system-path 'orig-dir)])
    (find-executable-path (find-system-path 'exec-file) #f)))
(define (current-collects-path)
   (let ([p (find-system-path 'collects-dir)])
                         (if (complete-path? p)
                             p
                             (path->complete-path p (or (path-only (current-executable-path))
                                                        (find-system-path 'orig-dir))))))


(define (parallel-do-event-loop initialcode initialmsg worker-cmdline-list jobqueue nprocs stopat)
  (define (send/msg x ch)
    (write x ch)
    (flush-output ch))
  (define (spawn id)
    (let-values ([(process-handle out in err) (apply subprocess #f #f (current-error-port) worker-cmdline-list)])
      (when initialcode
        (send/msg initialcode in))
      (when initialmsg 
        (send/msg (s-exp->fasl (serialize (initialmsg id))) in))
      (make-worker id process-handle out in err)))
  (define (kill-worker wrkr)
    (match wrkr
      [(worker id process-handle out in err)
       (eprintf "KILLING WORKER ~a ~a~n" id wrkr)
       (close-output-port in)
       (close-input-port out)
       (subprocess-kill process-handle #t)]))
  (define (jobs? x) (has-jobs? jobqueue))
  (define (empty? x) (not (has-jobs? jobqueue )))
  (define workers #f)

  (dynamic-wind
    (lambda ()
      (parameterize-break #f
        (set! workers (for/list ([i (in-range nprocs)]) (spawn i)))))
    (lambda ()
      (define (error-threshold x) 
        (if (x . >= . 4)
          (begin 
            (eprintf "Error count reached ~a, exiting~n" x)
            (exit 1))
          #f))
      (letrec ([loop (match-lambda*
                     ;; QUEUE IDLE INFLIGHT COUNT
                     ;; Reached stopat count STOP
                     [(list idle inflight count (? error-threshold error-count)) (void)]
                     [(list idle inflight (? (lambda (x) (= x stopat))) error-count)  (printf "DONE AT LIMIT~n")]
                     ;; Send work to idle worker
                     [(list (and (? jobs?) (cons wrkr idle)) inflight count error-count)
                        (let-values ([(job cmd-list) (get-job jobqueue (worker-id wrkr))])
                          (let retry-loop ([wrkr wrkr]
                                           [error-count error-count]) 
                            (error-threshold error-count)
                            (match wrkr
                              [(worker i s o in e)
                                (with-handlers* ([exn:fail? (lambda (e) 
                                                     (printf "MASTER WRITE ERROR - writing to worker: ~a~n" (exn-message e))    
                                                     (kill-worker wrkr)
                                                     (retry-loop (spawn i) (add1 error-count)))])
                                  (send/msg cmd-list in))])
                            (loop idle (cons (list job wrkr) inflight) count error-count)))]
                     ;; Queue empty and all workers idle, we are all done
                     [(list (and (? empty?) idle) (list) count error-count) 
                      (set! workers idle)]
                     ;; Wait for reply from worker
                     [(list idle inflight count error-count)
                       (apply sync (map (λ (node-worker) (match node-worker
                                                 [(list node (and wrkr (worker id sh out in err)))
                                                     (handle-evt out (λ (e)
                                                                   (let ([msg 
                                                                          (with-handlers* ([exn:fail? (lambda (e) 
                                                                                                        (printf "MASTER READ ERROR - reading from worker: ~a~n" (exn-message e))    
                                                                                                        (kill-worker wrkr)
                                                                                                        (loop (cons (spawn id) idle)
                                                                                                              (remove node-worker inflight)
                                                                                                              count
                                                                                                              (add1 error-count)))])
                                                                            (read out))])
                                                                     (work-done jobqueue node id msg)
                                                                     (loop
                                                                           (cons wrkr idle)
                                                                           (remove node-worker inflight) 
                                                                           (add1 count)
                                                                           error-count))))]))
                                        
                                        inflight))])])
        (loop workers null 0 0)))
     (lambda () 
      (for ([p workers])
        (with-handlers ([exn? void])
              (send/msg (list 'DIE) (worker-in p))))
      (for ([p workers]) (subprocess-wait (worker-process-handle p))))))

(define (parallel-do-default-error-handler work error-message outstr errstr)
  (printf "WORKER ERROR ~a~n" error-message)
  (printf "STDOUT~n~a=====~n" outstr)
  (printf "STDERR~N~a=====~n" errstr))
  
(define-struct list-queue (queue results create-job-thunk success-thunk failure-thunk) #:transparent
  #:mutable
  #:property prop:jobqueue
  (define-methods jobqueue
    (define (work-done jobqueue work workerid msg)
      (match msg
        [(list (list 'DONE result) stdout stderr)
          (let ([result ((list-queue-success-thunk jobqueue) work result stdout stderr)])
            (set-list-queue-results! jobqueue (cons result (list-queue-results jobqueue))))]
        [(list (list 'ERROR errmsg) stdout stderr)
          ((list-queue-failure-thunk jobqueue) work errmsg stdout stderr)]))
    (define (get-job jobqueue workerid)
      (match (list-queue-queue jobqueue)
       [(cons h t)
         (set-list-queue-queue! jobqueue t)
         (values h ((list-queue-create-job-thunk jobqueue) h))]))
    (define (has-jobs? jobqueue)
      (not (null? (list-queue-queue jobqueue))))
    (define (jobs-cnt jobqueue)
      (length (list-queue-queue jobqueue)))))

(define match-message-loop
  (lambda (stx)
    (raise-syntax-error 'match-message-loop "only allowed inside a parallel worker definition" stx)))
(define-syntax-parameter send/success
  (lambda (stx)
    (raise-syntax-error 'send/success "only allowed inside parallel worker definition" stx)))
(define-syntax-parameter send/error
  (lambda (stx)
    (raise-syntax-error 'send/error "only allowed inside parallel worker definition" stx)))


(define-for-syntax (gen-worker-body globals-list globals-body work-body)
  (with-syntax ([globals-list globals-list]
                [(globals-body ...) globals-body]
                [(work work-body ...) work-body])
    #'(begin
        (define orig-err (current-error-port))
        (define orig-out (current-output-port))
        (define (pdo-send msg)
          (with-handlers ([exn:fail?
            (lambda (x)
                (fprintf orig-err "WORKER SEND MESSAGE ERROR ~a~n" (exn-message x))
                (exit 1))])
            (write msg orig-out)
            (flush-output orig-out)))
        (define (pdo-recv)
          (with-handlers ([exn:fail?
            (lambda (x)
                (fprintf orig-err "WORKER RECEIVE MESSAGE ERROR ~a~n" (exn-message x))
                (exit 1))])
          (read)))
        (match (deserialize (fasl->s-exp (pdo-recv)))
          [globals-list
            globals-body ...
            (let loop ()
              (match (pdo-recv)
                 [(list 'DIE) void]
                 [work
                  (let ([out-str-port (open-output-string)]
                        [err-str-port (open-output-string)])
                    (define (send/resp type)
                        (pdo-send (list type (get-output-string out-str-port) (get-output-string err-str-port))))
                    (define (send/successp result)
                        (send/resp (list 'DONE result)))
                    (define (send/errorp message)
                        (send/resp (list 'ERROR message)))
                    (with-handlers ([exn:fail? (lambda (x) (send/errorp (exn-message x)) (loop))])
                      (parameterize ([current-output-port out-str-port]
                                     [current-error-port err-str-port])
                      (syntax-parameterize ([send/success (make-rename-transformer #'send/successp)]
                                            [send/error (make-rename-transformer #'send/errorp)])
                          work-body ...
                          (loop)))))]))]))))

(define-syntax (lambda-worker stx)
  (syntax-parse stx #:literals(match-message-loop)
    [(_ (globals-list:id ...)
      globals-body:expr ...
      (match-message-loop
        [work:id work-body:expr ...]))

      (with-syntax ([body (gen-worker-body #'(list globals-list ...) #'(globals-body ...) #'(work work-body ...))])
        #'(lambda ()
            body))]))
        
(define-syntax (parallel-do stx)
  (syntax-case stx ()
    [(_ initalmsg list-of-work create-job-thunk job-success-thunk job-failure-thunk workerthunk)
      (begin
        (define (gen-parallel-do-event-loop-syntax cmdline initial-stdin-data)
          (with-syntax ([cmdline cmdline]
                        [initial-stdin-data initial-stdin-data])
            #`(begin
                ;(printf "CMDLINE ~v~n" cmdline)
                ;(printf "INITIALTHUNK ~v~n" initial-stdin-data)
                (let ([jobqueue (make-list-queue list-of-work null create-job-thunk job-success-thunk job-failure-thunk)])
                  (parallel-do-event-loop initial-stdin-data initalmsg cmdline jobqueue (processor-count) 999999999)
                  (reverse (list-queue-results jobqueue))))))
        (define (gen-dynamic-require-current-module funcname)
          (with-syntax ([funcname funcname])
            #'(let ([module-path (path->string (resolved-module-path-name (variable-reference->resolved-module-path (#%variable-reference))))])
                `((dynamic-require (string->path ,module-path) (quote funcname))))))
        (syntax-case #'workerthunk (define-worker)
          [(define-worker (name args ...) body ...)
            (with-syntax ([interal-def-name (syntax-local-lift-expression #'(lambda-worker (args ...) body ...))])
              (syntax-local-lift-provide #'(rename interal-def-name name)))
            (gen-parallel-do-event-loop-syntax 
              #'(list (current-executable-path) "-X" (path->string (current-collects-path)) "-e" "(eval(read))")
              (gen-dynamic-require-current-module #'name))]
          [funcname
            (gen-parallel-do-event-loop-syntax 
              #'(list (current-executable-path) "-X" (path->string (current-collects-path)) "-e" "(eval(read))")
              (gen-dynamic-require-current-module #'funcname))]))]))
