#lang racket/base

(require racket/file
         racket/future
         racket/port
         racket/fasl
         racket/match
         racket/path
         racket/class
         racket/serialize
         racket/stxparam
         (for-syntax syntax/parse
                     racket/base))

(provide parallel-do
         parallel-do-event-loop
         current-executable-path
         current-collects-path
         match-message-loop
         send/success
         send/error
         WorkQueue<%>
         wrkr/send
         define/class/generics)

(define Worker% (class object%
  (field [id 0]
         [process-handle null]
         [out null]
         [in null]
         [err null])

  (define/public (spawn _id worker-cmdline-list initialcode initialmsg)
    (let-values ([(_process-handle _out _in _err) (apply subprocess #f #f (current-error-port) worker-cmdline-list)])
      (set! id _id)
      (set! process-handle _process-handle)
      (set! out _out)
      (set! in _in)
      (set! err _err)
      (when initialcode (send/msg initialcode))
      (when initialmsg (send/msg (s-exp->fasl (serialize (initialmsg id)))))))

  (define/public (send/msg msg) (write msg in) (flush-output in))
  (define/public (recv/msg) (read out))
  (define/public (get-id) id)
  (define/public (get-out) out)
  (define/public (kill)
    (eprintf "KILLING WORKER ~a\n" id)
    (close-output-port in)
    (close-input-port out)
    (subprocess-kill process-handle #t))
  (define/public (wait) (subprocess-wait process-handle))
  (super-new)))

(define WorkQueue<%> (interface ()
  get-job
  work-done
  has-jobs?
  jobs-cnt
  get-results))

(define-syntax-rule (mk-generic func clss method args ...)
  (begin
    (define g (generic clss method))
    (define (func obj args ...)
      (send-generic obj g args ...))))

(define-syntax-rule (define/class/generics class (func method args ...) ...)
  (begin
    (mk-generic func class method args ...) ...))

(define/class/generics Worker%
  (wrkr/send  send/msg msg)
  (wrkr/kill  kill)
  (wrkr/recv  recv/msg)
  (wrkr/id    get-id)
  (wrkr/out   get-out)
  (wrkr/spawn spawn id  worker-cmdline-list initialcode initialmsg))

(define/class/generics WorkQueue<%>
  (queue/get        get-job wrkrid)
  (queue/work-done  work-done node wrkr msg)
  (queue/has        has-jobs?)
  (queue/count      jobs-cnt))


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
  (define (spawn id)
    (define wrkr (new Worker%))
    (wrkr/spawn wrkr id worker-cmdline-list initialcode initialmsg)
    wrkr)
  (define (jobs?) (queue/has jobqueue))
  (define (empty?) (not (queue/has jobqueue)))
  (define workers #f)

  (dynamic-wind
    (lambda ()
      (parameterize-break #f
        (set! workers (for/list ([i (in-range nprocs)]) (spawn i)))))
    (lambda ()
      (define (error-threshold x) 
        (if (x . >= . 4)
          (begin 
            (eprintf "Error count reached ~a, exiting\n" x)
            (exit 1))
          #f))
      (let loop ([idle workers]
                 [inflight null]
                 [count 0]
                 [error-count 0])
        (cond 
          [(error-threshold error-count)]
          ;; Reached stopat count STOP
          [(= count stopat) (printf "DONE AT LIMIT\n")]
          ;; Queue empty and all workers idle, we are all done
          [(and (empty?) (null? inflight)) (set! workers idle)]
          ;; Send work to idle worker
          [(and (jobs?) (pair? idle))
           (match idle [(cons wrkr idle-rest)
            (let-values ([(job cmd-list) (queue/get jobqueue (wrkr/id wrkr))])
              (let retry-loop ([wrkr wrkr]
                               [error-count error-count]) 
                (error-threshold error-count)
                (with-handlers* ([exn:fail? (lambda (e) 
                        (printf "MASTER WRITE ERROR - writing to worker: ~a\n" (exn-message e))    
                        (wrkr/kill wrkr)
                        (retry-loop (spawn (wrkr/id wrkr)) (add1 error-count)))])
                   (wrkr/send wrkr cmd-list))
                (loop idle-rest (cons (list job wrkr) inflight) count error-count)))])]
          
          [else
            (define (kill/remove-dead-worker node-worker wrkr)
              (wrkr/kill wrkr)
              (loop (cons (spawn (wrkr/id wrkr)) idle)
                    (remove node-worker inflight)
                    count
                    (add1 error-count))) 
            (apply sync (for/list ([node-worker inflight])
              (match node-worker [(list node wrkr)
                (define out (wrkr/out wrkr))
                (handle-evt out (λ (e)
                  (with-handlers* ([exn:fail? (lambda (e) 
                                  (printf "MASTER READ ERROR - reading from worker: ~a\n" (exn-message e))
                                  (kill/remove-dead-worker node-worker wrkr))])
                    (let ([msg (wrkr/recv wrkr)])
                      (if (pair? msg)
                        (if (queue/work-done jobqueue node wrkr msg)
                          (loop (cons wrkr idle)
                                (remove node-worker inflight) 
                                (add1 count)
                                error-count)
                          (loop idle inflight count error-count))
                      (begin
                        (queue/work-done jobqueue node wrkr (string-append msg (port->string out))) 
                        (kill/remove-dead-worker node-worker wrkr)))))))]
                [else 
                  (eprintf "parallel-do-event-loop match node-worker failed.\n")
                  (eprintf "trying to match:\n~a\n" node-worker)])))])))
     (lambda () 
      (for ([p workers]) (with-handlers ([exn? void]) (wrkr/send p (list 'DIE))))
      (for ([p workers]) (send p wait)))))

(define ListQueue% (class* object% (WorkQueue<%>)
  (init-field queue create-job-thunk success-thunk failure-thunk)
  (field [results null])

  (define/public (work-done work workerid msg)
    (match msg
      [(list (list 'DONE result) stdout stderr)
        (set! results (cons (success-thunk work result stdout stderr) results))]
      [(list (list 'ERROR errmsg) stdout stderr)
        (failure-thunk work errmsg stdout stderr)]))
  (define/public (get-job workerid)
    (match queue
      [(cons h t)
        (set! queue t)
        (values h (create-job-thunk h))]))
  (define/public (has-jobs?) (not (null? queue)))
  (define/public (get-results) results)
  (define/public (jobs-cnt) (length queue))
  (super-new)))

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
                (fprintf orig-err "WORKER SEND MESSAGE ERROR ~a\n" (exn-message x))
                (exit 1))])
            (write msg orig-out)
            (flush-output orig-out)))
        (define (pdo-recv)
          (with-handlers ([exn:fail?
            (lambda (x)
                (fprintf orig-err "WORKER RECEIVE MESSAGE ERROR ~a\n" (exn-message x))
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
    [(_ worker-count initalmsg list-of-work create-job-thunk job-success-thunk job-failure-thunk workerthunk)
      (begin
        (define (gen-parallel-do-event-loop-syntax cmdline initial-stdin-data)
          (with-syntax ([cmdline cmdline]
                        [initial-stdin-data initial-stdin-data])
            #`(begin
                ;(printf "CMDLINE ~v\n" cmdline)
                ;(printf "INITIALTHUNK ~v\n" initial-stdin-data)
                (let ([jobqueue (make-object ListQueue% list-of-work create-job-thunk job-success-thunk job-failure-thunk)])
                  (parallel-do-event-loop initial-stdin-data initalmsg cmdline jobqueue worker-count 999999999)
                  (reverse (send jobqueue get-results))))))
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
              (gen-dynamic-require-current-module #'name))]))]))
