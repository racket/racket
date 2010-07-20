#lang racket/base

(require racket/future
         racket/list
         racket/match
         racket/path
         setup/collects
         unstable/generics)

(provide parallel-compile)

(define-generics (jobqueue prop:jobqueue jobqueue?)
  (work-done jobqueue queue work workerid msg)
  (get-job jobqueue queue workerid)
  (has-jobs? jobqueue queue)
  (jobs-cnt jobqueue queue)
  (job-desc jobqueue wokr)
  (initial-queue jobqueue))

(define (process-comp jobqueue nprocs stopat)
  (define process-worker-library "setup/parallel-build-worker")

  (define executable (parameterize ([current-directory (find-system-path 'orig-dir)])
                       (find-executable-path (find-system-path 'exec-file) #f)))
  (define collects-dir (let ([p (find-system-path 'collects-dir)])
                         (if (complete-path? p)
                             p
                             (path->complete-path p (or (path-only executable)
                                                        (find-system-path 'orig-dir))))))
                        
  (define (send/msg x ch)
    (write x ch)
    (flush-output ch))
  (define (spawn i)
    (let-values ([(s o in e) (subprocess #f #f (current-error-port) 
                                         executable
                                         "-X"
                                         (path->string collects-dir)
                                         "-l"
                                         process-worker-library)])
      (send/msg i in)
      (list i s o in e)))
  (define (kill-worker i nw o in)
     (eprintf "KILLING WORKER ~a ~a ~n" i nw)
     (close-input-port o)
     (close-output-port in))
  (define workers #f)
  (define (jobs? queue)
    (has-jobs? jobqueue queue))
  (define (empty? queue)
    (not (has-jobs? jobqueue queue)))

  (parameterize-break #f
    (set! workers (for/list ([i (in-range nprocs)]) (spawn i))))

  (dynamic-wind
    (lambda () (void)) 
    (lambda () 
      (letrec ([loop (match-lambda*
                     ;; QUEUE IDLE INFLIGHT COUNT
                     ;; Reached stopat count STOP
                     [(list queue idle inflight (? (lambda (x) (= x stopat))))  (printf "DONE AT LIMIT~n")]
                     ;; Send work to idle worker
                     [(list (? jobs? queue) (cons worker idle) inflight count)
                        (let-values ([(queue-state job cmd-list) (get-job jobqueue queue (first worker))])
                          (let retry-loop ([worker worker]) 
                            (match worker
                              [(list i s o in e)
                                (with-handlers* ([exn:fail? (lambda (nw) 
                                                     (kill-worker i nw i o)
                                                     (retry-loop (spawn i)))])
                                  (send/msg cmd-list in))])
                            (loop queue-state idle (cons (list job worker) inflight) count)))]
                     ;; Queue empty and all workers idle, we are all done
                     [(list (? empty?) idle (list) count) (void)]
                     ;; Wait for reply from worker
                     [(list queue idle inflight count)
                       (apply sync (map (λ (node-worker) (match node-worker
                                                 [(list node worker)
                                                  (match worker
                                                    [(list i s o in e)
                                                     (handle-evt o (λ (e)
                                                                   (let ([msg 
                                                                          (with-handlers* ([exn:fail? (lambda (nw) 
                                                                                                        (printf "READ ERROR - reading worker: ~a ~n" nw)    
                                                                                                        (kill-worker i nw i o)
                                                                                                        (loop queue
                                                                                                              (cons (spawn i) idle)
                                                                                                              (remove node-worker inflight)
                                                                                                              count))])
                                                                            (read o))])
                                                                     ;(list count i (length idle) (length inflight) (length queue))
                                                                     (loop (work-done jobqueue queue node i msg)
                                                                           (cons worker idle)
                                                                           (remove node-worker inflight) 
                                                                           (+ count 1)))))])]))
                                        
                                        inflight))])])
      (loop (initial-queue jobqueue) workers null 0)))
  (lambda () 
    (for ([p workers]) 
      (with-handlers ([exn? void])
        (send/msg (list 'DIE) (fourth p))))
    (for ([p workers]) (subprocess-wait (second p))))))


(define-struct collects-queue (cclst hash collects-dir printer) #:transparent
  #:mutable
  #:property prop:jobqueue
  (define-methods jobqueue
    (define (initial-queue jobqueue) null)
    (define (work-done jobqueue queue work workerid msg)
      (match (list work msg)
        [(list (list cc file) (list result-type out err))
          (let ([cc-name (cc-name cc)])
          (match result-type
            [(list 'ERROR msg)
              ((collects-queue-printer jobqueue) (current-error-port) "ERROR" "~a ~a: ~a" cc-name file msg)]
            ['DONE (void)])
          (when (or (not (zero? (string-length out))) (not (zero? (string-length err))))
            ((collects-queue-printer jobqueue) (current-error-port) "build-output" "~a ~a" cc-name file)
            (eprintf "STDOUT:~n~a=====~n" out)
            (eprintf "STDERR:~n~a=====~n" err)))]))
    ;; assigns a collection to each worker to be compiled
    ;; when it runs out of collections, steals work from other workers collections
    (define (get-job jobqueue queue workerid)
      (define (hash/first-pair hash)
         (match (hash-iterate-first hash)
           [#f #f]
           [x (cons (hash-iterate-key hash x) (hash-iterate-value hash x))]))
      (define (hash-ref!/true hash key thunk)
        (hash-ref hash key (lambda ()
                              (match (thunk)
                                [#f #f]
                                [x (hash-set! hash key x) x]))))
      (define (take-cc)
        (match (collects-queue-cclst jobqueue)
          [(list) #f]
          [(cons h t)
            (set-collects-queue-cclst! jobqueue t)
            (list h)]))
      (let ([w-hash (collects-queue-hash jobqueue)])
        (define (build-job cc file)
          (define (->bytes x)
            (cond [(path? x) (path->bytes x)]
                  [(string? x) (string->bytes/locale x)]))
          (let* ([cc-name (cc-name cc)]
                 [cc-path (cc-path cc)]
                 [full-path (path->string (build-path cc-path file))])
            ;(printf "JOB ~a ~a ~a ~a~n" workerid cc-name cc-path file)
            (values null (list cc file) (list cc-name (->bytes cc-path) (->bytes file)))))
        (let retry ()
          (define (find-job-in-cc cc id)
            (match cc
              [(list)
                (hash-remove! w-hash id) (retry)]
              [(list (list cc (list) (list)))       ;empty collect
                (hash-remove! w-hash id) (retry)]
              [(cons (list cc (list) (list)) tail)  ;empty parent collect
                (hash-set! w-hash id tail) (retry)]
              [(cons (list cc (list) subs) tail)    ;empty srcs list
                (hash-set! w-hash id (append subs tail)) (retry)]
              [(cons (list cc (list file) subs) tail)
                (hash-set! w-hash id (append subs tail))
                  ((collects-queue-printer jobqueue) (current-output-port) "made" "~a" (cc-name cc))
                  (build-job cc file)]
              [(cons (list cc (cons file ft) subs) tail)
                (hash-set! w-hash id (cons (list cc ft subs) tail))
                (build-job cc file)]))
          (match (hash-ref!/true w-hash workerid take-cc)
            [#f 
                (match (hash/first-pair w-hash)
                  [(cons id cc) (find-job-in-cc cc id)])]
            [cc (find-job-in-cc cc workerid)]))))
    (define (has-jobs? jobqueue queue)
      (define (hasjob?  cct)
        (let loop ([cct cct])
          (ormap (lambda (x) (or ((length (second x)) . > . 0) (loop (third x)))) cct)))

      (let ([jc  (jobs-cnt jobqueue queue)]
            [hj  (or (hasjob? (collects-queue-cclst jobqueue))
         (for/or ([cct (in-hash-values (collects-queue-hash jobqueue))])
            (hasjob? cct)))])
        ;(printf "JOBCNT ~a ~a ~a ~a~n" hj jc (length (collects-queue-cclst jobqueue)) (hash-count (collects-queue-hash jobqueue)))
        hj))
    (define (job-desc jobqueue work)
      work)
    (define (jobs-cnt jobqueue queue)
      (define (count-cct cct)
        (let loop ([cct cct])
          (apply + (map (lambda (x) (+ (length (second x)) (loop (third x)))) cct))))

      (+ (count-cct (collects-queue-cclst jobqueue))
         (for/fold ([cnt 0]) ([cct (in-hash-values (collects-queue-hash jobqueue))])
            (+ cnt (count-cct cct)))))))

(define (parallel-compile worker-count setup-fprintf collects)
  (let ([cd (find-system-path 'collects-dir)]) 
    (setup-fprintf (current-output-port) #f "--- parallel build using ~a processor cores ---" worker-count)
    (process-comp (make-collects-queue collects (make-hash) cd setup-fprintf) worker-count 999999999)))

