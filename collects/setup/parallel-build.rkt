#lang racket/base

(require compiler/cm
         racket/list
         racket/match
         racket/path
         setup/collects
         setup/parallel-do
         racket/class)

(define Lock-Manager% (class object%
  (field (locks (make-hash)))
  (define/public (lock fn wrkr)
    (let ([v (hash-ref locks fn #f)])
      (hash-set! locks fn
        (if v
          (match v [(list w waitlst) (list w (append waitlst (list wrkr)))])
          (begin
            (send wrkr send/msg 'locked)
            (list wrkr null))))
      (not v)))
  (define/public (unlock fn)
    (for ([x (second (hash-ref locks fn))])
      (wrkr/send x 'compiled))
    (hash-remove! locks fn))
  (super-new)))

(provide parallel-compile
         parallel-build-worker)


(define CollectsQueue% (class* object% (WorkQueue<%>) 
  (init-field cclst collects-dir printer append-error)
  (field (lock-mgr (new Lock-Manager%)))
  (field (hash (make-hash)))
  (inspect #f)

  (define/public (work-done work wrkr msg)
    (match (list work msg)
      [(list (list cc file last) (list result-type out err))
        (begin0
          (match result-type
            [(list 'ERROR msg)
              (append-error cc "making" (exn msg (current-continuation-marks)) out err "error")
              #t]
            ;[(list 'LOCK fn) (lock fn wrkr) #f]
            ;[(list 'UNLOCK fn) (unlock fn) #f]
            ['DONE
              (define (string-!empty? s) (not (zero? (string-length s))))
              (when (ormap string-!empty? (list out err))
                (append-error cc "making" null out err "output"))
                #t])
          (when last (printer (current-output-port) "made" "~a" (cc-name cc))))]
      [else
        (match work 
          [(list-rest (list cc file last) message)
            (append-error cc "making" null "" "" "error")
            (eprintf "work-done match cc failed.\n")
            (eprintf "trying to match:\n~a\n" (list work msg))
            #t]
          [else
            (eprintf "work-done match cc failed.\n")
            (eprintf "trying to match:\n~a\n" (list work msg))
            (eprintf "FATAL\n")
            (exit 1)])]))
         
    ;; assigns a collection to each worker to be compiled
    ;; when it runs out of collections, steals work from other workers collections
    (define/public (get-job workerid)
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
        (match cclst
          [(list) #f]
          [(cons h t)
            (set! cclst t)
            (list h)]))
      (let ([w-hash hash])
        (define (build-job cc file last)
          (define (->bytes x)
            (cond [(path? x) (path->bytes x)]
                  [(string? x) (string->bytes/locale x)]))
          (let* ([cc-name (cc-name cc)]
                 [cc-path (cc-path cc)]
                 [full-path (path->string (build-path cc-path file))])
            ;(printf "JOB ~a ~a ~a ~a\n" workerid cc-name cc-path file)
            (values (list cc file last) (list cc-name (->bytes cc-path) (->bytes file)))))
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
                  (build-job cc file #t)]
              [(cons (list cc (cons file ft) subs) tail)
                (hash-set! w-hash id (cons (list cc ft subs) tail))
                (build-job cc file #f)]
              [else
                (eprintf "get-job match cc failed.\n")
                (eprintf "trying to match:\n~a\n" cc)]))
 
          (match (hash-ref!/true w-hash workerid take-cc)
            [#f 
                (match (hash/first-pair w-hash)
                  [(cons id cc) (find-job-in-cc cc id)])]
            [cc (find-job-in-cc cc workerid)]))))

    (define/public (has-jobs?)
      (define (hasjob?  cct)
        (let loop ([cct cct])
          (ormap (lambda (x) (or ((length (second x)) . > . 0) (loop (third x)))) cct)))

      (or (hasjob? cclst)
          (for/or ([cct (in-hash-values hash)])
            (hasjob? cct))))

    (define/public (jobs-cnt)
      (define (count-cct cct)
        (let loop ([cct cct])
          (apply + (map (lambda (x) (+ (length (second x)) (loop (third x)))) cct))))

      (+ (count-cct cclst)
         (for/fold ([cnt 0]) ([cct (in-hash-values hash)])
            (+ cnt (count-cct cct)))))
    (define/public (get-results) (void))
    (super-new)))

(define (parallel-compile worker-count setup-fprintf append-error collects-tree)
  (let ([collects-dir (current-collects-path)]) 
    (setup-fprintf (current-output-port) #f "--- parallel build using ~a processor cores ---" worker-count)
    (parallel-do-event-loop #f
                            values ; identity function
                            (list (current-executable-path)
                                  "-X"
                                  (path->string collects-dir)
                                  "-l"
                                  "setup/parallel-build-worker.rkt")
                            (make-object CollectsQueue% collects-tree collects-dir setup-fprintf append-error)
                            worker-count 999999999)))

(define (parallel-build-worker)
  (define prev-uncaught-exception-handler (uncaught-exception-handler))
  (uncaught-exception-handler (lambda (x)
    (when (exn:break? x) (exit 1))
    (prev-uncaught-exception-handler x)))

  (let ([cmc (make-caching-managed-compile-zo)]
        [worker-id (read)])
   (let loop ()
     (match (read)
       [(list 'DIE) void]
       [(list name dir file)
         (let ([dir (bytes->path dir)]
               [file (bytes->path file)])
          (let ([out-str-port (open-output-string)]
                [err-str-port (open-output-string)])
            (define (send/resp type)
              (let ([msg (list type (get-output-string out-str-port) (get-output-string err-str-port))])
                (write msg)))
            (let ([cep (current-error-port)])
              (define (pp x)
                (fprintf cep "COMPILING ~a ~a ~a ~a\n" worker-id name file x))
            (with-handlers ([exn:fail? (lambda (x)
                             (send/resp (list 'ERROR (exn-message x))))])
              (parameterize (
                             [current-namespace (make-base-empty-namespace)]
                             [current-directory dir]
                             [current-load-relative-directory dir]
                             [current-output-port out-str-port]
                             [current-error-port err-str-port]
                             ;[manager-compile-notify-handler pp]
                            )

                (cmc (build-path dir file)))
              (send/resp 'DONE))))
          (flush-output)
          (loop))]))))
