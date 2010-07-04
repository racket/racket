#lang racket

(require raco/command-name)
(require racket/future)
(require unstable/generics)
(require setup/collects)

(provide parallel-compile)

(define-struct node (path children parents) #:mutable #:prefab)


(define (get-dirs-files path) (partition (λ (x) (directory-exists? (build-path path x))) (directory-list path)))
(define (get-dirs path)       (filter    (λ (x) (directory-exists? (build-path path x))) (directory-list path)))
(define (get-files path)      (filter    (λ (x) (file-exists?      (build-path path x))) (directory-list path)))
(define (sort-path x) (sort x (λ (a b) (string<? (path->string a) (path->string b)))))

(define (find-dep-files path)
  (define isdep? (regexp "\\.dep$"))
  (let loop 
    ([next (list path)] [matched null])
    (match next
      [(cons h t) 
       (let*-values ([(c nc) (partition (λ (x) (string=? (path->string x) "compiled")) (get-dirs h))]
                     [(mfiles) (if (pair? c)
                                   (map (curry build-path h (car c)) 
                                        (sort-path(filter (λ (x) (regexp-match isdep? (path->string x))) 
                                                          (get-files (build-path h (car c))))))
                                   null)])
         (loop (append t (map (curry build-path h) (sort-path nc)))
               (append matched mfiles)))] 
      
      [else matched])))

(define (build-dag collects-path)
  (define dag (make-hash))
  (define (get-dag-node dag path) (hash-ref! dag path (λ () (make-node (string->bytes/locale path) null null))))
  (define (dep-path->collect-path path)
    (match (regexp-match "/collects/(.*)/compiled(.*)_(.*)\\.dep$" (path->string path))
    [(list a b c d) (string-append b c "." d)]
    [else (raise "BAD MATCH")]))
  (define (get-deps path)
    (foldl (λ (x init) 
             (match x
               [(list-rest 'collects rest) (cons (path->string (apply build-path (map bytes->string/locale rest))) init)]
               [else init])) 
           null
           (with-input-from-file path read)))
  (for ([file (find-dep-files collects-path)])
    (let ([deps (get-deps file)]
          [path (dep-path->collect-path file)])
      (let ([node (get-dag-node dag path)])
        (for ([dep deps])
          (let ([dep-node (get-dag-node dag dep)])
            (set-node-children! node (cons dep-node (node-children node)))
            (set-node-parents! dep-node (cons node (node-parents dep-node))))))))
  dag)

(define (children-names n)
  (map node-path (node-children n)))

(define (find-initials dag) 
  (for/fold ([ready null]) ([n (in-hash-values dag)])
          (match n 
            [(struct node (path '() ps)) (cons n ready)]
            [else ready])))

(define (compile-done dag node)
    (hash-remove! dag (node-path node))
    (let loop ([ready null]
               [todo  (node-parents node)])
      (match todo
        [(list) ready]
        [(cons depnode t)
         (set-node-children! depnode (filter (λ (x) (not (equal? node x))) (node-children depnode)))
         
         (loop 
          (if (null? (node-children depnode))
              (cons depnode ready)
              ready)
          t)])))

(define (sort-profit x)
  (define (count-depend-only-on me)
    (for/fold ([cnt 0]) ([p (node-parents me)])               
      (if (> 2 (length (node-children p)))
          (+ cnt 1)
          cnt)))  
  (sort x (λ (x y) (> (count-depend-only-on x) (count-depend-only-on y)))))

(define (node-path-str x) (bytes->string/locale (node-path x)))

(define-generics (jobqueue prop:jobqueue jobqueue?)
  (work-done jobqueue queue work workerid msg)
  (get-job jobqueue queue workerid)
  (has-jobs? jobqueue queue)
  (jobs-cnt jobqueue queue)
  (job-desc jobqueue wokr)
  (initial-queue jobqueue))

(define-struct dag-queue (dag collects-dir) #:transparent
  #:property prop:jobqueue
  (define-methods jobqueue
    (define (initial-queue jobqueue)
      (sort-profit (find-initials (dag-queue-dag jobqueue))))
    (define (work-done jobqueue queue work workerid msg)
       (sort-profit (append queue (compile-done (dag-queue-dag jobqueue) work))))
    (define (get-job jobqueue queue workerid)
      (match queue
        [(cons node rest) 
          (values 
            rest
            node
            (path->string (build-path (dag-queue-collects-dir jobqueue) (node-path-str node))))]))
    (define (has-jobs? jobqueue queue)
      (not (null? queue)))
    (define (job-desc jobqueue work)
      (node-path work))
    (define (jobs-cnt jobqueue queue)
      (length queue))))

(define (splat txt fn)
  (call-with-output-file fn #:exists 'replace
      (lambda (out)
        (fprintf out "~a" txt))))

#|
(define (places-comp jobqueue nprocs stopat)
  (define place-worker-filename "pct1.rkt")
  (define place-worker-src
#<<END
 (module pct1 racket
   (provide place-main)
   
   (define (place-main ch)     
     (let ([mc ((dynamic-require 'compiler/cm 'make-caching-managed-compile-zo))])
       (let loop ()
         (match (place-channel-recv ch)
           ["DIE" void]
           [file
            (with-handlers ([exn:fail? (lambda (x) (printf "PLACE WORKER ERROR ~a~n" x) (place-channel-send ch "ERROR"))])
              (parameterize ([current-namespace (make-base-empty-namespace)]) 
                (let-values ([(r t1 t2 t3) (time-apply mc (list file))])
                  (place-channel-send ch (list "DONE" t1 t2 t3)))))
            (loop)]))))
 )
END

  (define ps (for/list ([i (in-range nprocs)]) (place place-worker-filename 'place-main)))
  (define (jobs? queue)
    (has-jobs? jobqueue queue))
  (define (empty? queue)
    (not (has-jobs? jobqueue queue)))
  
  (splat place-worker-src place-worker-filename)
  (letrec ([loop (match-lambda* 
                   [(list queue waiters inflight (? (lambda (x) (= x stopat))))   (printf "DONE WITH LIMIT~n")]
                   [(list (? jobs? queue-state) (cons worker wt) inflight count) 
                              (let-values ([(queue-state job cmd-list) (get-job jobqueue queue-state 0)])
                                (place-channel-send worker cmd-list)
                                (loop queue-state wt (cons (list worker job) inflight) count))]
                   [(list (? empty?) waiters (list) count) (void)]
                   [(list queue waiters inflight count)
                     (let ([report-done (λ (x p node msg)
                             (list 
                                 (work-done jobqueue queue node 0 msg)
                                 (cons p waiters)
                                 (remove x inflight)
                                 (+ count 1)))])
                       (apply loop (apply sync (map (λ (x) (match x
                                               [(list p node) 
                                                (handle-evt p (λ (e)
                                                              ;(printf "RECV ~a~n")
                                                              (match (place-channel-recv p)
                                                                ["ERROR" (printf "ERROR ~a ~a ~a ~a ~a~n" count (length waiters) (length inflight) (jobs-cnt jobqueue) (node-path node))]
                                                                [(list D t1 t2 t3) 
                                                                    (when (= 0 (modulo count 100))  (printf "DONE ~a ~a ~a ~a ~a ~a ~a ~a~n" count (length waiters) (length inflight) (jobs-cnt jobqueue) (node-path node) t1 t2 t3))])
                                                              (report-done x p node "")))]))
                                    
                                    inflight))))])])
    (loop (initial-queue jobqueue) ps null 0))
  
  (for ([p ps]) (place-channel-send p "DIE"))
  (for ([p ps]) (place-wait p)))
|#

(define (process-comp jobqueue nprocs stopat)
  (define process-worker-filename
    (path->string (build-path (collection-path "setup") "parallel-build-worker.rkt")))

  (define executable (parameterize ([current-directory (find-system-path 'orig-dir)])
                       (find-executable-path (find-system-path 'exec-file) #f)))
  (define (send/msg x ch)
    (write x ch)
    (flush-output ch))
  (define (spawn i)
    (let-values ([(s o in e) (subprocess #f #f (current-error-port) executable process-worker-filename)])
      (send/msg i in)
      (list i s o in e)))
  (define (kill-worker i nw o in)
     (eprintf "KILLING WORKER ~a ~a ~n" i nw)
     (close-input-port o)
     (close-output-port in))
  (define workers (for/list ([i (in-range nprocs)]) (spawn i)))
  (define (jobs? queue)
    (has-jobs? jobqueue queue))
  (define (empty? queue)
    (not (has-jobs? jobqueue queue)))
 
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
    (loop (initial-queue jobqueue) workers null 0))
  
  (for ([p workers]) (send/msg (list 'DIE) (fourth p)))
  (for ([p workers]) (subprocess-wait (second p))))


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


(define (build-dag->file collects-dir dest-filename)
  (with-output-to-file dest-filename #:exists 'replace (λ () (write (build-dag collects-dir)))))
(define (file->dag dag-path)
  (hash-copy (with-input-from-file dag-path (λ () (read)))))

(define (build-dag-queue dagfile collects-dir) 
  (let ([dag (file->dag dagfile)])
    (make-dag-queue dag collects-dir)))
  
(define (absolute-collects-path)
  (simplify-path (find-executable-path (find-system-path 'exec-file) (find-system-path 'collects-dir))))

(match (current-command-name)
  ;; called from raco
  ["build-dag" (let ([cd (absolute-collects-path)])
    (build-dag->file cd (build-path cd "setup/dag")))]
  ["parallel-build" (let ([cd (absolute-collects-path)]) 
    (printf "Using ~a processor cores~n" (processor-count))
    (process-comp (build-dag-queue (build-path cd "setup/dag")) cd (processor-count) 999999999))]
  [#f (void)])
