#lang racket/base
(require "../common/check.rkt"
         "../host/thread.rkt"
         "port.rkt"
         "input-port.rkt"
         "output-port.rkt"
         "count.rkt"
         "commit-manager.rkt")

(provide make-pipe
         pipe-input-port?
         pipe-output-port?
         pipe-content-length
         pipe-write-position
         pipe-discard-all)

(define (min+1 a b) (if a (min (add1 a) b) b))

(struct pipe-data (get-content-length
                   write-position
                   discard-all))

(define (pipe-input-port? p)
  (and (input-port? p)
       (pipe-data? (core-port-data (->core-input-port p)))))

(define (pipe-output-port? p)
  (and (output-port? p)
       (pipe-data? (core-port-data (->core-output-port p)))))

(define (pipe-content-length p)
  ((pipe-data-get-content-length
    (core-port-data 
     (cond
       [(pipe-input-port? p) (->core-input-port p)]
       [(pipe-output-port? p) (->core-output-port p)]
       [else
        (raise-argument-error 'pipe-contact-length "(or/c pipe-input-port? pipe-output-port?)" p)])))))

(define pipe-write-position
  (case-lambda
    [(p) ((pipe-data-write-position (core-port-data p)))]
    [(p pos) ((pipe-data-write-position (core-port-data p)) pos)]))

(define (pipe-discard-all p)
  ((pipe-data-discard-all (core-port-data p))))

(define/who (make-pipe [limit #f] [input-name 'pipe] [output-name 'pipe])
  (check who #:or-false exact-positive-integer? limit)
  (define bstr (make-bytes (min+1 limit 16)))
  (define len (bytes-length bstr))
  (define peeked-amt 0) ; peeked but not yet read effectively extends `limit`
  (define start 0)
  (define end 0)
  (define write-pos #f) ; to adjust the write position via `file-position` on a string port
  (define input-closed? #f)
  (define output-closed? #f)

  (define (content-length)
    (if (start . <= . end)
        (- end start)
        (+ end (- len start))))
  (define (input-empty?) (= start end))
  (define (output-full?)
    (and limit
         ((content-length) . >= . (+ limit peeked-amt))))

  (define data
    (pipe-data
     ;; get-content-length
     (lambda ()
       (atomically (content-length)))
     ;; write-position
     (case-lambda
       [() (or write-pos end)]
       [(pos)
        ;; `pos` must be between `start` and `end`
        (if (= pos end)
            (set! write-pos #f)
            (set! write-pos pos))])
     ;; discard-all
     (lambda ()
       (set! peeked-amt 0)
       (set! start 0)
       (set! end 0)
       (set! write-pos #f))))

  (define read-ready-sema (make-semaphore))
  (define write-ready-sema (and limit (make-semaphore 1)))
  (define more-read-ready-sema #f) ; for lookahead peeks
  (define read-ready-evt (wrap-evt (semaphore-peek-evt read-ready-sema)
                                   (lambda (v) 0)))
  (define write-ready-evt (if limit
                              (semaphore-peek-evt write-ready-sema)
                              always-evt))
  (define progress-sema #f)

  ;; Used before/after read:
  (define (check-output-unblocking)
    (when (output-full?) (semaphore-post write-ready-sema)))
  (define (check-input-blocking)
    (when (input-empty?) (semaphore-wait read-ready-sema)))

  ;; Used before/after write:
  (define (check-input-unblocking)
    (when (and (input-empty?) (not output-closed?)) (semaphore-post read-ready-sema))
    (when more-read-ready-sema
      (semaphore-post more-read-ready-sema)
      (set! more-read-ready-sema #f)))
  (define (check-output-blocking)
    (when (output-full?) (semaphore-wait write-ready-sema)))

  ;; Used after peeking:
  (define (peeked! amt)
    (when (amt . > . peeked-amt)
      (check-output-unblocking)
      (set! peeked-amt amt)))

  (define (progress!)
    (when progress-sema
      (semaphore-post progress-sema)
      (set! progress-sema #f)))

  (define commit-manager #f)

  ;; in atomic mode [can leave atomic mode temporarily]
  ;; After this function returns, complete any commit-changing work
  ;; before leaving atomic mode again.
  (define (pause-waiting-commit)
    (when commit-manager
      (commit-manager-pause commit-manager)))

  ;; in atomic mode [can leave atomic mode temporarily]
  (define (wait-commit progress-evt ext-evt finish)
    (cond
      [(and (not commit-manager)
            ;; Try shortcut:
            (not (sync/timeout 0 progress-evt))
            (sync/timeout 0 ext-evt))
       (finish)
       #t]
      [else
       ;; General case to support blocking and potentially multiple
       ;; commiting threads:
       (unless commit-manager
         (set! commit-manager (make-commit-manager)))
       (commit-manager-wait commit-manager progress-evt ext-evt finish)]))

  ;; input ----------------------------------------
  (define ip
    (make-core-input-port
     #:name input-name
     #:data data

     #:prepare-change
     (lambda ()
       (pause-waiting-commit))
     
     #:read-byte
     (lambda ()
       (cond
         [(input-empty?)
          (if output-closed?
              eof
              ;; event's synchronization value is ignored:
              read-ready-evt)]
         [else
          (define pos start)
          (check-output-unblocking)
          (set! start (add1 pos))
          (set! peeked-amt (max 0 (sub1 peeked-amt)))
          (when (= start len)
            (set! start 0))
          (check-input-blocking)
          (progress!)
          (bytes-ref bstr pos)]))

     #:read-in
     (lambda (dest-bstr dest-start dest-end copy?)
       (cond
         [(input-empty?)
          (if output-closed?
              eof
              read-ready-evt)]
         [else
          (check-output-unblocking)
          (begin0
            (cond
              [(start . < . end)
               (define amt (min (- dest-end dest-start)
                                (- end start)))
               (bytes-copy! dest-bstr dest-start bstr start (+ start amt))
               (set! start (+ start amt))
               (set! peeked-amt (max 0 (- peeked-amt amt)))
               amt]
              [else
               (define amt (min (- dest-end dest-start)
                                (- len start)))
               (bytes-copy! dest-bstr dest-start bstr start (+ start amt))
               (set! start (modulo (+ start amt) len))
               (set! peeked-amt (max 0 (- peeked-amt amt)))
               amt])
            (check-input-blocking)
            (progress!))]))

     #:peek-byte
     (lambda ()
       (cond
         [(input-empty?)
          (if output-closed?
              eof
              read-ready-evt)]
         [else
          (peeked! 1)
          (bytes-ref bstr start)]))
     
     #:peek-in
     (lambda (dest-bstr dest-start dest-end skip progress-evt copy?)
       (define content-amt (content-length))
       (cond
         [(and progress-evt
               (sync/timeout 0 progress-evt))
          #f]
         [(content-amt . <= . skip)
          (cond
            [output-closed? eof]
            [else
             (unless (or (zero? skip) more-read-ready-sema)
               (set! more-read-ready-sema (make-semaphore)))
             (define evt (if (zero? skip)
                             read-ready-evt
                             (wrap-evt (semaphore-peek-evt more-read-ready-sema)
                                       (lambda (v) 0))))
             evt])]
         [else
          (define peek-start (modulo (+ start skip) len))
          (cond
            [(peek-start . < . end)
             (define amt (min (- dest-end dest-start)
                              (- end peek-start)))
             (bytes-copy! dest-bstr dest-start bstr peek-start (+ peek-start amt))
             (peeked! (+ skip amt))
             amt]
            [else
             (define amt (min (- dest-end dest-start)
                              (- len peek-start)))
             (bytes-copy! dest-bstr dest-start bstr peek-start (+ peek-start amt))
             (peeked! (+ skip amt))
             amt])]))

     #:byte-ready
     (lambda (work-done!)
       (or output-closed?
           (not (zero? (content-length)))))

     #:close
     (lambda ()
       (unless input-closed?
         (set! input-closed? #t)
         (progress!)))

     #:get-progress-evt
     (lambda ()
       (atomically
        (cond
          [input-closed? always-evt]
          [else
           (unless progress-sema
             (set! progress-sema (make-semaphore)))
           (semaphore-peek-evt progress-sema)])))

     #:commit
     ;; Allows `amt` to be zero and #f for other arguments,
     ;; which is helpful for `open-input-peek-via-read`.
     (lambda (amt progress-evt ext-evt finish)
       ;; `progress-evt` is a `semepahore-peek-evt`, and `ext-evt`
       ;; is constrained; we can send them over to different threads
       (cond
         [(zero? amt)
          (progress!)]
         [else
          (wait-commit
           progress-evt ext-evt
           ;; in atomic mode, maybe in a different thread:
           (lambda ()
             (let ([amt (min amt (content-length))])
               (cond
                 [(zero? amt)
                  ;; There was nothing to commit; claim success for 0 bytes
                  (finish #"")]
                 [else
                  (define dest-bstr (make-bytes amt))
                  (cond
                    [(start . < . end)
                     (bytes-copy! dest-bstr 0 bstr start (+ start amt))]
                    [else
                     (define amt1 (min (- len start) amt))
                     (bytes-copy! dest-bstr 0 bstr start (+ start amt1))
                     (when (amt1 . < . amt)
                       (bytes-copy! dest-bstr amt1 bstr 0 (- amt amt1)))])
                  (set! start (modulo (+ start amt) len))
                  (progress!)
                  (check-input-blocking)
                  (finish dest-bstr)]))))]))))

  ;; out ----------------------------------------
  (define op
    (make-core-output-port
     #:name output-name
     #:data data

     #:evt write-ready-evt
     
     #:write-out
     ;; in atomic mode
     (lambda (src-bstr src-start src-end nonblock? enable-break? copy?)
       (let try-again ()
         (define top-pos (if (zero? start)
                             (sub1 len)
                             len))
         (define (maybe-grow)
           (cond
             [(or (not limit)
                  ((+ limit peeked-amt) . > . (sub1 len)))
              ;; grow pipe size
              (define new-bstr (make-bytes (min+1 (and limit (+ limit peeked-amt)) (* len 2))))
              (cond
                [(zero? start)
                 (bytes-copy! new-bstr 0 bstr 0 (sub1 len))]
                [else
                 (bytes-copy! new-bstr 0 bstr start len)
                 (bytes-copy! new-bstr (- len start) bstr 0 end)
                 (set! start 0)
                 (set! end (sub1 len))])
              (set! bstr new-bstr)
              (set! len (bytes-length new-bstr))
              (try-again)]
             [else (pipe-is-full)]))
         (define (pipe-is-full)
           (wrap-evt write-ready-evt (lambda (v) #f)))
         (define (apply-limit amt)
           (if limit
               (min amt (- (+ limit peeked-amt) (content-length)))
               amt))
         (cond
           [(= src-start src-end) ;; => flush
            0]
           [write-pos ; set by `file-position` on a bytes port
            (define amt (apply-limit (min (- end write-pos)
                                          (- src-end src-start))))
            (cond
              [(zero? amt) (pipe-is-full)]
              [else
               (check-input-unblocking)
               (bytes-copy! bstr write-pos src-bstr src-start (+ src-start amt))
               (let ([new-write-pos (+ write-pos amt)])
                 (if (= new-write-pos end)
                     (set! write-pos #f) ; back to normal mode
                     (set! write-pos new-write-pos)))
               (check-output-blocking)
               amt])]
           [(and (end . >= . start)
                 (end . < . top-pos))
            (define amt (apply-limit (min (- top-pos end)
                                          (- src-end src-start))))
            (cond
              [(zero? amt) (pipe-is-full)]
              [else
               (check-input-unblocking)
               (bytes-copy! bstr end src-bstr src-start (+ src-start amt))
               (let ([new-end (+ end amt)])
                 (set! end (if (= new-end len) 0 new-end)))
               (check-output-blocking)
               amt])]
           [(= end top-pos)
            (cond
              [(zero? start)
               (maybe-grow)]
              [else
               (define amt (min (sub1 start)
                                (- src-end src-start)))
               (cond
                 [(zero? amt) (pipe-is-full)]
                 [else
                  (check-input-unblocking)
                  (bytes-copy! bstr 0 src-bstr src-start (+ src-start amt))
                  (set! end amt)
                  (check-output-blocking)
                  amt])])]
           [(end . < . (sub1 start))
            (define amt (apply-limit (min (- (sub1 start) end)
                                          (- src-end src-start))))
            (cond
              [(zero? amt) (pipe-is-full)]
              [else
               (check-input-unblocking)
               (bytes-copy! bstr end src-bstr src-start (+ src-start amt))
               (set! end (+ end amt))
               (check-output-blocking)
               amt])]
           [else
            (maybe-grow)])))

     #:count-write-evt-via-write-out
     (lambda (v bstr start)
       (port-count! op v bstr start))

     #:close
     ;; in atomic mode
     (lambda ()
       (unless output-closed?
         (set! output-closed? #t)
         (when write-ready-sema
           (semaphore-post write-ready-sema))
         (when more-read-ready-sema
           (semaphore-post more-read-ready-sema))
         (semaphore-post read-ready-sema)))))

  ;; Results ----------------------------------------
  (when (port-count-lines-enabled)
    (port-count-lines! ip)
    (port-count-lines! op))
  
  (values ip op))
