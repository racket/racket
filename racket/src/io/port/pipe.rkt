#lang racket/base
(require racket/fixnum
         "../common/check.rkt"
         "../common/class.rkt"
         "../host/thread.rkt"
         "port.rkt"
         "input-port.rkt"
         "output-port.rkt"
         "count.rkt"
         "commit-port.rkt")

(provide make-pipe
         make-pipe-ends
         (rename-out [pipe-input-port?* pipe-input-port?]
                     [pipe-output-port?* pipe-output-port?])
         pipe-content-length)

(define (min+1 a b) (if a (min (add1 a) b) b))

(define pipe-input-port?*
  (let ([pipe-input-port?
         (lambda (p)
           (define cp (->core-input-port p))
           (pipe-input-port? p))])
    pipe-input-port?))

(define pipe-output-port?*
  (let ([pipe-output-port?
         (lambda (p)
           (define cp (->core-output-port p))
           (pipe-output-port? p))])
    pipe-output-port?))

(define (pipe-content-length p)
  (define d
    (cond
      [(let ([p (->core-input-port p)])
         (and p
              (pipe-input-port? p)
              p))
       => (lambda (p) (pipe-input-port-d p))]
      [(let ([p (->core-output-port p)])
         (and p
              (pipe-output-port? p)
              p))
       => (lambda (p) (pipe-output-port-d p))]
      [else
       (raise-argument-error 'pipe-contact-length "(or/c pipe-input-port? pipe-output-port?)" p)]))
  (atomically
   (let ([input (pipe-data-input d)])
     (when input (send pipe-input-port input sync-data)))
   (let ([output (pipe-data-output d)])
     (when output (send pipe-output-port output sync-data)))
   (send pipe-data d content-length)))

;; ----------------------------------------

(class pipe-data
  (field
   [bstr #""]
   [len 0]
   [limit 0]
   [peeked-amt 0] ; peeked but not yet read, effectively extends `limit`
   [start 0]
   [end 0]
   [input #f]  ; #f => closed
   [output #f] ; #f => closed
   [read-ready-sema #f]
   [write-ready-sema #f]
   [more-read-ready-sema #f] ; for lookahead peeks
   [read-ready-evt #f]
   [write-ready-evt #f])

  ;; All methods in atomic mode.
  ;; Beware that the input port must be synced to sure that `start`
  ;; represents the current position before using these methods.
  (static
   [content-length
    (lambda ()
      (define s start)
      (define e end)
      (if (s . fx<= . e)
          (fx- e s)
          (fx+ e (fx- len s))))]

   [input-empty?
    (lambda ()
      (fx= start end))]

   [output-full?
    (lambda ()
      (define l limit)
      (and l
           ((content-length) . >= . (+ l peeked-amt))))]

   ;; Used before/after read:
   [check-output-unblocking
    (lambda ()
      (when (output-full?) (semaphore-post write-ready-sema)))]
   [check-input-blocking
    (lambda ()
      (when (input-empty?)
        (semaphore-wait read-ready-sema)
        (when output
          (send pipe-output-port output on-input-empty))))]

   ;; Used before/after write:
   [check-input-unblocking
    (lambda ()
      (when (and (input-empty?) output) (semaphore-post read-ready-sema))
      (when more-read-ready-sema
        (semaphore-post more-read-ready-sema)
        (set! more-read-ready-sema #f)))]
   [check-output-blocking
    (lambda ()
      (when (output-full?)
        (semaphore-wait write-ready-sema)
        (when input
          (send pipe-input-port input on-output-full))))]

   ;; Used after peeking:
   [peeked!
    (lambda (amt)
      (when (amt . > . peeked-amt)
        (check-output-unblocking)
        (set! peeked-amt amt)))]))

;; ----------------------------------------

(class pipe-input-port #:extends commit-input-port
  (field
   [d #f]) ; pipe-data

  (private
    [fast-mode!
     (lambda (amt) ; amt = not yet added to `offset`
       (with-object pipe-data d
         (define s start)
         (define e end)
         (unless (fx= s e)
           (set! buffer bstr)
           (set! buffer-pos s)
           ;; don't read last byte, because the output
           ;; end needs to know about a transition to
           ;; the empty state
           (set! buffer-end (fx- (if (s . fx< . e) e len) 1))
           (define o offset)
           (when o
             (set! offset (- (+ o amt) s))))))]

    [slow-mode!
     (lambda ()
       (with-object pipe-data d
         (when buffer
           (define pos buffer-pos)
           (define o offset)
           (when o
             (set! offset (+ o pos)))
           (set! start (if (fx= pos len) 0 pos))
           (set! buffer #f)
           (set! buffer-pos buffer-end))
         (define out output)
         (when out
           (send pipe-output-port out sync-data))))])

  (static
   [sync-data
    (lambda ()
      (when buffer
        (with-object pipe-data d
          (define pos buffer-pos)
          (set! start (if (fx= pos len)
                          0
                          pos)))))]
   [sync-data-both
    (lambda ()
      (sync-data)
      (with-object pipe-data d
        (define out output)
        (when out
          (send pipe-output-port out sync-data))))]
   [on-resize
    (lambda ()
      (slow-mode!))]
   [on-output-full
    (lambda ()
      (slow-mode!))])

  (override
    [prepare-change
     (lambda ()
       (with-object pipe-data d
         (pause-waiting-commit)))]

    [read-in
     (lambda (dest-bstr dest-start dest-end copy?)
       (assert-atomic)
       (slow-mode!)
       (with-object pipe-data d
         (cond
           [(input-empty?)
            (if output
                read-ready-evt
                eof)]
           [else
            (check-output-unblocking)
            (define s start)
            (define e end)
            (define amt
              (cond
                [(s . fx< . e)
                (define amt (fxmin (fx- dest-end dest-start)
                                   (fx- e s)))
                (bytes-copy! dest-bstr dest-start bstr s (fx+ s amt))
                (set! start (fx+ s amt))
                (set! peeked-amt (fxmax 0 (fx- peeked-amt amt)))
                amt]
               [else
                (define amt (fxmin (fx- dest-end dest-start)
                                   (fx- len s)))
                (bytes-copy! dest-bstr dest-start bstr s (fx+ s amt))
                (set! start (modulo (fx+ s amt) len))
                (set! peeked-amt (fxmax 0 (fx- peeked-amt amt)))
                amt]))
            (check-input-blocking)
            (progress!)
            (fast-mode! amt)
            amt])))]

    [peek-in
     (lambda (dest-bstr dest-start dest-end skip progress-evt copy?)
       (with-object pipe-data d
         (assert-atomic)
         (sync-data-both)
         (define content-amt (content-length))
         (cond
           [(and progress-evt
                 (sync/timeout 0 progress-evt))
            #f]
           [(content-amt . <= . skip)
            (cond
              [(not output) eof]
              [else
               (unless (or (zero? skip) more-read-ready-sema)
                 (set! more-read-ready-sema (make-semaphore))
                 (when output
                   (send pipe-output-port output on-need-more-ready)))
               (define evt (if (zero? skip)
                               read-ready-evt
                               (wrap-evt (semaphore-peek-evt more-read-ready-sema)
                                         (lambda (v) 0))))
               evt])]
           [else
            (define peek-start (fxmodulo (fx+ start skip) len))
            (cond
              [(peek-start . fx< . end)
               (define amt (fxmin (fx- dest-end dest-start)
                                  (fx- end peek-start)))
               (bytes-copy! dest-bstr dest-start bstr peek-start (fx+ peek-start amt))
               (peeked! (+ skip amt))
               amt]
              [else
               (define amt (fxmin (fx- dest-end dest-start)
                                  (fx- len peek-start)))
               (bytes-copy! dest-bstr dest-start bstr peek-start (fx+ peek-start amt))
               (peeked! (+ skip amt))
               amt])])))]

    [byte-ready
     (lambda (work-done!)
       (assert-atomic)
       (with-object pipe-data d
         (or (not output)
             (begin
               (sync-data-both)
               (not (fx= 0 (content-length)))))))]

    [close
     (lambda ()
       (with-object pipe-data d
         (when input
           (slow-mode!)
           (set! input #f)
           (progress!))))]

    [get-progress-evt
     (lambda ()
       (atomically
        (with-object pipe-data d
          (cond
            [(not input) always-evt]
            [else
             (slow-mode!)
             (make-progress-evt)]))))]

    [commit
     ;; Allows `amt` to be zero and #f for other arguments,
     ;; which is helpful for `open-input-peek-via-read`.
     (lambda (amt progress-evt ext-evt finish)
       (assert-atomic)
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
             (with-object pipe-data d
               (slow-mode!)
               (let ([amt (min amt (content-length))])
                 (cond
                   [(fx= 0 amt)
                    ;; There was nothing to commit; claim success for 0 bytes
                    (finish #"")]
                   [else
                    (define dest-bstr (make-bytes amt))
                    (define s start)
                    (define e end)
                    (cond
                      [(s . fx< . e)
                       (bytes-copy! dest-bstr 0 bstr s (fx+ s amt))]
                      [else
                       (define amt1 (fxmin (fx- len s) amt))
                       (bytes-copy! dest-bstr 0 bstr s (fx+ s amt1))
                       (when (amt1 . fx< . amt)
                         (bytes-copy! dest-bstr amt1 bstr 0 (fx- amt amt1)))])
                    (set! start (fxmodulo (fx+ s amt) len))
                    (progress!)
                    (fast-mode! amt)
                    (check-input-blocking)
                    (finish dest-bstr)])))))]))]

    [count-lines!
     (lambda ()
       (slow-mode!))]))

;; ----------------------------------------

(class pipe-output-port #:extends core-output-port
  (field
   [d d]) ; pipe-data

  (private
    [fast-mode!
     (lambda (amt) ; amt = not yet added to `offset`
       (with-object pipe-data d
         (define lim limit)
         (define avail (and lim (- lim (content-length)
                                   ;; don't fill last byte, because the input
                                   ;; end needs to know about a trasition to the
                                   ;; full state
                                   1)))
         (when (or (not avail) (avail . <= . 0))
           (define s start)
           (define e end)             
           (set! buffer bstr)
           (set! buffer-pos e)
           (set! buffer-end (let ([end (if (s . fx<= . e)
                                           (if (fx= s 0)
                                               (fx- len 1)
                                               len)
                                           (fx- s 1))])
                              (if (and avail
                                       ((fx- end e) . > . avail))
                                  (fx+ e avail)
                                  end)))
           (define o offset)
           (when o
             (set! offset (- (+ o amt) e))))))]

    [slow-mode!
     (lambda ()
       (with-object pipe-data d
         (when buffer
           (define pos buffer-pos)
           (define o offset)
           (when o
             (set! offset (+ o pos)))
           (set! end (if (fx= pos len) 0 pos))
           (set! buffer #f)
           (set! buffer-pos buffer-end))
         (define in input)
         (when in
           (send pipe-input-port in sync-data))))])

  (static
   [sync-data
    (lambda ()
      (when buffer
        (with-object pipe-data d
          (define pos buffer-pos)
          (set! end (if (fx= pos len)
                        0
                        pos)))))]
   [sync-data-both
    (lambda ()
      (sync-data)
      (with-object pipe-data d
        (define in input)
        (when in
          (send pipe-output-port in sync-data #f))))]
   [on-input-empty
    (lambda ()
      (slow-mode!))]
   [on-need-more-ready
    (lambda ()
      (slow-mode!))])

  (override
    [write-out
     ;; in atomic mode
     (lambda (src-bstr src-start src-end nonblock? enable-break? copy?)
       (assert-atomic)
       (slow-mode!)
       (with-object pipe-data d
         (send pipe-input-port input sync-data)
         (let try-again ()
           (define top-pos (if (fx= start 0)
                               (fx- len 1)
                               len))
           (define (maybe-grow)
             (cond
               [(or (not limit)
                    ((+ limit peeked-amt) . > . (fx- len 1)))
                ;; grow pipe size
                (send pipe-input-port input on-resize)
                (define new-bstr (make-bytes (min+1 (and limit (+ limit peeked-amt)) (* len 2))))
                (cond
                  [(fx= 0 start)
                   (bytes-copy! new-bstr 0 bstr 0 (fx- len 1))]
                  [else
                   (bytes-copy! new-bstr 0 bstr start len)
                   (bytes-copy! new-bstr (fx- len start) bstr 0 end)
                   (set! start 0)
                   (set! end (fx- len 1))])
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
             [(fx= src-start src-end) ;; => flush
              0]
             [(and (end . fx>= . start)
                   (end . fx< . top-pos))
              (define amt (apply-limit (fxmin (fx- top-pos end)
                                              (fx- src-end src-start))))
              (cond
                [(fx= amt 0) (pipe-is-full)]
                [else
                 (check-input-unblocking)
                 (bytes-copy! bstr end src-bstr src-start (fx+ src-start amt))
                 (let ([new-end (fx+ end amt)])
                   (set! end (if (fx= new-end len) 0 new-end)))
                 (check-output-blocking)
                 (fast-mode! amt)
                 amt])]
             [(fx= end top-pos)
              (cond
                [(fx= start 0)
                 (maybe-grow)]
                [else
                 (define amt (fxmin (fx- start 1)
                                    (fx- src-end src-start)))
                 (cond
                   [(fx= amt 0) (pipe-is-full)]
                   [else
                    (check-input-unblocking)
                    (bytes-copy! bstr 0 src-bstr src-start (fx+ src-start amt))
                    (set! end amt)
                    (check-output-blocking)
                    (fast-mode! amt)
                    amt])])]
             [(end . fx< . (fx- start 1))
              (define amt (apply-limit (fxmin (fx- (fx- start 1) end)
                                              (fx- src-end src-start))))
              (cond
                [(fx= amt 0) (pipe-is-full)]
                [else
                 (check-input-unblocking)
                 (bytes-copy! bstr end src-bstr src-start (fx+ src-start amt))
                 (set! end (fx+ end amt))
                 (check-output-blocking)
                 (fast-mode! amt)
                 amt])]
             [else
              (maybe-grow)]))))]

    [get-write-evt
     (get-write-evt-via-write-out (lambda (out v bstr start)
                                    (port-count! out v bstr start)))]

    [close
     ;; in atomic mode
     (lambda ()
       (with-object pipe-data d
         (when output
           (slow-mode!)
           (set! output #f)
           (when write-ready-sema
             (semaphore-post write-ready-sema))
           (when more-read-ready-sema
             (semaphore-post more-read-ready-sema))
           (semaphore-post read-ready-sema))))]))

;; ----------------------------------------
    
(define (make-pipe-ends [limit #f] [input-name 'pipe] [output-name 'pipe])
  (define len (min+1 limit 16))
  (define read-ready-sema (make-semaphore))
  (define write-ready-sema (and limit (make-semaphore 1)))
  (define write-ready-evt (if limit
                              (semaphore-peek-evt write-ready-sema)
                              always-evt))
  (define d (new pipe-data
                 [bstr (make-bytes len)]
                 [len len]
                 [limit limit]
                 [read-ready-sema read-ready-sema]
                 [write-ready-sema write-ready-sema]
                 [read-ready-evt (wrap-evt (semaphore-peek-evt read-ready-sema)
                                           (lambda (v) 0))]
                 [write-ready-evt write-ready-evt]))

  (define input (new pipe-input-port
                     [name input-name]
                     [d d]))
  (define output (new pipe-output-port
                      [name output-name]
                      [evt write-ready-evt]
                      [d d]))

  (set-pipe-data-input! d input)
  (set-pipe-data-output! d output)

  (values input output))

(define/who (make-pipe [limit #f] [input-name 'pipe] [output-name 'pipe])
  (check who #:or-false exact-positive-integer? limit)
  (define-values (ip op) (make-pipe-ends limit input-name output-name))
  (when (port-count-lines-enabled)
    (port-count-lines! ip)
    (port-count-lines! op))
  (values ip op))
