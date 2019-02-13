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
   (with-object pipe-data d
     (sync-both)
     (content-length))))

;; ----------------------------------------

(class pipe-data
  #:field
  [bstr #""]
  [len 0]
  [limit 0]
  [peeked-amt 0] ; peeked but not yet read, effectively extends `limit`
  [start 0]
  [end 0]
  [input-ref #f]     ; #f => closed
  [output-ref #f]    ; #f => closed
  [input-buffer #f]
  [output-buffer #f]
  [read-ready-sema #f]
  [write-ready-sema #f]
  [more-read-ready-sema #f] ; for lookahead peeks
  [read-ready-evt #f]
  [write-ready-evt #f]

  ;; in atomic mode for all static methods 

  #:static
  ;; sync local fields with input buffer without implying slow mode
  [sync-input
   (lambda ()
     (define b input-buffer)
     (when (direct-bstr b)
       (define pos (direct-pos b))
       (set! start (if (fx= pos len)
                       0
                       pos))))]
  ;; sync local fields with output buffer without implying slow mode
  [sync-output
   (lambda ()
     (define b output-buffer)
     (when (direct-bstr b)
       (define pos (direct-pos b))
       (set! end (if (fx= pos len)
                     0
                     pos))))]
  
  [sync-both
   (lambda ()
     (sync-input)
     (sync-output))]

  ;; assumes sync'ed
  [content-length
   (lambda ()
     (define s start)
     (define e end)
     (if (s . fx<= . e)
         (fx- e s)
         (fx+ e (fx- len s))))]

  ;; assumes sync'ed
  [input-empty?
   (lambda ()
     (fx= start end))]

  ;; assumes sync'ed
  [output-full?
   (lambda ()
     (define l limit)
     (and l
          ((content-length) . >= . (+ l peeked-amt))))]

  ;; Used before read:
  [check-output-unblocking
   (lambda ()
     (when write-ready-sema
       (semaphore-post write-ready-sema)
       (set! write-ready-sema #f)))]

  ;; Used before write:
  [check-input-unblocking
   (lambda ()
     (when read-ready-sema
       (semaphore-post read-ready-sema)
       (set! read-ready-sema #f))
     (when more-read-ready-sema
       (semaphore-post more-read-ready-sema)
       (set! more-read-ready-sema #f)))]

  ;; Used after peeking:
  [peeked!
   (lambda (amt)
     (when (amt . > . peeked-amt)
       (check-output-unblocking)
       (set! peeked-amt amt)))])

(define (make-ref v) (make-weak-box v))
(define (ref-value r) (weak-box-value r))

;; ----------------------------------------

(class pipe-input-port #:extends commit-input-port
  #:field
  [d #f] ; pipe-data
  
  #:private
  [fast-mode!
   (lambda (amt) ; amt = not yet added to `offset`
     (with-object pipe-data d
       (define s start)
       (define e end)
       (unless (fx= s e)
         (define b buffer)
         (set-direct-bstr! b bstr)
         (set-direct-pos! b s)
         (set-direct-end! b (if (s . fx< . e) e len))
         (define o offset)
         (when o
           (set! offset (- (+ o amt) s))))))]

  [slow-mode!
   (lambda ()
     (with-object pipe-data d
       (define b buffer)
       (when (direct-bstr b)
         (define pos (direct-pos b))
         (define o offset)
         (when o
           (set! offset (+ o pos)))
         (set! start (if (fx= pos len) 0 pos))
         (set-direct-bstr! b #f)
         (set-direct-pos! b (direct-end b)))
       (sync-output)))]

  #:static
  [on-resize
   (lambda ()
     (slow-mode!))]
  [on-output-full
   (lambda ()
     (slow-mode!))]

  #:override
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
          (if output-ref
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
          (progress!)
          (fast-mode! amt)
          amt])))]

  [peek-in
   (lambda (dest-bstr dest-start dest-end skip progress-evt copy?)
     (with-object pipe-data d
       (assert-atomic)
       (sync-both)
       (define content-amt (content-length))
       (cond
         [(and progress-evt
               (sync/timeout 0 progress-evt))
          #f]
         [(content-amt . <= . skip)
          (cond
            [(not output-ref) eof]
            [else
             (unless (or (zero? skip) more-read-ready-sema)
               (set! more-read-ready-sema (make-semaphore))
               (define out (ref-value output-ref))
               (when out
                 (send pipe-output-port out on-need-more-ready)))
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
       (or (not output-ref)
           (begin
             (sync-both)
             (not (fx= 0 (content-length)))))))]

  [close
   (lambda ()
     (with-object pipe-data d
       (when input-ref
         (slow-mode!)
         (set! input-ref #f)
         (progress!))))]

  [get-progress-evt
   (lambda ()
     (atomically
      (with-object pipe-data d
        (cond
          [(not input-ref) always-evt]
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
                  (finish dest-bstr)])))))]))]

  [count-lines!
   (lambda ()
     (slow-mode!))])

;; ----------------------------------------

(class pipe-output-port #:extends core-output-port
  #:field
  [d d] ; pipe-data

  #:private
  [fast-mode!
   (lambda (amt) ; amt = not yet added to `offset`
     (with-object pipe-data d
       (define lim limit)
       (define avail (and lim (- lim (content-length))))
       (when (or (not avail) (avail . <= . 0))
         (define s start)
         (define e end)
         (define b buffer)
         (set-direct-bstr! b bstr)
         (set-direct-pos! b e)
         (set-direct-end! b (let ([end (if (s . fx<= . e)
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
       (define b buffer)
       (when (direct-bstr b)
         (define pos (direct-pos b))
         (define o offset)
         (when o
           (set! offset (+ o pos)))
         (set! end (if (fx= pos len) 0 pos))
         (set-direct-bstr! b #f)
         (set-direct-pos! b (direct-end b)))
       (sync-input)))]

  #:static
  [on-input-empty
   (lambda ()
     (slow-mode!))]
  [on-need-more-ready
   (lambda ()
     (slow-mode!))]

  #:override
  [write-out
   ;; in atomic mode
   (lambda (src-bstr src-start src-end nonblock? enable-break? copy?)
     (assert-atomic)
     (slow-mode!)
     (with-object pipe-data d
       (let try-again ()
         (define top-pos (if (fx= start 0)
                             (fx- len 1)
                             len))
         (define (maybe-grow)
           (cond
             [(or (not limit)
                  ((+ limit peeked-amt) . > . (fx- len 1)))
              ;; grow pipe size
              (define in (ref-value input-ref))
              (when in
                (send pipe-input-port in on-resize))
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
       (when output-ref
         (slow-mode!)
         (set! output-ref #f)
         (check-input-unblocking))))])

;; ----------------------------------------

(define (make-pipe-ends [limit #f] [input-name 'pipe] [output-name 'pipe])
  (define len (min+1 limit 16))

  (define d (new pipe-data
                 #:field
                 [bstr (make-bytes len)]
                 [len len]
                 [limit limit]))

  (define write-ready-evt (if limit
                              (pipe-write-poller d)
                              always-evt))
  (define read-ready-evt (pipe-read-poller d))

  (define input (new pipe-input-port
                     #:field
                     [name input-name]
                     [d d]))
  (define output (new pipe-output-port
                      #:field
                      [name output-name]
                      [evt write-ready-evt]
                      [d d]))

  (set-pipe-data-input-buffer! d (core-port-buffer input))
  (set-pipe-data-output-buffer! d (core-port-buffer output))
  (set-pipe-data-input-ref! d (make-ref input))
  (set-pipe-data-output-ref! d (make-ref output))
  (set-pipe-data-write-ready-evt! d write-ready-evt)
  (set-pipe-data-read-ready-evt! d read-ready-evt)

  (values input output))

(define/who (make-pipe [limit #f] [input-name 'pipe] [output-name 'pipe])
  (check who #:or-false exact-positive-integer? limit)
  (define-values (ip op) (make-pipe-ends limit input-name output-name))
  (values (finish-port/count ip)
          (finish-port/count op)))

;; ----------------------------------------

;; Note: a thread blocked on writing to a limited pipe cannot be GCed
;; due to the use of `replace-evt`.
(struct pipe-write-poller (d)
  #:property
  prop:evt
  (poller
   (lambda (pwp ctx)
     (with-object pipe-data (pipe-write-poller-d pwp)
       (sync-both)
       (cond
         [(not (output-full?))
          (values (list pwp) #f)]
         [else
          (unless write-ready-sema
            (set! write-ready-sema (make-semaphore)))
          (define in (ref-value input-ref))
          (when in
            (send pipe-input-port in on-output-full))
          (values #f (replace-evt (semaphore-peek-evt write-ready-sema)
                                  (lambda (v) pwp)))])))))

(struct pipe-read-poller (d)
  #:property
  prop:evt
  (poller
   (lambda (prp ctx)
     (with-object pipe-data (pipe-read-poller-d prp)
       (sync-both)
       (cond
         [(not (input-empty?))
          (values (list 0) #f)]
         [else
          (unless read-ready-sema
            (set! read-ready-sema (make-semaphore)))
          (define out (ref-value output-ref))
          (when out
            (send pipe-output-port out on-input-empty))
          (values #f (wrap-evt (semaphore-peek-evt read-ready-sema)
                               (lambda (v) 0)))])))))
