#lang racket/base
(require racket/fixnum
         "../common/class.rkt"
         "../host/thread.rkt"
         "port.rkt"
         "input-port.rkt"
         "output-port.rkt"
         "lock.rkt"
         "commit-port.rkt")

(provide peek-via-read-input-port)

(class peek-via-read-input-port #:extends commit-input-port
  #:field
  [bstr (make-bytes 4096)]
  [pos 0]
  [end-pos 0]
  [peeked-eof? #f]
  [buffer-mode 'block]
  ;; When `read-inner-may-leave-lock?`, the buffer can change while
  ;; `read-in/inner` runs. `buffer-generation` counts changes that
  ;; add bytes or an EOF to the buffer or close the buffer, so a 0 or
  ;; evt result can be detected as stale. A thread that waits on an
  ;; evt result also needs to wake up on a later change, since the
  ;; change can mean that the evt never becomes ready; such waiters
  ;; use `buffer-change-sema`, and while it exists, the port lock
  ;; requires atomic mode so that the semaphore can be posted.
  [buffer-generation 0]
  [buffer-change-sema #f]

  #:public
  ;; with lock held; must override
  [read-in/inner
   (lambda (dest-bstr start end copy? to-buffer?)
     0)]

  ;; with lock held
  ;; called when no peeked bytes are available;
  ;; return 'not-ready as #f, while #f means implement by peeking,
  ;; and other possibilities are #t or evt 
  [byte-ready/inner
   (lambda (work-done!) #f)]

  ;; (-> boolean?)
  [read-inner-may-leave-lock? (lambda () #f)]

  ;; with lock held
  ;; (-> boolean?)
  ;; Reports whether the port lock must continue to require atomic
  ;; mode for a reason specific to the subclass
  [lock-must-stay-atomic? (lambda () #f)]

  #:static
  ;; with lock held
  [purge-buffer
   (lambda ()
     (slow-mode!)
     (set! pos 0)
     (set! end-pos 0)
     (set! peeked-eof? #f))]

  ;; with lock held
  [close-peek-buffer
   (lambda ()
     (purge-buffer)
     (set! bstr #"")
     (buffer-changed!)
     (progress!))]

  ;; with lock held
  [buffer-adjust-pos
   (lambda (i is-converted) ; is-converted reports on CRLF conversions in the buffer
     (define b buffer)
     (define start-pos (if (direct-bstr b) (direct-pos b) pos))
     (define r (- i (fx- end-pos start-pos)))
     (cond
       [is-converted (let loop ([pos start-pos] [r r])
                       (if (fx= pos end-pos)
                           r
                           (loop (fx+ pos 1)
                                 (if (eqv? 0 (bytes-ref is-converted pos))
                                     r
                                     (- r 1)))))]
       [else r]))]

  ;; with lock held
  [default-buffer-mode
    (case-lambda
      [() buffer-mode]
      [(mode) (set! buffer-mode mode)])]

  #:private
  ;; with lock held
  [pull-some-bytes
   (lambda ([amt (if (eq? 'block buffer-mode) (bytes-length bstr) 1)] [offset 0] [init-pos 0])
     (define get-end (min (+ amt offset) (bytes-length bstr)))
     (cond
       [(send peek-via-read-input-port this read-inner-may-leave-lock?)
        ;; Another thread can read, peek, commit, or close while the
        ;; lock is released, so `bstr`, `pos`, and `end-pos` may all
        ;; change. Pull into a fresh byte string, and then append to
        ;; the buffer as it exists after the lock is reacquired.
        (define fresh-bstr (make-bytes (fx- get-end offset)))
        (define generation buffer-generation)
        (define v (send peek-via-read-input-port this read-in/inner fresh-bstr 0 (bytes-length fresh-bstr) #f #t))
        (cond
          [(eof-object? v)
           (pulled-eof!)
           eof]
          [(or (evt? v) (eqv? v 0))
           (retry-on-buffer-change
            generation v
            ;; If an evt eventually produces a byte count, the bytes
            ;; are in `fresh-bstr`, so move them to the buffer; for a
            ;; count or EOF, report 0 to make the caller look again
            (lambda (r)
              (cond
                [(exact-positive-integer? r)
                 (with-lock this
                   (append-pulled-bytes! fresh-bstr r))
                 0]
                [(eof-object? r)
                 (with-lock this
                   (pulled-eof!))
                 0]
                [else r])))]
          [else
           (append-pulled-bytes! fresh-bstr v)
           (if closed? 0 v)])]
       [else
        ;; lock is kept, so `bstr`, `pos`, and `end-pos` stay as-is
        (define v (send peek-via-read-input-port this read-in/inner bstr offset get-end #f #t))
        (cond
          [(eof-object? v)
           (set! peeked-eof? #t)
           eof]
          [(evt? v) v]
          [(eqv? v 0) 0]
          [else
           (set! pos init-pos)
           (set! end-pos (fx+ offset v))
           v])]))]

  ;; with lock held; adds `n` bytes from `src-bstr` after any
  ;; currently buffered bytes, shifting or growing the buffer as
  ;; needed; drops the bytes if the port has been closed
  [append-pulled-bytes!
   (lambda (src-bstr n)
     (unless closed?
       ;; another thread may have switched to fast mode
       (slow-mode!)
       (define len (bytes-length bstr))
       (unless ((fx+ end-pos n) . fx<= . len)
         (define keep (fx- end-pos pos))
         (define new-bstr (if ((fx+ keep n) . fx<= . len)
                              bstr
                              (make-bytes (fxmax (fx* 2 len) (fx+ keep n)))))
         (bytes-copy! new-bstr 0 bstr pos end-pos)
         (set! bstr new-bstr)
         (set! pos 0)
         (set! end-pos keep))
       (bytes-copy! bstr end-pos src-bstr 0 n)
       (set! end-pos (fx+ end-pos n))
       (buffer-changed!)))]

  ;; with lock held; records an EOF from a pull that may have left
  ;; the lock, unless the port has been closed
  [pulled-eof!
   (lambda ()
     (unless closed?
       (set! peeked-eof? #t)
       (buffer-changed!)))]

  ;; with lock held, and in atomic mode if `buffer-change-sema`
  [buffer-changed!
   (lambda ()
     (set! buffer-generation (add1 buffer-generation))
     (define s buffer-change-sema)
     (when s
       (set! buffer-change-sema #f)
       (semaphore-post s)
       (unless progress-sema
         (send commit-input-port this no-more-atomic-for-progress))))]

  ;; with lock held, where `v` is a 0 or evt result from a
  ;; `read-in/inner` that may have left the lock, and `generation`
  ;; is the value of `buffer-generation` before the call. If the
  ;; buffer changed meanwhile, the result is 'retry. Otherwise, an evt
  ;; is adjusted to also become ready with 0 on a later buffer
  ;; change; `on-result` is applied to a non-evt result of the evt
  [retry-on-buffer-change
   (lambda (generation v [on-result values])
     (cond
       [(not (eqv? generation buffer-generation)) 'retry]
       [(evt? v)
        (define s (or buffer-change-sema
                      (let ([s (make-semaphore)])
                        (set! buffer-change-sema s)
                        (port-lock-require-atomic! this #t)
                        s)))
        (define changed-evt (wrap-evt (semaphore-peek-evt s) (lambda (r) 0)))
        ;; an evt's result can be another evt, which needs the same treatment
        (let loop ([v v])
          (choice-evt (list (wrap-evt v (lambda (r)
                                          (if (evt? r)
                                              (loop r)
                                              (on-result r))))
                            changed-evt)))]
       [else v]))]

  ;; with lock held
  [pull-more-bytes
   (lambda (amt)
     (cond
       [(end-pos . fx< . (bytes-length bstr))
        ;; add to end of buffer
        (define pull-amt (if (eq? 'block buffer-mode)
                             (fx- (bytes-length bstr) end-pos)
                             amt))
        (pull-some-bytes pull-amt end-pos pos)]
       [(fx= pos 0)
        ;; extend buffer
        (define new-bstr (make-bytes (fx* 2 (bytes-length bstr))))
        (bytes-copy! new-bstr 0 bstr 0 end-pos)
        (set! bstr new-bstr)
        (pull-some-bytes amt end-pos)]
       [else
        ;; shift to start of buffer and retry
        (bytes-copy! bstr 0 bstr pos end-pos)
        (set! end-pos (fx- end-pos pos))
        (set! pos 0)
        (pull-more-bytes amt)]))]

  ;; with lock held
  [retry-pull?
   (lambda (v)
     (or (eq? v 'retry)
         (and (integer? v) (not (eqv? v 0)))))]

  ;; with lock held
  [fast-mode!
   (lambda (amt) ; amt = not yet added to `offset`
     (define b buffer)
     (set-direct-bstr! b bstr)
     (define s pos)
     (set-direct-pos! b s)
     (set-direct-end! b end-pos)
     (define o offset)
     (when o
       (set! offset (- (+ o amt) s))))]

  ;; with lock held
  [slow-mode!
   (lambda ()
     (define b buffer)
     (when (direct-bstr b)
       (define s (direct-pos b))
       (define o offset)
       (when o
         (set! offset (+ o s)))
       (set! pos s)
       (set-direct-bstr! b #f)
       (set-direct-pos! b (direct-end b))))]

  #:override
  ;; with lock held
  [no-more-atomic-for-progress
   (lambda ()
     (unless (or closed-sema
                 buffer-change-sema
                 (send peek-via-read-input-port this lock-must-stay-atomic?))
       (port-lock-require-atomic! this #f)))]

  ;; in atomic mode
  [prepare-change
   (lambda ()
     (pause-waiting-commit))]
  
  ;; with lock held
  [read-in
   (lambda (dest-bstr start end copy?)
     (let try-again ()
       (slow-mode!)
       (cond
         [(pos . fx< . end-pos)
          (define amt (min (fx- end-pos pos) (fx- end start)))
          (bytes-copy! dest-bstr start bstr pos (fx+ pos amt))
          (set! pos (fx+ pos amt))
          (progress!)
          (fast-mode! amt)
          amt]
         [peeked-eof?
          (set! peeked-eof? #f)
          ;; an EOF doesn't count as progress
          eof]
         [else
          (cond
            [(and (eq? 'block buffer-mode)
                  (fx< (fx- end start) (fxrshift (bytes-length bstr) 1)))
             (define v (pull-some-bytes))
             (cond
               [(or (eqv? v 0) (evt? v)) v]
               [else (try-again)])] ; includes 'retry
            [else
             (define generation buffer-generation)
             (define v (send peek-via-read-input-port this read-in/inner dest-bstr start end copy? #f))
             (define r (if (and (or (eqv? v 0) (evt? v))
                                (send peek-via-read-input-port this read-inner-may-leave-lock?))
                           (retry-on-buffer-change generation v)
                           v))
             (cond
               [(eq? r 'retry) (try-again)]
               [else
                (unless (eqv? r 0)
                  (progress!))
                r])])])))]

  ;; with lock held, and in atomic mode if `progress-evt`
  [peek-in
   (lambda (dest-bstr start end skip progress-evt copy?)
     (let try-again ()
       (cond
         [(and progress-evt
               (sync/timeout 0 progress-evt))
          #f]
         [else
          (define b buffer)
          (define s (if (direct-bstr b) (direct-pos b) pos))
          (define peeked-amt (fx- end-pos s))
          (cond
            [(peeked-amt . > . skip)
             (define amt (min (fx- peeked-amt skip) (fx- end start)))
             (define s-pos (fx+ s skip))
             (bytes-copy! dest-bstr start bstr s-pos (fx+ s-pos amt))
             (unless progress-sema
               (fast-mode! 0))
             amt]
            [peeked-eof?
             eof]
            [else
             (slow-mode!)
             (define v (pull-more-bytes (+ (- skip peeked-amt) (fx- end start))))
             (if (retry-pull? v) ; includes 'retry
                 (try-again)
                 v)])])))]

  ;; with lock held
  [byte-ready
   (lambda (work-done!)
     (let loop ()
       (define b buffer)
       (define peeked-amt (fx- end-pos (if (direct-bstr b) (direct-pos b) pos)))
       (cond
         [(peeked-amt . fx> . 0) #t]
         [peeked-eof? #t]
         [(send peek-via-read-input-port this byte-ready/inner work-done!)
          => (lambda (status) (and (not (eq? status 'not-ready))
                                   status))]
         [else
          (slow-mode!)
          (define v (pull-some-bytes))
          (work-done!)
          (cond
            [(retry-pull? v) ; includes 'retry
             (loop)]
            [(evt? v) v]
            [else
             (not (eqv? v 0))])])))]

  [get-progress-evt
   (lambda ()
     (with-lock this
       (slow-mode!)
       (make-progress-evt)))]

  ;; with lock held, which will imply atomic mode (due to progress-evt)
  [commit
   (lambda (amt progress-evt ext-evt finish)
     (slow-mode!)
     (wait-commit
      progress-evt ext-evt
      ;; in atomic mode, maybe in a different thread;
      ;; since we have progress-evt, then the lock must
      ;; requrire atomic mode
      (lambda ()
        (with-lock this
          (let ([amt (fxmin amt (fx- end-pos pos))])
            (cond
              [(fx= 0 amt)
               (finish #"")]
              [else
               (define dest-bstr (make-bytes amt))
               (bytes-copy! dest-bstr 0 bstr pos (fx+ pos amt))
               (set! pos (fx+ pos amt))
               (progress!)
               (finish dest-bstr)]))))))]

  ;; with lock held
  [buffer-mode
   (case-lambda
     [() (default-buffer-mode)]
     [(mode) (default-buffer-mode mode)])]

  ;; with lock held
  [close
   (lambda ()
     (close-peek-buffer))])
