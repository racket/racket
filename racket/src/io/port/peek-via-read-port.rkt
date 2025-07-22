#lang racket/base
(require racket/fixnum
         "../common/class.rkt"
         "../host/thread.rkt"
         "port.rkt"
         "input-port.rkt"
         "output-port.rkt"
         "lock.rkt"
         "pipe.rkt"
         "commit-port.rkt")

(provide peek-via-read-input-port)

(class peek-via-read-input-port #:extends commit-input-port
  #:field
  [bstr (make-bytes 4096)]
  [pos 0]
  [end-pos 0]
  [peeked-eof? #f]
  [buffer-mode 'block]

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
        v]))]

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
     (and (integer? v) (not (eqv? v 0))))]

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

  ;; with lock hald
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
  ;; in atomic mode
  [prepare-change
   (lambda ()
     (pause-waiting-commit))]
  
  ;; in atomic mode
  [read-in
   (lambda (dest-bstr start end copy?)
     (slow-mode!)
     (let try-again ()
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
               [else (try-again)])]
            [else
             (define v (send peek-via-read-input-port this read-in/inner dest-bstr start end copy? #f))
             (unless (eqv? v 0)
               (progress!))
             v])])))]

  ;; with lock held
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
             (if (retry-pull? v)
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
            [(retry-pull? v)
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
