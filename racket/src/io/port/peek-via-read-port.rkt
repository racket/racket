#lang racket/base
(require racket/fixnum
         "../common/class.rkt"
         "../host/thread.rkt"
         "port.rkt"
         "input-port.rkt"
         "output-port.rkt"
         "pipe.rkt"
         "commit-port.rkt")

(provide peek-via-read-input-port
         open-input-peek-via-read)

(class peek-via-read-input-port #:extends commit-input-port
  (field
   [bstr (make-bytes 4096)]
   [pos 0]
   [end-pos 0]
   [peeked-eof? #f]
   [buffer-mode 'block])

  (public
    ;; in atomic mode; must override
    [read-in/inner
     (lambda (dest-bstr start end copy?)
       0)])

  (static
   ;; in atomic mode
   [purge-buffer
    (lambda ()
      (slow-mode!)
      (set! pos 0)
      (set! end-pos 0)
      (set! peeked-eof? #f))]

   [close-peek-buffer
    (lambda ()
      (purge-buffer)
      (set! bstr #""))]

   [buffer-adjust-pos
    (lambda (i)
      (- i (fx- end-pos (if buffer buffer-pos pos))))])

  (private
    ;; in atomic mode
    [pull-some-bytes
     (lambda ([amt (if (eq? 'block buffer-mode) (bytes-length bstr) 1)] [offset 0] [init-pos 0])
       (define get-end (min (+ amt offset) (bytes-length bstr)))
       (define v (send peek-via-read-input-port this read-in/inner bstr offset get-end #f))
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

    ;; in atomic mode
    [pull-more-bytes
     (lambda (amt)
       (cond
         [(end-pos . fx< . (bytes-length bstr))
          ;; add to end of buffer
          (pull-some-bytes amt end-pos pos)]
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

    ;; in atomic mode
    [retry-pull?
     (lambda (v)
       (and (integer? v) (not (eqv? v 0))))]

    ;; in atomic mode
    [fast-mode!
     (lambda (amt) ; amt = not yet added to `count`
       (unless count
         (set! buffer bstr)
         (define s pos)
         (set! buffer-pos s)
         (set! buffer-end end-pos)
         (define o offset)
         (when o
           (set! offset (- (+ o amt) s)))))]

    ;; in atomic mode
    [slow-mode!
     (lambda ()
       (when buffer
         (define s buffer-pos)
         (define o offset)
         (when o
           (set! offset (+ o s)))
         (set! pos s)
         (set! buffer #f)
         (set! buffer-pos buffer-end)))])

  (override
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
               (define v (send peek-via-read-input-port this read-in/inner dest-bstr start end copy?))
               (unless (eqv? v 0)
                 (progress!))
               v])])))]

    ;; in atomic mode
    [peek-in
     (lambda (dest-bstr start end skip progress-evt copy?)
       (let try-again ()
         (cond
           [(and progress-evt
                 (sync/timeout 0 progress-evt))
            #f]
           [else
            (define s (if buffer buffer-pos pos))
            (define peeked-amt (fx- end-pos s))
            (cond
              [(peeked-amt . > . skip)
               (define amt (min (fx- peeked-amt skip) (fx- end start)))
               (define s-pos (fx+ s skip))
               (bytes-copy! dest-bstr start bstr s-pos (fx+ s-pos amt))
               amt]
              [peeked-eof?
               eof]
              [else
               (slow-mode!)
               (define v (pull-more-bytes (+ (- skip peeked-amt) (fx- end start))))
               (if (retry-pull? v)
                   (try-again)
                   v)])])))]

    ;; in atomic mode
    [byte-ready
     (lambda (work-done!)
       (let loop ()
         (define peeked-amt (fx- end-pos (if buffer buffer-pos pos)))
         (cond
           [(peeked-amt . fx> . 0) #t]
           [peeked-eof? #t]
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
       (atomically
        (slow-mode!)
        (make-progress-evt)))]

    ;; in atomic mode
    [commit
     (lambda (amt progress-evt ext-evt finish)
       (slow-mode!)
       (wait-commit
        progress-evt ext-evt
        ;; in atomic mode, maybe in a different thread:
        (lambda ()
          (let ([amt (fxmin amt (fx- end-pos pos))])
            (cond
              [(fx= 0 amt)
               (finish #"")]
              [else
               (define dest-bstr (make-bytes amt))
               (bytes-copy! dest-bstr 0 bstr pos (fx+ pos amt))
               (set! pos (fx+ pos amt))
               (progress!)
               (finish dest-bstr)])))))]

    ;; in atomic mode
    [buffer-mode
     (case-lambda
       [(self) buffer-mode]
       [(self mode) (set! buffer-mode mode)])]

    ;; in atomic mode
    [close
     (lambda ()
       (close-peek-buffer))]))

;; ----------------------------------------

(define (open-input-peek-via-read #:name name
                                  #:self next-self
                                  #:data [data #f]
                                  #:read-in read-in
                                  #:read-is-atomic? [read-is-atomic? #f] ; => can implement progress evts
                                  #:close close
                                  #:get-location [get-location #f]
                                  #:count-lines! [count-lines! #f]
                                  #:init-offset [init-offset 0]
                                  #:file-position [file-position #f]
                                  #:alt-buffer-mode [alt-buffer-mode #f])
  (define-values (peek-pipe-i peek-pipe-o) (make-pipe))
  (define peeked-eof? #f)
  (define buf (make-bytes 4096))
  (define buffer-mode 'block)

  ;; in atomic mode
  (define (prepare-change self)
    (send core-input-port peek-pipe-i prepare-change))

  ;; in atomic mode
  (define (pull-some-bytes [amt (if (eq? 'block buffer-mode) (bytes-length buf) 1)] #:keep-eof? [keep-eof? #t])
    (define v (read-in next-self buf 0 amt #f))
    (cond
      [(eof-object? v)
       (when keep-eof?
         (set! peeked-eof? #t))
       eof]
      [(evt? v) v]
      [(eqv? v 0) 0]
      [else
       (let loop ([wrote 0])
         (define just-wrote (send core-output-port peek-pipe-o write-out buf wrote v #t #f #f))
         (define next-wrote (+ wrote just-wrote))
         (unless (= v next-wrote)
           (loop next-wrote)))
       v]))

  (define (retry-pull? v)
    (and (integer? v) (not (eqv? v 0))))

  ;; in atomic mode
  (define (do-read-in self dest-bstr start end copy?)
    (let try-again ()
      (cond
        [(positive? (pipe-content-length peek-pipe-i))
         (send core-input-port peek-pipe-i read-in dest-bstr start end copy?)]
        [peeked-eof?
         (set! peeked-eof? #f)
         ;; an EOF doesn't count as progress
         eof]
        [else
         (cond
           [(and (< (- end start) (bytes-length buf))
                 (eq? 'block buffer-mode))
            (define v (pull-some-bytes))
            (cond
              [(or (eqv? v 0) (evt? v)) v]
              [else (try-again)])]
           [else
            (define v (read-in next-self dest-bstr start end copy?))
            (unless (eq? v 0)
              (progress!))
            v])])))

  ;; in atomic mode
  (define (do-peek-in self dest-bstr start end skip progress-evt copy?)
    (let try-again ()
      (define peeked-amt (if peek-pipe-i
                             (pipe-content-length peek-pipe-i)
                             0))
      (cond
        [(and progress-evt
              (sync/timeout 0 progress-evt))
         #f]
        [(and peek-pipe-i
              (peeked-amt . > . skip))
         (send core-input-port peek-pipe-i peek-in dest-bstr start end skip progress-evt copy?)]
        [peeked-eof?
         eof]
        [else
         (define v (pull-some-bytes))
         (if (retry-pull? v)
             (try-again)
             v)])))

  ;; in atomic mode
  (define (do-byte-ready self work-done!)
    (cond
      [(positive? (pipe-content-length peek-pipe-i))
       #t]
      [peeked-eof?
       #t]
      [else
       (define v (pull-some-bytes))
       (work-done!)
       (cond
         [(retry-pull? v)
          (do-byte-ready self void)]
         [(evt? v) v]
         [else
          (not (eqv? v 0))])]))

  ;; in atomic mode
  (define (purge-buffer)
    (set!-values (peek-pipe-i peek-pipe-o) (make-pipe))
    (set! peeked-eof? #f))

  ;; in atomic mode
  (define (get-progress-evt self)
    (send core-input-port peek-pipe-i get-progress-evt))

  ;; in atomic mode
  (define (progress!)
    ;; Relies on support for `0 #f #f` arguments in pipe implementation:
    (send core-input-port peek-pipe-i commit 0 #f #f void))

  (define (commit self amt evt ext-evt finish)
    (send core-input-port peek-pipe-i commit amt evt ext-evt finish))

  (define do-buffer-mode
    (case-lambda
      [(self) buffer-mode]
      [(self mode) (set! buffer-mode mode)]))

  (values (make-core-input-port
           #:name name
           #:data data
           #:self #f

           #:prepare-change prepare-change
           
           #:read-in do-read-in
           #:peek-in do-peek-in
           #:byte-ready do-byte-ready

           #:get-progress-evt (and read-is-atomic?
                                   get-progress-evt)
           #:commit commit

           #:close (lambda (self)
                     (close next-self)
                     (purge-buffer))

           #:get-location (and get-location
                               (lambda (self) (get-location next-self)))
           #:count-lines! (and count-lines!
                               (lambda (self) (count-lines! next-self)))
           #:init-offset init-offset
           #:file-position (and file-position
                                (case-lambda
                                  [(self) (file-position next-self)]
                                  [(self pos) (file-position next-self pos)]))
           #:buffer-mode (or (and alt-buffer-mode
                                  (case-lambda
                                    [(self) (alt-buffer-mode next-self)]
                                    [(self mode) (alt-buffer-mode next-self mode)]))
                             do-buffer-mode))

          ;; in atomic mode:
          (case-lambda
            [() (purge-buffer)]
            [(pos) (- pos (pipe-content-length peek-pipe-i))])))
