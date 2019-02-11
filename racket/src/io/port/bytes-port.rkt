#lang racket/base
(require racket/fixnum
         "../common/check.rkt"
         "../common/fixnum.rkt"
         "../common/class.rkt"
         "../host/thread.rkt"
         "port.rkt"
         "input-port.rkt"
         "output-port.rkt"
         "pipe.rkt"
         "bytes-input.rkt"
         "count.rkt"
         "commit-manager.rkt")

(provide open-input-bytes
         open-output-bytes
         get-output-bytes
         string-port?)

(define/who (open-input-bytes bstr [name 'string])
  (check who bytes? bstr)
  (define p (make-input-bytes (bytes->immutable-bytes bstr) name))
  (when (port-count-lines-enabled)
    (port-count-lines! p))
  p)

(class bytes-input-port #:extends core-input-port
  (field
   [progress-sema #f]
   [commit-manager #f]
   [bstr #f] ; normally installed as buffer
   [pos 0]   ; used when bstr is not installed as buffer
   [alt-pos #f])

  (private
    ;; in atomic mode
    [progress!
     (lambda ()
       (when progress-sema
         (semaphore-post progress-sema)
         (set! progress-sema #f)))]

    ;; in atomic mode [can leave atomic mode temporarily]
    ;; After this function returns, complete any commit-changing work
    ;; before leaving atomic mode again.
    [pause-waiting-commit
     (lambda ()
       (when commit-manager
         (commit-manager-pause commit-manager)))]

    ;; in atomic mode [can leave atomic mode temporarily]
    [wait-commit
     (lambda (progress-evt ext-evt finish)
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
          (commit-manager-wait commit-manager progress-evt ext-evt finish)]))]

    ;; in atomic mode
    [in-buffer-pos
     (lambda ()
       (if buffer
           buffer-pos
           pos))])

  (override
    [close
     (lambda ()
       (set! commit-manager #f) ; to indicate closed
       (progress!)
       (set! bstr #f)
       (when buffer
         (set! offset buffer-pos)
         (set! buffer #f)))]
    [file-position
     (case-lambda
       [() (or alt-pos (in-buffer-pos))]
       [(given-pos)
        (define len buffer-end)
        (define new-pos (if (eof-object? given-pos)
                            len
                            (min len given-pos)))
        (if buffer
            (set! buffer-pos new-pos)
            (set! pos new-pos))
        (set! alt-pos (and (not (eof-object? given-pos))
                           (given-pos . > . new-pos)
                           given-pos))])]

    [prepare-change
     (lambda ()
       (pause-waiting-commit))]

    [read-in
     (lambda (dest-bstr start end copy?)
       (define len buffer-end)
       (define i (in-buffer-pos))
       (cond
         [(i . < . len)
          (define amt (min (- end start) (fx- len i)))
          (define new-pos (fx+ i amt))
          (cond
            [(not count)
             ;; Keep/resume fast mode
             (set! buffer-pos new-pos)
             (set! offset 0)
             (set! buffer bstr)]
            [else
             (set! pos new-pos)])
          (bytes-copy! dest-bstr start bstr i new-pos)
          (progress!)
          amt]
         [else eof]))]

    [peek-in
     (lambda (dest-bstr start end skip progress-evt copy?)
       (define i (in-buffer-pos))
       (define len buffer-end)
       (define at-pos (+ i skip))
       (cond
         [(and progress-evt (sync/timeout 0 progress-evt))
          #f]
         [(at-pos . < . len)
          (define amt (min (- end start) (fx- len at-pos)))
          (bytes-copy! dest-bstr start bstr at-pos (fx+ at-pos amt))
          amt]
         [else eof]))]

    [byte-ready
     (lambda (work-done!)
       ((in-buffer-pos) . < . buffer-end))]
   
    [get-progress-evt
     (lambda ()
       (define new-sema
         (or progress-sema
             (let ([sema (make-semaphore)])
               (set! progress-sema sema)
               ;; set port to slow mode:
               (when buffer
                 (define i buffer-pos)
                 (set! pos i)
                 (set! offset i)
                 (set! buffer #f)
                 (set! buffer-pos buffer-end))
               sema)))
       (semaphore-peek-evt new-sema))]

    [commit
     (lambda (amt progress-evt ext-evt finish)
       (wait-commit
        progress-evt ext-evt
        ;; in atomic mode, maybe in a different thread:
        (lambda ()
          (define len buffer-end)
          (define i (in-buffer-pos))
          (let ([amt (min amt (- len i))])
            (define dest-bstr (make-bytes amt))
            (bytes-copy! dest-bstr 0 bstr i (+ i amt))
            (cond
              [(not count)
               ;; Keep/resume fast mode
               (set! buffer-pos (fx+ i amt))
               (set! buffer bstr)
               (set! offset 0)]
              [else
               (set! pos (fx+ i amt))])
            (progress!)
            (finish dest-bstr)))))]

    [count-lines!
     (lambda ()
       (when buffer
         (define i buffer-pos)
         (set! offset i)
         (set! pos i)
         (set! buffer #f)
         (set! buffer-pos buffer-end)))]))

(define (make-input-bytes bstr name)
  (new bytes-input-port
       [name name]
       [buffer bstr]
       [buffer-end (bytes-length bstr)]
       [bstr bstr]))

;; ----------------------------------------

(struct output-bytes-data (o get))

(define (open-output-bytes [name 'string])
  (define-values (i/none o) (make-pipe-ends #f name name #:need-input? #f))
  (define p
    (make-core-output-port
     #:name name
     #:data (output-bytes-data o (lambda (o bstr start-pos discard?)
                                   ;; in atomic mode
                                   (pipe-get-content o bstr start-pos)
                                   (when discard?
                                     (pipe-discard-all o))))
     #:self o
     #:evt o
     #:write-out
     (lambda (o src-bstr src-start src-end nonblock? enable-break? copy?)
       (send core-output-port o write-out src-bstr src-start src-end nonblock? enable-break? copy?))
     #:close
     (lambda (o) (send core-port o close))
     #:get-write-evt
     (and (method core-output-port o get-write-evt)
          (lambda (o orig-o bstr start-k end-k)
            (send core-output-port o get-write-evt bstr start-k end-k)))
     #:get-location
     (and (method core-port o get-location)
          (lambda (o) (send core-port o get-location)))
     #:count-lines!
     (and (method core-port o count-lines!)
          (lambda (o)
            (send core-port o count-lines!)))
     #:file-position
     (case-lambda
       [(o) (pipe-write-position o)]
       [(o new-pos)
        (define len (pipe-content-length o))
        (cond
          [(eof-object? new-pos)
           (pipe-write-position o len)]
          [(new-pos . > . len)
           (when (new-pos . >= . (expt 2 48))
             ;; implausibly large
             (end-atomic)
             (raise-arguments-error 'file-position
                                    "new position is too large"
                                    "port" o
                                    "position" new-pos))
           (pipe-write-position o len)
           (define amt (- new-pos len))
           (send core-output-port o write-out (make-bytes amt 0) 0 amt #f #f #f)
           (void)]
          [else
           (pipe-write-position o new-pos)])])))
  (when (port-count-lines-enabled)
    (port-count-lines! o)
    (port-count-lines! p))
  p)

(define/who (get-output-bytes o [reset? #f] [start-pos 0] [end-pos #f])
  (check who (lambda (v) (and (output-port? o) (string-port? o)))
         #:contract "(and/c output-port? string-port?)"
         o)
  (check who exact-nonnegative-integer? start-pos)
  (check who exact-nonnegative-integer? #:or-false end-pos)
  (let ([bstr-o (->core-output-port o)])
    (define o (output-bytes-data-o (core-port-data bstr-o)))
    (start-atomic)
    (define len (pipe-content-length o))
    (when (start-pos . > . len)
      (end-atomic)
      (raise-range-error who "port content" "starting " start-pos o 0 len #f))
    (when end-pos
      (unless (<= start-pos end-pos len)
        (end-atomic)
        (raise-range-error who "port content" "ending " end-pos o 0 len start-pos)))
    (define amt (- (min len (or end-pos len)) start-pos))
    (define bstr (make-bytes amt))
    ((output-bytes-data-get (core-port-data bstr-o)) o bstr start-pos reset?)
    (end-atomic)
    bstr))

;; ----------------------------------------

(define (string-port? p)
  (cond
    [(input-port? p)
     (let ([p (->core-input-port p)])
       (bytes-input-port? p))]
    [(output-port? p)
     (let ([p (->core-output-port p)])
       (output-bytes-data? (core-port-data p)))]
    [else
     (raise-argument-error 'string-port? "port?" p)]))
