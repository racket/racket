#lang racket/base
(require racket/fixnum
         "../common/check.rkt"
         "../common/class.rkt"
         "../host/thread.rkt"
         "port.rkt"
         "input-port.rkt"
         "output-port.rkt"
         "bytes-input.rkt"
         "count.rkt"
         "commit-port.rkt")

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

(class bytes-input-port #:extends commit-input-port
  #:field
  [bstr #f] ; normally installed as buffer
  [pos 0]   ; used when bstr is not installed as buffer
  [alt-pos #f]

  #:private
  ;; in atomic mode
  [in-buffer-pos
   (lambda ()
     (define b buffer)
     (if (direct-bstr b)
         (direct-pos b)
         pos))]

  #:override
  [close
   (lambda ()
     (set! commit-manager #f) ; to indicate closed
     (progress!)
     (set! bstr #f)
     (define b buffer)
     (when (direct-bstr b)
       (set! offset (direct-pos b))
       (set-direct-bstr! b #f)))]
  [file-position
   (case-lambda
     [() (or alt-pos (in-buffer-pos))]
     [(given-pos)
      (define b buffer)
      (define len (direct-end b))
      (define new-pos (if (eof-object? given-pos)
                          len
                          (min len given-pos)))
      (if (direct-bstr b)
          (set-direct-pos! b new-pos)
          (set! pos new-pos))
      (set! alt-pos (and (not (eof-object? given-pos))
                         (given-pos . > . new-pos)
                         given-pos))])]

  [prepare-change
   (lambda ()
     (pause-waiting-commit))]

  [read-in
   (lambda (dest-bstr start end copy?)
     (define b buffer)
     (define len (direct-end b))
     (define i (in-buffer-pos))
     (cond
       [(i . < . len)
        (define amt (min (- end start) (fx- len i)))
        (define new-pos (fx+ i amt))
        ;; Keep/resume fast mode
        (set-direct-pos! b new-pos)
        (set! offset 0)
        (set-direct-bstr! b bstr)
        (bytes-copy! dest-bstr start bstr i new-pos)
        (progress!)
        amt]
       [else eof]))]

  [peek-in
   (lambda (dest-bstr start end skip progress-evt copy?)
     (define b buffer)
     (define len (direct-end b))
     (define i (in-buffer-pos))
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
     ((in-buffer-pos) . < . (direct-end buffer)))]
  
  [get-progress-evt
   (lambda ()
     (atomically
      (unless progress-sema
        ;; set port to slow mode:
        (define b buffer)
        (when (direct-bstr b)
          (define i (direct-pos b))
          (set! pos i)
          (set! offset i)
          (set-direct-bstr! b #f)
          (set-direct-pos! b (direct-end b))))
      (make-progress-evt)))]

  [commit
   (lambda (amt progress-evt ext-evt finish)
     (wait-commit
      progress-evt ext-evt
      ;; in atomic mode, maybe in a different thread:
      (lambda ()
        (define b buffer)
        (define len (direct-end b))
        (define i (in-buffer-pos))
        (let ([amt (min amt (- len i))])
          (define dest-bstr (make-bytes amt))
          (bytes-copy! dest-bstr 0 bstr i (+ i amt))
          ;; Keep/resume fast mode
          (set-direct-pos! b (fx+ i amt))
          (set-direct-bstr! b bstr)
          (set! offset 0)
          (progress!)
          (finish dest-bstr)))))])

(define (make-input-bytes bstr name)
  (finish-port/count
   (new bytes-input-port
        #:field
        [name name]
        [buffer (direct bstr 0 (bytes-length bstr))]
        [bstr bstr])))

;; ----------------------------------------

(class bytes-output-port #:extends core-output-port
  #:field
  [bstr #""]
  [pos 0]
  [max-pos 0]

  #:public
  [get-length (lambda ()
                (start-atomic)
                (slow-mode!)
                (end-atomic)
                max-pos)]
  [get-bytes (lambda (dest-bstr start-pos discard?)
               (start-atomic)
               (slow-mode!)
               (bytes-copy! dest-bstr 0 bstr start-pos (fx+ start-pos (bytes-length dest-bstr)))
               (when discard?
                 (set! bstr #"")
                 (set! pos 0)
                 (set! max-pos 0))
               (end-atomic))]

  #:private
  [enlarge!
   (lambda (len)
     (define new-bstr (make-bytes (fx* 2 len)))
     (bytes-copy! new-bstr 0 bstr 0 pos)
     (set! bstr new-bstr))]

  [slow-mode!
   (lambda ()
     (define b buffer)
     (when (direct-bstr b)
       (define s (direct-pos b))
       (set! pos s)
       (set-direct-pos! b (direct-end b))
       (set-direct-bstr! b #f)
       (set! offset s)
       (set! max-pos (fxmax s max-pos))))]

  [fast-mode!
   (lambda ()
     (define b buffer)
     (set-direct-bstr! b bstr)
     (set-direct-pos! b pos)
     (set-direct-end! b (bytes-length bstr))
     (set! offset 0))]

  #:override
  [write-out
   (lambda (src-bstr src-start src-end nonblock? enable-break? copy?)
     (slow-mode!)
     (define i pos)
     (define amt (min (fx- src-end src-start) 4096))
     (define end-i (fx+ i amt))
     (when ((bytes-length bstr) . < . end-i)
       (enlarge! end-i))
     (bytes-copy! bstr i src-bstr src-start (fx+ src-start amt))
     (set! pos end-i)
     (set! max-pos (fxmax pos max-pos))
     (fast-mode!)
     amt)]
  [get-write-evt
   (get-write-evt-via-write-out (lambda (out v bstr start)
                                  (port-count! out v bstr start)))]
  [file-position
   (case-lambda
     [()
      (define b buffer)
      (if (direct-bstr b) (direct-pos b) pos)]
     [(new-pos)
      (slow-mode!)
      (define len (bytes-length bstr))
      (cond
        [(eof-object? new-pos)
         (set! pos max-pos)]
        [(new-pos . > . len)
         (when (new-pos . >= . (expt 2 48))
           ;; implausibly large
           (end-atomic)
           (raise-arguments-error 'file-position
                                  "new position is too large"
                                  "port" this
                                  "position" new-pos))
         (enlarge! len)
         (set! pos new-pos)
         (set! max-pos new-pos)]
        [else
         (set! pos new-pos)
         (set! max-pos (fxmax max-pos new-pos))])])])

(define (open-output-bytes [name 'string])
  (finish-port/count
   (new bytes-output-port
        #:field
        [bstr (make-bytes 16)]
        [name name]
        [evt always-evt])))

(define/who (get-output-bytes o [reset? #f] [start-pos 0] [end-pos #f])
  (check who (lambda (v) (and (output-port? o) (string-port? o)))
         #:contract "(and/c output-port? string-port?)"
         o)
  (check who exact-nonnegative-integer? start-pos)
  (check who exact-nonnegative-integer? #:or-false end-pos)
  (let ([o (->core-output-port o)])
    (start-atomic)
    (define len (send bytes-output-port o get-length))
    (when (start-pos . > . len)
      (end-atomic)
      (raise-range-error who "port content" "starting " start-pos o 0 len #f))
    (when end-pos
      (unless (<= start-pos end-pos len)
        (end-atomic)
        (raise-range-error who "port content" "ending " end-pos o 0 len start-pos)))
    (define amt (- (min len (or end-pos len)) start-pos))
    (define bstr (make-bytes amt))
    (send bytes-output-port o get-bytes bstr start-pos reset?)
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
       (bytes-output-port? p))]
    [else
     (raise-argument-error 'string-port? "port?" p)]))
