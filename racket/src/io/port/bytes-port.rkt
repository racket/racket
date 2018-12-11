#lang racket/base
(require "../common/check.rkt"
         "../common/fixnum.rkt"
         "../common/object.rkt"
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

(struct input-bytes-data ())

(define/who (open-input-bytes bstr [name 'string])
  (check who bytes? bstr)
  (define p (make-input-bytes (bytes->immutable-bytes bstr) name))
  (when (port-count-lines-enabled)
    (port-count-lines! p))
  p)

(define-constructor (make-input-bytes bstr name)
  (define-fixnum i 0)
  (define alt-pos #f)
  (define len (bytes-length bstr))

  (define progress-sema #f)
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

  (make-core-input-port
   #:name name
   #:data (input-bytes-data)
   #:self self

   #:prepare-change
   (method
    (lambda ()
      (pause-waiting-commit)))

   #:read-byte
   (method
    (lambda ()
      (let ([pos i])
        (if (pos . < . len)
            (begin
              (set! i (add1 pos))
              (progress!)
              (bytes-ref bstr pos))
            eof))))

   #:read-in
   (method
    (lambda (dest-bstr start end copy?)
      (define pos i)
      (cond
        [(pos . < . len)
         (define amt (min (- end start) (- len pos)))
         (set! i (+ pos amt))
         (bytes-copy! dest-bstr start bstr pos (+ pos amt))
         (progress!)
         amt]
        [else eof])))

   #:peek-byte
   (method
    (lambda ()
      (let ([pos i])
        (if (pos . < . len)
            (bytes-ref bstr pos)
            eof))))

   #:peek-in
   (method
    (lambda (dest-bstr start end skip progress-evt copy?)
      (define pos (+ i skip))
      (cond
        [(and progress-evt (sync/timeout 0 progress-evt))
         #f]
        [(pos . < . len)
         (define amt (min (- end start) (- len pos)))
         (bytes-copy! dest-bstr start bstr pos (+ pos amt))
         amt]
        [else eof])))

   #:byte-ready
   (method
    (lambda (work-done!)
      (i . < . len)))

   #:close
   (method
    (lambda ()
      (set! commit-manager #f) ; to indicate closed
      (progress!)))

   #:get-progress-evt
   (method
    (lambda ()
      (unless progress-sema
        (set! progress-sema (make-semaphore)))
      (semaphore-peek-evt progress-sema)))

   #:commit
   (method
    (lambda (amt progress-evt ext-evt finish)
      (unless commit-manager
        (set! commit-manager (make-commit-manager)))
      (commit-manager-wait
       commit-manager
       progress-evt ext-evt
       ;; in atomic mode, maybe in a different thread:
       (lambda ()
         (let ([amt (min amt (- len i))])
           (define dest-bstr (make-bytes amt))
           (bytes-copy! dest-bstr 0 bstr i (+ i amt))
           (set! i (+ i amt))
           (progress!)
           (finish dest-bstr))))))

   #:file-position
   (method
    (case-lambda
      [() (or alt-pos i)]
      [(new-pos)
       (set! i (if (eof-object? new-pos)
                   len
                   (min len new-pos)))
       (set! alt-pos
             (and new-pos
                  (not (eof-object? new-pos))
                  (new-pos . > . i)
                  new-pos))]))))

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
     #:write-out o
     #:close
     (lambda (o) ((core-port-close o) (core-port-self o)))
     #:get-write-evt
     (and (core-output-port-get-write-evt o)
          (lambda (o orig-o bstr start-k end-k)
            ((core-output-port-get-write-evt o) (core-port-self o) o bstr start-k end-k)))
     #:get-location
     (and (core-port-get-location o)
          (lambda (o) ((core-port-get-location o) (core-port-self o))))
     #:count-lines!
     (and (core-port-count-lines! o)
          (lambda (o)
            ((core-port-count-lines! o) (core-port-self o))))
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
           ((core-output-port-write-out o) (core-port-self o) (make-bytes amt 0) 0 amt #f #f #f)
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
       (input-bytes-data? (core-port-data p)))]
    [(output-port? p)
     (let ([p (->core-output-port p)])
       (output-bytes-data? (core-port-data p)))]
    [else
     (raise-argument-error 'string-port? "port?" p)]))
