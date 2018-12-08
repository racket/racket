#lang racket/base
(require "../host/thread.rkt"
         "port.rkt"
         "input-port.rkt"
         "output-port.rkt"
         "pipe.rkt")

(provide open-input-peek-via-read)

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
    ((core-input-port-prepare-change peek-pipe-i) (core-port-self peek-pipe-i)))

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
         (define write-out (core-output-port-write-out peek-pipe-o))
         (define just-wrote (write-out (core-port-self peek-pipe-o) buf wrote v #t #f #f))
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
         (define read-in (core-input-port-read-in peek-pipe-i))
         (read-in (core-port-self peek-pipe-i) dest-bstr start end copy?)]
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
  (define (read-byte self)
    (define b ((core-input-port-read-byte peek-pipe-i) (core-port-self peek-pipe-i)))
    (cond
      [(or (fixnum? b) (eof-object? b))
       b]
      [peeked-eof?
       (set! peeked-eof? #f)
       ;; an EOF doesn't count as progress
       eof]
      [else
       (define v (pull-some-bytes #:keep-eof? #f))
       (cond
         [(retry-pull? v) (read-byte self)]
         [else
          (progress!)
          v])]))

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
         (define peek-in (core-input-port-peek-in peek-pipe-i))
         (peek-in (core-port-self peek-pipe-i) dest-bstr start end skip progress-evt copy?)]
        [peeked-eof?
         eof]
        [else
         (define v (pull-some-bytes))
         (if (retry-pull? v)
             (try-again)
             v)])))

  ;; in atomic mode
  (define (peek-byte self)
    (cond
      [(positive? (pipe-content-length peek-pipe-i))
       ((core-input-port-peek-byte peek-pipe-i) (core-port-self peek-pipe-i))]
      [peeked-eof?
       eof]
      [else
       (define v (pull-some-bytes))
       (if (retry-pull? v)
           (peek-byte self)
           v)]))

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
    ((core-input-port-get-progress-evt peek-pipe-i) (core-port-self peek-pipe-i)))

  ;; in atomic mode
  (define (progress!)
    ;; Relies on support for `0 #f #f` arguments in pipe implementation:
    ((core-input-port-commit peek-pipe-i) (core-port-self peek-pipe-i) 0 #f #f void))

  (define (commit self amt evt ext-evt finish)
    ((core-input-port-commit peek-pipe-i) (core-port-self peek-pipe-i) amt evt ext-evt finish))

  (define do-buffer-mode
    (case-lambda
      [(self) buffer-mode]
      [(self mode) (set! buffer-mode mode)]))

  (values (make-core-input-port
           #:name name
           #:data data
           #:self #f

           #:prepare-change prepare-change
           
           #:read-byte read-byte
           #:read-in do-read-in
           #:peek-byte peek-byte
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
