#lang racket/base
(require "../common/check.rkt"
         "../host/thread.rkt"
         "../string/utf-8-decode.rkt"
         "port.rkt"
         "input-port.rkt"
         "bytes-input.rkt"
         "check.rkt"
         "prepare-change.rkt")

(provide byte-ready?
         char-ready?)

(define/who (byte-ready? in)
  (check who input-port? in)
  (let loop ([in (->core-input-port in)])
    (define byte-ready (core-input-port-byte-ready in))
    (cond
      [(input-port? byte-ready) (loop (->core-input-port byte-ready))]
      [else
       (start-atomic)
       (prepare-change in)
       (check-not-closed who in)
       (define r (byte-ready void))
       (end-atomic)
       (eq? #t r)])))

(define/who (char-ready? in)
  (check who input-port? in)
  (let ([in (->core-input-port in)])
    (cond
      [(byte-ready? in)
       (define peek-byte (core-input-port-peek-byte in))
       (define b (and peek-byte (peek-byte)))
       (cond
         [(and b
               (or (eof-object? b)
                   (b . < . 128)))
          ;; Shortcut worked
          #t]
         [else
          (define bstr (make-bytes 1))
          (let loop ([offset 0] [state #f])
            (cond
              [(eq? 1 (peek-bytes-avail!* bstr offset #f in))
               (define-values (used-bytes got-chars new-state)
                 (utf-8-decode! bstr 0 1
                                #f 0 #f
                                #:error-char #\?
                                #:abort-mode 'state
                                #:state state))
               (cond
                 [(utf-8-state? new-state)
                  (loop (add1 offset) new-state)]
                 [else #t])]
              [else #f]))])]
      [else #f])))
