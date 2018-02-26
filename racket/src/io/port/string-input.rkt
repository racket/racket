#lang racket/base
(require "../common/check.rkt"
         "../host/thread.rkt"
         "parameter.rkt"
         "read-and-peek.rkt"
         "port.rkt"
         "input-port.rkt"
         (submod "bytes-input.rkt" internal)
         "../string/utf-8-decode.rkt"
         "count.rkt"
         "flush-output.rkt"
         "check.rkt"
         "prepare-change.rkt")

(provide read-char
         read-string
         read-string!
         
         peek-char
         peek-string
         peek-string!

         do-read-char
         do-peek-char)

;; ----------------------------------------

;; Read up to `(- end start)` characters by UTF-8 decoding of bytes,
;; producing at least one character unless `zero-ok?`, but it's
;; possible that fewer that `(- end start)` characters are read. The
;; result is two values: either EOF or the number of read characters,
;; and the number of converted bytes
(define (read-some-chars! who orig-in str start end
                          #:zero-ok? [zero-ok? #f]
                          #:extra-bytes-amt [extra-bytes-amt 0]
                          #:keep-eof? [keep-eof? #f]
                          #:just-peek? [just-peek? #f]
                          #:skip [skip-k 0] ; must be 0 if `(not just-peek?)`
                          #:special-ok? [special-ok? #f])
  (define amt (- end start))
  (define bstr (make-bytes amt))
  ;; We're allowed to read up to `amt` characters, which means at
  ;; least `amt` bytes.
  (define consumed-v
    (cond
      [just-peek? 0]
      [else
       (read-some-bytes! who orig-in bstr 0 amt
                         #:zero-ok? zero-ok?
                         #:copy-bstr? #f
                         #:keep-eof? keep-eof?
                         #:special-ok? special-ok?)]))
  (define v
    (cond
      [just-peek?
       (peek-some-bytes! who orig-in 
                         bstr consumed-v amt skip-k
                         #:copy-bstr? #f
                         #:zero-ok? zero-ok?)]
      [else consumed-v]))
  ;; At this point, `v` is the number of bytes that we have ready, and
  ;; the first `consumed-v` of those are read (as opposed to just
  ;; peeked) from the port. [Currently, `consumed-v` is either 0 or `v`.]
  (cond
    [(not (exact-integer? v)) (values v 0)]
    [(zero? v) (values 0 0)]
    [else
     (define-values (used-bytes got-chars state)
       (utf-8-decode! bstr 0 v
                      str start (+ start amt)
                      #:error-char #\uFFFD
                      #:abort-mode 'state))
     ;; Includes consumed bytes:
     (define actually-used-bytes (- used-bytes
                                    (if (utf-8-state? state)
                                        (utf-8-state-pending-amt state)
                                        0)))
     ;; The `state` result can't be 'continues, because N
     ;; bytes will never produce > N chars; it can't be
     ;; 'error, because we provide an error character; it
     ;; can't be 'aborts, because we request an abort state
     (cond
       [(or (zero? got-chars)
            (actually-used-bytes . < . consumed-v))
        ;; The state must be an abort state.
        ;; We need to try harder to get a character; even if
        ;; `zero-ok?` is true, we may need to try asking
        ;; for more bytes to make any progress for a polling
        ;; request
        (let loop ([skip-k (+ skip-k (- v consumed-v))]
                   [total-used-bytes used-bytes]
                   [state state]
                   [total-chars got-chars]
                   [start (+ start got-chars)]
                   [amt (- amt got-chars)])
          (define v (peek-some-bytes! who orig-in bstr 0 1 skip-k
                                      #:zero-ok? zero-ok?
                                      #:special-ok? special-ok?))
          (cond
            [(and (eq? v 0)
                  (zero? consumed-v))
             ;; `zero-ok?` must be true, and we haven't
             ;; consumed any bytes, so give up
             (values 0 0)]
            [else
             ;; Try to convert with the additional byte; v can be
             ;; `eof` or a special-value procedure, in which case the
             ;; abort mode should be 'error to trigger decodings as
             ;; errors
             (define-values (used-bytes got-chars new-state)
               (if (eq? v 0)
                   (values 0 0 state)
                   (utf-8-decode! bstr 0 (if (integer? v) v 0)
                                  str start (+ start amt)
                                  #:error-char #\uFFFD
                                  #:state (and (utf-8-state? state) state)
                                  #:abort-mode (if (integer? v)
                                                   'state
                                                   'error))))
             (cond
               [(zero? got-chars)
                ;; Try even harder; we shouldn't get here if v was `eof`
                ;; or a special-value procedure
                (loop (+ skip-k v) (+ total-used-bytes used-bytes) new-state total-chars start amt)]
               [else
                ;; At this point `used-bytes` by itself can be negative, since
                ;; conversion may not have used all the bytes that
                ;; we peeked to try to complete a decoding. Those unused bytes
                ;; count again `skip-k`. Meanwhile, an error state might
                ;; report that some other bytes aren't actually consumed, yet.
                ;; Does not include consumed bytes:
                (define actually-used-bytes (- (+ total-used-bytes
                                                  used-bytes)
                                               (if (utf-8-state? new-state)
                                                   (utf-8-state-pending-amt new-state)
                                                   0)))
                (cond
                  [(actually-used-bytes . < . consumed-v)
                   ;; We need to inspect at least one more byte to
                   ;; consume the bytes that we have already consumed from
                   ;; the point
                   (loop (+ skip-k v) (+ total-used-bytes used-bytes) new-state
                         (+ total-chars got-chars) (+ start got-chars) (- amt got-chars))]
                  [else
                   (unless just-peek?
                     (let ([discard-bytes (- actually-used-bytes consumed-v)])
                       (define finish-bstr (if (discard-bytes . <= . (bytes-length bstr))
                                               bstr
                                               (make-bytes discard-bytes)))
                       (do-read-bytes! who orig-in finish-bstr 0 discard-bytes)))
                   (values (+ total-chars got-chars)
                           actually-used-bytes)])])]))]
       [else
        ;; Conversion succeeded for at least 1 character. Since we used
        ;; all bytes that we consumed from the port, if more characters are needed,
        ;; another call to `read-some-chars!` can deal with it.
        (unless (or just-peek?
                    (= actually-used-bytes consumed-v))
          (do-read-bytes! who orig-in bstr 0 (- actually-used-bytes consumed-v)))
        (values got-chars actually-used-bytes)])]))

;; ----------------------------------------

;; Read `(- end start)` chars, stopping early only if an EOF is found
(define (do-read-string! who in str start end
                         #:just-peek? [just-peek? #f]
                         #:skip [skip-k 0]
                         #:special-ok? [special-ok? #f])
  (define amt (- end start))
  (define-values (v used-bytes) (read-some-chars! who in str start end
                                                  #:just-peek? just-peek?
                                                  #:skip skip-k
                                                  #:special-ok? special-ok?))
  (cond
   [(not (exact-integer? v)) v]
   [(= v amt) v]
   [else
    (let loop ([got v] [total-used-bytes used-bytes])
      (define-values (v used-bytes) (read-some-chars! who in str (+ start got) end
                                                      #:keep-eof? #t
                                                      #:just-peek? just-peek?
                                                      #:skip (if just-peek?
                                                                 (+ skip-k total-used-bytes)
                                                                 0)))
      (cond
        [(eof-object? v)
         got]
        [else
         (define new-got (+ got v))
         (cond
           [(= new-got amt) amt]
           [else (loop new-got (+ total-used-bytes used-bytes))])]))]))

;; ----------------------------------------

;; A shortcut to implement `read-char` in terms of a port-specific
;; `read-byte`:
(define (read-char-via-read-byte who in read-byte #:special-ok? [special-ok? #t])
  (define b
    (let loop ()
      (start-atomic)
      (prepare-change in)
      (check-not-closed who in)
      (define b (read-byte))
      (cond
        [(evt? b)
         (end-atomic)
         (sync b)
         (loop)]
        [else
         (unless (eof-object? b)
           (port-count-byte! in b))
         (end-atomic)
         b])))
  (cond
    [(eof-object? b) b]
    [else
     (cond
       [(b . < . 128) (integer->char b)]
       [else
        ;; UTF-8 decoding... May need to peek bytes to discover
        ;; whether the decoding will work (in which case this wasn't
        ;; much of a shortcut)
        (define bstr (bytes b))
        (define str (make-string 1))
        (define-values (used-bytes got-chars state)
          (utf-8-decode! bstr 0 1
                         #f 0 #f
                         #:abort-mode 'state))
        (cond
          [(eq? state 'error)
           ;; This happens if the byte is a UTF-8 continuation byte
           #\uFFFD]
          [else
           ;; Need to peek ahead
           (let loop ([skip-k 0] [state state])
             (define v (peek-some-bytes! who in bstr 0 1 skip-k #:copy-bstr? #f #:special-ok? special-ok?))
             (cond
               [(or (eof-object? v)
                    (procedure? v))
                ;; Already-consumed byte is an error byte
                #\uFFFD]
               [else
                (define-values (used-bytes got-chars new-state)
                  (utf-8-decode! bstr 0 1
                                 str 0 1
                                 #:state state
                                 #:error-char #\uFFFD
                                 #:abort-mode 'state))
                (cond
                  [(= got-chars 1)
                   (define actually-used-bytes (+ skip-k used-bytes))
                   (unless (zero? actually-used-bytes)
                     (define finish-bstr (if (actually-used-bytes . <= . (bytes-length bstr))
                                             bstr
                                             (make-bytes actually-used-bytes)))
                     (do-read-bytes! who in finish-bstr 0 actually-used-bytes))
                   (string-ref str 0)]
                  [else
                   (loop (add1 skip-k) new-state)])]))])])]))

;; ----------------------------------------

;; If `special-ok?`, can return a special-value procedure
(define (do-read-char who in #:special-ok? [special-ok? #f])
  (check who input-port? in)
  (let ([in (->core-input-port in)])
    (define read-byte (core-input-port-read-byte in))
    (cond
      [(not read-byte)
       (define str (make-string 1))
       (define-values (v used-bytes) (read-some-chars! who in str 0 1 #:special-ok? special-ok?))
       (if (eq? v 1)
           (string-ref str 0)
           v)]
      [else
       ;; Byte-level shortcut is available, so try it as a char shortcut
       (read-char-via-read-byte who in read-byte #:special-ok? special-ok?)])))

(define/who (read-char [in (current-input-port)])
  (check who input-port? in)
  (do-read-char who in))
  
(define/who (read-string amt [in (current-input-port)])
  (check who exact-nonnegative-integer? amt)
  (check who input-port? in)
  (maybe-flush-stdout in)
  (let ([in (->core-input-port in)])
    (define bstr (make-string amt))
    (define v (do-read-string! 'read-string in bstr 0 amt))
    (if (exact-integer? v)
        (if (= v amt)
            bstr
            (substring bstr 0 v))
        v)))

(define/who (read-string! str [in (current-input-port)] [start-pos 0] [end-pos (and (string? str)
                                                                                    (string-length str))])
  (check who string? str)
  (check who input-port? in)
  (check who exact-nonnegative-integer? start-pos)
  (check who exact-nonnegative-integer? end-pos)
  (check-range who start-pos end-pos (string-length str) str)
  (maybe-flush-stdout in)
  (let ([in (->core-input-port in)])
    (do-read-string! who in str start-pos end-pos)))

;; ----------------------------------------

(define (do-peek-string! who in str start end skip #:special-ok? [special-ok? #f])
  (do-read-string! who in str start end #:skip skip #:just-peek? #t #:special-ok? special-ok?))

(define (do-peek-char who in skip-k #:special-ok? [special-ok? #f])
  (let ([in (->core-input-port in)])
    (define peek-byte (and (zero? skip-k)
                           (core-input-port-peek-byte in)))
    (define b (and peek-byte (peek-byte)))
    (cond
      [(and b
            (or (eof-object? b)
	    	(and (byte? b)
                     (b . < . 128))))
       ;; Shortcut worked
       (if (eof-object? b) b (integer->char b))]
      [else
       ;; General mode
       (define bstr (make-string 1))
       (define v (do-peek-string! who in bstr 0 1 skip-k #:special-ok? special-ok?))
       (if (eq? v 1)
           (string-ref bstr 0)
           v)])))

(define/who (peek-char [in (current-input-port)] [skip-k 0])
  (check who input-port? in)
  (check who exact-nonnegative-integer? skip-k)
  (do-peek-char who in skip-k #:special-ok? #f))
  
(define/who (peek-string amt skip-k [in (current-input-port)])
  (check who exact-nonnegative-integer? amt)
  (check who exact-nonnegative-integer? skip-k)
  (check who input-port? in)
  (maybe-flush-stdout in)
  (let ([in (->core-input-port in)])
    (define bstr (make-string amt))
    (define v (do-peek-string! who in bstr 0 amt skip-k))
    (if (exact-integer? v)
        (if (= v amt)
            bstr
            (substring bstr 0 v))
        v)))

(define/who (peek-string! str skip-k [in (current-input-port)] [start-pos 0] [end-pos (and (string? str)
                                                                                           (string-length str))])
  (check who string? str)
  (check who exact-nonnegative-integer? skip-k)
  (check who input-port? in)
  (check who exact-nonnegative-integer? start-pos)
  (check who exact-nonnegative-integer? end-pos)
  (check-range who start-pos end-pos (string-length str) str)
  (maybe-flush-stdout in)
  (let ([in (->core-input-port in)])
    (do-peek-string! who str in start-pos end-pos skip-k)))
