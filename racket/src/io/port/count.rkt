#lang racket/base
(require "../common/check.rkt"
         "../common/class.rkt"
         "../host/thread.rkt"
         "port.rkt"
         "input-port.rkt"
         "output-port.rkt"
         "check.rkt"
         "file-position.rkt"
         "../string/utf-8-decode.rkt")

(provide port-count-lines-enabled
         finish-port/count

         port-count-lines!
         port-counts-lines?
         port-next-location
         set-port-next-location!
         
         port-count!
         port-count-byte!

         port-count-all!
         port-count-byte-all!)

(define port-count-lines-enabled
  (make-parameter #f (lambda (v) (and v #t))))

(define (finish-port/count p)
  (when (port-count-lines-enabled)
    (port-count-lines! p))
  p)

(define/who (port-count-lines! p)
  (let ([p (cond
             [(input-port? p) (->core-input-port p)]
             [(output-port? p) (->core-output-port p)]
             [else (check who #:test #f #:contract "port?" p)])])
    (atomically
     (check-not-closed who p)
     (unless (core-port-count p)
       (set-core-port-count! p (location #f #f 1 0 (add1 (or (core-port-offset p) 0))))
       (define count-lines! (method core-port p count-lines!))
       (when count-lines!
         (count-lines! p))))))

(define/who (port-counts-lines? p)
  (and (core-port-count
        (cond
          [(input-port? p) (->core-input-port p)]
          [(output-port? p) (->core-output-port p)]
          [else
           (check who #:test #f #:contract "port?" p)]))
       #t))

(define/who (port-next-location p)
  (let ([p (cond
             [(input-port? p) (->core-input-port p)]
             [(output-port? p) (->core-output-port p)]
             [else
              (check who #:test #f #:contract "port?" p)])])
    (define loc (core-port-count p))
    (cond
      [loc
       (atomically
        (check-not-closed who p)
        (define get-location (method core-port p get-location))
        (cond
          [get-location
           (get-location p)]
          [else
           (values (location-line loc)
                   (location-column loc)
                   (location-position loc))]))]
      [(method core-port p file-position)
       (define offset (do-simple-file-position who p (lambda () #f)))
       (values #f #f (and offset (add1 offset)))]
      [else
       (define offset (get-core-port-offset p))
       (values #f #f (and offset (add1 offset)))])))

(define/who (set-port-next-location! p line col pos)
  (check who (lambda (p) (or (input-port? p) (output-port? p)))
         #:contract "port?"
         p)
  (check who #:or-false exact-positive-integer? line)
  (check who #:or-false exact-nonnegative-integer? col)
  (check who #:or-false exact-positive-integer? pos)
  (let ([p (cond
             [(input-port? p) (->core-input-port p)]
             [else (->core-output-port p)])])
    (atomically
     (define loc (core-port-count p))
     (when (and loc (not (method core-port p get-location)))
       (set-location-line! loc line)
       (set-location-column! loc col)
       (set-location-position! loc pos)))))

;; in atomic mode
;; When line counting is enabled, increment line, column, etc. counts
;; --- which involves UTF-8 decoding. To make column and position counting
;; interact well with decoding errors, the column and position are advanced
;; while accumulating decoding information, and then the column and position
;; can go backwards when the decoding completes.
(define (port-count! in amt bstr start)
  (increment-offset! in amt)
  (define loc (core-port-count in))
  (when loc
    (define end (+ start amt))
    (let loop ([i start]
               [span 0] ; number of previous bytes still to send to UTF-8 decoding
               [line (location-line loc)]
               [column (location-column loc)]
               [position (location-position loc)]
               [state (location-state loc)]
               [cr-state (location-cr-state loc)]) ; #t => previous char was #\return
      (define (finish-utf-8 i abort-mode)
        (define-values (used-bytes got-chars new-state)
          (utf-8-decode! bstr (- i span) i
                         #f 0 #f
                         #:error-char #\?
                         #:abort-mode abort-mode
                         #:state state))
        (define delta-chars (- got-chars
                               ;; Correct for earlier increment of position
                               ;; and column based on not-yet-decoded bytes,
                               ;; leaving counts for still-not-decoded bytes
                               ;; in place:
                               (+ span
                                  (- (if (utf-8-state? state)
                                         (utf-8-state-pending-amt state)
                                         0)
                                     (if (utf-8-state? new-state)
                                         (utf-8-state-pending-amt new-state)
                                         0)))))
        (define (keep-aborts s) (if (eq? s 'complete) #f s))
        (loop i 0 line (and column (+ column delta-chars)) (and position (+ position delta-chars))
              (keep-aborts new-state) #f))
      (cond
       [(= i end)
        (cond
         [(zero? span)
          (set-location-line! loc line)
          (set-location-column! loc column)
          (set-location-position! loc position)
          (set-location-state! loc state)
          (set-location-cr-state! loc cr-state)]
         [else          
          ;; span doesn't include CR, LF, or tab
          (finish-utf-8 end 'state)])]
       [else
        (define b (bytes-ref bstr i))
        (define (end-utf-8) ; => next byte is ASCII, so we can terminate a UTF-8 sequence
          (finish-utf-8 i 'error))
        (cond
         [(eq? b (char->integer #\newline))
          (cond
           [(or state (not (zero? span))) (end-utf-8)]
           [cr-state
            ;; "\r\n" combination counts as a single position
            (loop (add1 i) 0 line column position #f #f)]
           [else
            (loop (add1 i) 0 (and line (add1 line)) (and column 0) (and position (add1 position)) #f #f)])]
         [(eq? b (char->integer #\return))
          (if (and (zero? span)(not state))
              (loop (add1 i) 0 (and line (add1 line)) (and column 0) (and position (add1 position)) #f #t)
              (end-utf-8))]
         [(eq? b (char->integer #\tab))
          (if (and (zero? span) (not state))
              (loop (add1 i) 0 line (and column (+ (bitwise-and column -8) 8)) (and position (add1 position)) #f #f)
              (end-utf-8))]
         [(b . < . 128)
          (if (and (zero? span) (not state))
              (loop (add1 i) 0 line (and column (add1 column)) (and position (add1 position)) #f #f)
              (loop (add1 i) (add1 span) line (and column (add1 column)) (and position (add1 position)) state #f))]
         [else
          ;; This is where we tentatively increment the column and position, to be
          ;; reverted later if decoding collapses multiple bytes:
          (loop (add1 i) (add1 span) line (and column (add1 column)) (and position (add1 position)) state #f)])]))))

;; in atomic mode
(define (port-count-all! in extra-ins amt bstr start)
  (port-count! in amt bstr start)
  (for ([in (in-list extra-ins)])
    (port-count! in amt bstr start)))

;; in atomic mode
;; If `b` is not a byte, it is treated like
;; a non-whitespace byte.
(define (port-count-byte! in b)
  (increment-offset! in 1)
  (define loc (core-port-count in))
  (when loc
    (cond
     [(or (location-state loc)
          (location-cr-state loc)
          (and (fixnum? b) (b . > . 127))
          (eq? b (char->integer #\return))
          (eq? b (char->integer #\newline))
          (eq? b (char->integer #\tab)))
      (port-count! in 1 (bytes b) 0)]
     [else
      (let ([column (location-column loc)]
            [position (location-position loc)])
        (when position (set-location-position! loc (add1 position)))
        (when column (set-location-column! loc (add1 column))))])))

;; in atomic mode
(define (port-count-byte-all! in extra-ins b)
  (port-count-byte! in b)
  (for ([in (in-list extra-ins)])
    (port-count-byte! in b)))

;; in atomic mode
(define (increment-offset! in amt)
  (unless (direct-bstr (core-port-buffer in))
    (define old-offset (core-port-offset in))
    (when old-offset
      (set-core-port-offset! in (+ amt old-offset)))))
