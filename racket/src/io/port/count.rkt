#lang racket/base
(require racket/fixnum
         "../common/check.rkt"
         "../common/class.rkt"
         "../host/thread.rkt"
         "port.rkt"
         "input-port.rkt"
         "output-port.rkt"
         "check.rkt"
         "file-position.rkt"
         "../string/utf-8-decode.rkt")

(provide port-count-lines-enabled
         port-count-graphemes-enabled
         finish-port/count

         port-count-lines!
         port-counts-lines?
         port-counts-graphemes?
         port-next-location
         set-port-next-location!
         
         port-count!
         port-count-byte!

         port-count-all!
         port-count-byte-all!)

(define port-count-lines-enabled
  (make-parameter #f (lambda (v) (and v #t)) 'port-count-lines-enabled))

(define port-count-graphemes-enabled
  (make-parameter #t (lambda (v) (and v #t)) 'port-count-graphemes-enabled))

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
       (define grcl-state (if (port-count-graphemes-enabled) 0 'no))
       (set-core-port-count! p (location #f grcl-state 1 0 (add1 (or (core-port-offset p) 0))))
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

(define/who (port-counts-graphemes? p)
  (and (let ([cl (core-port-count
                  (cond
                    [(input-port? p) (->core-input-port p)]
                    [(output-port? p) (->core-output-port p)]
                    [else
                     (check who #:test #f #:contract "port?" p)]))])
         (and cl (not (symbol? (location-grcl-state cl)))))
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
           (define grcl-state (location-grcl-state loc))
           ;; assume that pending characters form a grapheme cluster
           (cond
             [(pair? grcl-state)
              (define delta (sub1 (cdr grcl-state)))
              (define col (location-column loc))
              (define pos (location-position loc))
              (values (location-line loc)
                      (and col (- col delta))
                      (and pos (- pos delta)))]
             [else
              (values (location-line loc)
                      (location-column loc)
                      (location-position loc))])]))]
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
;; --- which involves UTF-8 decoding and then grapheme clustering. To
;; make column and position counting interact well with decoding
;; errors, the column and position are advanced while accumulating
;; decoding information, and then the column and position can go
;; backwards when the decoding completes.
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
               [grcl-state (location-grcl-state loc)]) ; 'cr or 'no => CRLF only, 0 => no pending, fixnum => 1 pending, otherwise `(cons state pending)`
      (define (finish-utf-8 i abort-mode)
        (define-values (used-bytes got-chars new-state got-grcl new-grcl-state)
          (utf-8-decode! bstr (- i span) i
                         #f 0 #f
                         grcl-state
                         #:error-char #\?
                         #:abort-mode abort-mode
                         #:state state))
        (define delta-grcl (- got-grcl
                              ;; Correct for earlier increment of position
                              ;; and column based on not-yet-decoded bytes
                              ;; and not-yet-consumed grapheme characters,
                              ;; leaving counts for still-not-decoded bytes
                              ;; and still-not-consumed grapheme characters
                              ;; in place:
                              (+ span
                                  (- (if (utf-8-state? state)
                                         (utf-8-state-pending-amt state)
                                         0)
                                     (if (utf-8-state? new-state)
                                         (utf-8-state-pending-amt new-state)
                                         0)))
                              (- (cond
                                   [(pair? grcl-state) (cdr grcl-state)]
                                   [(symbol? grcl-state) (if (eq? grcl-state 'cr) 1 0)]
                                   [(fx= 0 grcl-state) 0]
                                   [else 1])
                                 (cond
                                   [(pair? new-grcl-state) (cdr new-grcl-state)]
                                   [(symbol? new-grcl-state) (if (eq? grcl-state 'cr) 1 0)]
                                   [(fx= 0 new-grcl-state) 0]
                                   [else 1]))))
        (define (keep-aborts s) (if (eq? s 'complete) #f s))
        (loop i 0 line (and column (+ column delta-grcl)) (and position (+ position delta-grcl))
              (keep-aborts new-state) new-grcl-state))
      (cond
       [(= i end)
        (cond
         [(fx= 0 span)
          (set-location-line! loc line)
          (set-location-column! loc column)
          (set-location-position! loc position)
          (set-location-state! loc state)
          (set-location-grcl-state! loc grcl-state)]
         [else          
          ;; span doesn't include CR, LF, or tab
          (finish-utf-8 end 'state)])]
       [else
        (define b (bytes-ref bstr i))
        (define (end-utf-8) ; => next byte is ASCII, so we can terminate a UTF-8 sequence
          (finish-utf-8 i 'error))
        (define (end-grcl)
          ;; Next character doesn't continue a grapheme cluster; reset state to 0
          (cond
            [(pair? grcl-state)
             ;; all pending chars form one grapheme cluster, so revert tentative increments
             (define n (sub1 (cdr grcl-state)))
             (loop i 0 line (and column (- column n)) (and position (- position n)) #f 0)]
            [else
             ;; pending character is its own grapheme cluster
             (loop i 0 line column position #f (if (symbol? grcl-state) #f (if (symbol? grcl-state) 'no 0)))]))
        (cond
         [(eq? b (char->integer #\newline))
          (cond
            [(or state (not (fx= 0 span))) (end-utf-8)]
            [(pair? grcl-state)
             ;; A "\r" state will not be represented as a pair, so end previous cluster
             (end-grcl)]
            [(eqv? 0 grcl-state)
             (loop (fx+ i 1) 0 (and line (add1 line)) (and column 0) (and position (add1 position)) #f 0)]
            [(symbol? grcl-state)
             ;; 'cr or 'no, where we've hit "\r\n" if 'cr
             (if (eq? grcl-state 'cr)
                 (loop (fx+ i 1) 0 line column position #f 'no)
                 (loop (fx+ i 1) 0 (and line (add1 line)) (and column 0) (and position (add1 position)) #f 'no))]
            [else
             ;; "\r\n" combination counts as a single position; it turns out that `char-grapheme-step`
             ;; will return `new-grcl-state` as 0 if a "\r\n" is consumed
             (define-values (consume? new-grcl-state) (char-grapheme-step #\newline grcl-state))
             (cond
               [(fx= 0 new-grcl-state)
                (loop (fx+ i 1) 0 line column position #f 0)]
               [else
                ;; assert: `consumed?` is true, because "\r\n" is the only way that "\n" continues;
                ;; also, "\n" won't continue to anything else
                (loop (fx+ i 1) 0 (and line (add1 line)) (and column 0) (and position (add1 position)) #f 0)])])]
         [(eq? b (char->integer #\return))
          (cond
            [(and (fx= 0 span) (not state))
             (cond
               [(eqv? 0 grcl-state)
                (define-values (consume? new-grcl-state) (char-grapheme-step #\return 0))
                (loop (fx+ i 1) 0 (and line (add1 line)) (and column 0) (and position (add1 position)) #f new-grcl-state)]
               [(symbol? grcl-state)
                (loop (fx+ i 1) 0 (and line (add1 line)) (and column 0) (and position (add1 position)) #f 'cr)]
               [else
                (end-grcl)])]
            [else
             (end-utf-8)])]
         [(eq? b (char->integer #\tab))
          (cond
            [(and (fx= 0 span) (not state))
             (if (eqv? 0 grcl-state)
                 (loop (fx+ i 1) 0 line (and column (+ (bitwise-and column -8) 8)) (and position (add1 position)) #f 0)
                 ;; A tab (which is a control character) cannot continue a grapheme cluster
                 (end-grcl))]
            [else
             (end-utf-8)])]
         [(b . >= . 128)
          ;; This is one place where we tentatively increment the column and position, to be
          ;; reverted later if decoding and/or clustering collapses multiple bytes:
          (loop (fx+ i 1) (fx+ span 1) line (and column (add1 column)) (and position (add1 position)) state grcl-state)]
         [else
          (cond
            [(and (fx= 0 span) (not state))
             (cond
               [(not (pair? grcl-state))
                ;; This common case is also handled in `port-count-byte!`
                (cond
                  [(symbol? grcl-state)
                   (loop (fx+ i 1) 0 line (and column (add1 column)) (and position (add1 position)) #f 'no)]
                  [else
                   (define-values (consumed? new-grcl-state) (char-grapheme-step (integer->char b) grcl-state))
                   (cond
                     [(or (fx= 0 grcl-state)
                          consumed?)
                      ;; number of grcl-pending characters is accurately reflected by `new-grcl-state`; if it's
                      ;; non-0, this is a tentative increment
                      (loop (fx+ i 1) 0 line (and column (add1 column)) (and position (add1 position)) #f new-grcl-state)]
                     [(and consumed?
                           (fx= 0 new-grcl-state))
                      ;; old state must be non-0; since if new state is 0, then both characters consumed as one grcl,
                      ;; so don't increment
                      (loop (fx+ i 1) 0 line column position #f 0)]
                     [else
                      ;; two characters are now pending for the grapheme cluster, and this is a tentative increment
                      ;; for the second one
                      (loop (fx+ i 1) 0 line (and column (add1 column)) (and position (add1 position)) #f (cons new-grcl-state 2))])])]
               [else
                (define-values (consumed? new-grcl-state) (char-grapheme-step (integer->char b) (car grcl-state)))
                (cond
                  [consumed?
                   ;; pending characters (and maybe new one) consumed as one grapheme cluster, so we need to revert
                   ;; all but one tentative increment; at most one character is pending, reflected by `new-grcl-state`
                   (define n (- (if (fx= 0 new-grcl-state) 1 2) (cdr grcl-state)))
                   (loop (fx+ i 1) 0 line (and column (+ column n)) (and position (+ position n)) #f new-grcl-state)]
                  [else
                   ;; new character is pending
                   (loop (fx+ i 1) 0 line (and column (add1 column)) (and position (add1 position)) #f
                         (cons new-grcl-state (add1 (cdr grcl-state))))])])]
            [else
             ;; This is another place where we tentatively increment the column and position, to be
             ;; reverted later if clustering collapses multiple characters:
             (loop (fx+ i 1) (fx+ span 1) line (and column (add1 column)) (and position (add1 position)) state grcl-state)])])]))))

;; in atomic mode
(define (port-count-all! in extra-ins amt bstr start)
  (port-count! in amt bstr start)
  (for ([in (in-list extra-ins)])
    (port-count! in amt bstr start)))

;; in atomic mode
;; If `b` is not a byte, it is treated like #\x.
(define (port-count-byte! in b)
  (increment-offset! in 1)
  (define loc (core-port-count in))
  (when loc
    (cond
     [(or (location-state loc)
          (pair? (location-grcl-state loc))
          (and (fixnum? b) (b . > . 127))
          (eq? b (char->integer #\return))
          (eq? b (char->integer #\newline))
          (eq? b (char->integer #\tab)))
      (port-count! in 1 (if (fixnum? b) (bytes b) #"x") 0)]
     [else
      ;; Same as handling above in `port-count!` (see rationale there):
      (define grcl-state (location-grcl-state loc))
      (cond
        [(symbol? grcl-state)
         ;; only counting CRLF
         (define column (location-column loc))
         (define position (location-position loc))
         (when position (set-location-position! loc (add1 position)))
         (when column (set-location-column! loc (add1 column)))
         (set-location-grcl-state! loc 'no)]
        [else
         (define-values (consumed? new-grcl-state)
           (char-grapheme-step (if (fixnum? b) (integer->char b) #\x) grcl-state))
         (cond
           [(and (fx= 0 new-grcl-state) ; implies `consumed?`
                 (not (fx= 0 grcl-state)))
            (set-location-grcl-state! loc 0)]
           [else
            (define column (location-column loc))
            (define position (location-position loc))
            (when position (set-location-position! loc (add1 position)))
            (when column (set-location-column! loc (add1 column)))
            (set-location-grcl-state! loc (if (or consumed?
                                                  (fx= 0 grcl-state))
                                              new-grcl-state
                                              (cons new-grcl-state 2)))])])])))

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
