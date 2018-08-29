#lang racket/base
(require "../common/check.rkt"
         "match/regexp.rkt"
         "match/main.rkt"
         "replace/main.rkt"
         (only-in "common/range.rkt" range-place-init!))

(provide regexp
         byte-regexp
         pregexp
         byte-pregexp
         
         regexp-match
         regexp-match/end
         regexp-match-positions
         regexp-match-positions/end
         regexp-match?
         regexp-match-peek
         regexp-match-peek-positions
         regexp-match-peek-positions/end
         regexp-match-peek-immediate
         regexp-match-peek-positions-immediate
         regexp-match-peek-positions-immediate/end
         regexp-replace
         regexp-replace*
         
         regexp?
         byte-regexp?
         pregexp?
         byte-pregexp?
         
         regexp-max-lookbehind

         regexp-place-init!)

(define/who (regexp p [handler #f])
  (check who string? p)
  (make-regexp who p #f #f handler))
  
(define/who (byte-regexp p [handler #f])
  (check who bytes? p)
  (make-regexp who p #f #t handler))

(define/who (pregexp p [handler #f])
  (check who string? p)
  (make-regexp 'pregexp p #t #f handler))
  
(define/who (byte-pregexp p [handler #f])
  (check who bytes? p)
  (make-regexp 'byte-pregexp p #t #t handler))

(define/who (regexp-max-lookbehind rx)
  (check who
         #:test (or (regexp? rx) (byte-regexp? rx))
         #:contract "(or regexp? byte-regexp?)"
         rx)
  (rx:regexp-max-lookbehind rx))

;; ----------------------------------------

;; For especially simple and common cases, reduce the overhead created
;; by the general case by checking for simple cases and using a faster,
;; specific driver.

(define no-prefix #"")

(define (fast-bytes? rx in start-pos end-pos out prefix)
  (and (byte-regexp? rx)
       (bytes? in)
       (exact-nonnegative-integer? start-pos)
       (let ([len (bytes-length in)])
         (and (start-pos . <= . len)
              (or (not end-pos)
                  (and (exact-nonnegative-integer? end-pos)
                       (end-pos . <= . len)
                       (end-pos . >= . start-pos)))))
       (not out)
       (eq? prefix no-prefix)))

(define (fast-string? rx in start-pos end-pos out prefix)
  (and (regexp? rx)
       (string? in)
       (exact-nonnegative-integer? start-pos)
       (let ([len (string-length in)])
         (and (len . < . FAST-STRING-LEN)
              (start-pos . <= . len)
              (or (not end-pos)
                  (and (exact-nonnegative-integer? end-pos)
                       (end-pos . <= . len)
                       (end-pos . >= . start-pos)))))
       (not out)
       (eq? prefix no-prefix)))

(define/who (regexp-match? rx in [start-pos 0] [end-pos #f] [out #f] [prefix no-prefix])
  (cond
   [(fast-bytes? rx in start-pos end-pos out prefix)
    (fast-drive-regexp-match?/bytes rx in start-pos end-pos)]
   [(fast-string? rx in start-pos end-pos out prefix)
    (fast-drive-regexp-match?/string rx in start-pos end-pos)]
   [else
    (drive-regexp-match who rx in start-pos end-pos out prefix
                        #:mode '?)]))

(define/who (regexp-match-positions rx in [start-pos 0] [end-pos #f] [out #f] [prefix no-prefix])
  (cond
   [(fast-bytes? rx in start-pos end-pos out prefix)
    (fast-drive-regexp-match-positions/bytes rx in start-pos end-pos)]
   [(fast-string? rx in start-pos end-pos out prefix)
    (fast-drive-regexp-match-positions/string rx in start-pos end-pos)]
   [else
    (drive-regexp-match who rx in start-pos end-pos out prefix
                        #:mode 'positions)]))

(define/who (regexp-match rx in [start-pos 0] [end-pos #f] [out #f] [prefix no-prefix])
  (cond
   [(fast-bytes? rx in start-pos end-pos out prefix)
    (fast-drive-regexp-match/bytes rx in start-pos end-pos)]
   [(fast-string? rx in start-pos end-pos out prefix)
    (fast-drive-regexp-match/string rx in start-pos end-pos)]
   [else
    (drive-regexp-match who rx in start-pos end-pos out prefix
                        #:mode 'strings)]))

(define/who (regexp-match-positions/end rx in [start-pos 0] [end-pos #f] [out #f] [prefix no-prefix] [end-bytes-count 1])
  (drive-regexp-match who rx in start-pos end-pos out prefix
                      #:mode 'positions
                      #:end-bytes? #t
                      #:end-bytes-count end-bytes-count))

(define/who (regexp-match/end rx in [start-pos 0] [end-pos #f] [out #f] [prefix no-prefix] [end-bytes-count 1])
  (drive-regexp-match who rx in start-pos end-pos out prefix
                      #:mode 'strings
                      #:end-bytes? #t
                      #:end-bytes-count end-bytes-count))

(define/who (regexp-match-peek rx in [start-pos 0] [end-pos #f] [progress-evt #f] [prefix no-prefix])
  (drive-regexp-match who rx in start-pos end-pos #f prefix
                      #:peek? #t
                      #:progress-evt progress-evt
                      #:mode 'strings))

(define/who (regexp-match-peek-immediate rx in [start-pos 0] [end-pos #f] [progress-evt #f] [prefix no-prefix])
  (drive-regexp-match who rx in start-pos end-pos #f prefix
                      #:peek? #t #:immediate-only? #t
                      #:progress-evt progress-evt
                      #:mode 'strings))

(define/who (regexp-match-peek-positions rx in [start-pos 0] [end-pos #f] [progress-evt #f] [prefix no-prefix])
  (drive-regexp-match who rx in start-pos end-pos #f prefix
                      #:peek? #t
                      #:progress-evt progress-evt
                      #:mode 'positions))

(define/who (regexp-match-peek-positions/end rx in [start-pos 0] [end-pos #f] [progress-evt #f] [prefix no-prefix] [end-bytes-count 1])
  (drive-regexp-match who rx in start-pos end-pos #f prefix
                      #:peek? #t
                      #:progress-evt progress-evt
                      #:mode 'positions
                      #:end-bytes? #t
                      #:end-bytes-count end-bytes-count))

(define/who (regexp-match-peek-positions-immediate rx in [start-pos 0] [end-pos #f] [progress-evt #f] [prefix no-prefix])
  (drive-regexp-match who rx in start-pos end-pos #f prefix
                      #:peek? #t #:immediate-only? #t
                      #:progress-evt progress-evt
                      #:mode 'positions))

(define/who (regexp-match-peek-positions-immediate/end rx in [start-pos 0] [end-pos #f] [progress-evt #f] [prefix no-prefix] [end-bytes-count 1])
  (drive-regexp-match who rx in start-pos end-pos #f prefix
                      #:peek? #t #:immediate-only? #t
                      #:progress-evt progress-evt
                      #:mode 'positions
                      #:end-bytes? #t
                      #:end-bytes-count end-bytes-count))

;; ----------------------------------------

(define (regexp-place-init!)
  (range-place-init!))
