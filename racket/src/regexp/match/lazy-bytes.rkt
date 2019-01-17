#lang racket/base

;; A compiled matcher accepts a byte string or a `lazy-bytes` object,
;; where the later is used to pull bytes on demand from a port or a
;; long character string.

(provide make-lazy-bytes
         lazy-bytes-before-end?
         lazy-bytes-ref
         lazy-bytes-bstr
         lazy-bytes-failed?
         lazy-bytes-discarded-count
         lazy-bytes-advance!)

(struct lazy-bytes ([bstr #:mutable] ; buffered bytes
                    [end #:mutable]  ; number of available bytes --- plus discarded bytes
                    in               ; input port
                    skip-amt         ; offset into the port; 0 if `(not peek?)`
                    prefix-len       ; length of prefix (not from port)
                    peek?            ; peeking mode
                    immediate-only?  ; non-blocking mode; implies `peek?`
                    progress-evt     ; stop peeking if ready
                    out              ; output hold discarded bytes; implies `(not peek?)`
                    max-lookbehind   ; bytes before current counter to preserve, if `out`
                    [failed? #:mutable] ; set to #t if `progress-evt` fires or read blocks
                    [discarded-count #:mutable] ; bytes discarded, if not `peek?`
                    max-peek))       ; maximum number of bytes to peek or #f

(define (make-lazy-bytes in skip-amt prefix
                         peek? immediate-only? progress-evt
                         out max-lookbehind
                         max-peek)
  (define len (bytes-length prefix))
  (lazy-bytes prefix len in skip-amt len
              peek? immediate-only? progress-evt
              out max-lookbehind
              #f 0
              max-peek))

(define (lazy-bytes-before-end? s pos end)
  (and (or (not (exact-integer? end))
           (pos . < . end))
       (cond
        [(pos . < . (lazy-bytes-end s))
         #t]
        [else
         (and (get-more-bytes! s)
              (lazy-bytes-before-end? s pos end))])))

(define (lazy-bytes-ref s pos)
  ;; Assume a preceding `lazy-bytes-before-end?` call, so
  ;; we have the byte
  (bytes-ref (lazy-bytes-bstr s) (- pos (lazy-bytes-discarded-count s))))

(define (lazy-bytes-advance! s given-pos force?)
  ;; If we advance far enough and not peeking,
  ;; then flush unneeded bytes...
  ;; The promise is that we won't ask for bytes before
  ;; `pos` minus the `max-lookbehind`
  (when force?
    (lazy-bytes-before-end? s given-pos 'eof))
  (define pos (min given-pos (lazy-bytes-end s)))
  (when (and (lazy-bytes? s)
             (not (lazy-bytes-peek? s)))
    (define discarded-count (lazy-bytes-discarded-count s))
    (define unneeded (- pos
                        discarded-count
                        (lazy-bytes-max-lookbehind s)))
    (when (or force? (unneeded . > . 4096))
      (define amt (if force?
                      (- pos (lazy-bytes-discarded-count s))
                      4096))
      (define bstr (lazy-bytes-bstr s))
      (define out (lazy-bytes-out s))
      (when out
        ;; Discard bytes to `out`
        (define prefix-len (lazy-bytes-prefix-len s))
        (write-bytes bstr
                     out
                     ;; Skip over bytes that are part of the prefix:
                     (cond
                      [(discarded-count . > . prefix-len) 0]
                      [else (min amt (- prefix-len discarded-count))])
                     ;; To amount to discard:
                     amt))
      (define copy-end (- (lazy-bytes-end s) discarded-count))
      (unless (= amt copy-end)
        (bytes-copy! bstr 0 bstr amt copy-end))
      (set-lazy-bytes-discarded-count! s (+ amt discarded-count)))))

;; ----------------------------------------

;; Result reports whether new bytes were read
(define (get-more-bytes! s)
  (cond
   [(lazy-bytes? s)
    (define discarded-count (lazy-bytes-discarded-count s))
    (define len (- (lazy-bytes-end s) discarded-count))
    (define bstr (lazy-bytes-bstr s))
    (cond
     [(lazy-bytes-failed? s) #f]
     [(len . < . (bytes-length bstr))
      ;; Room in current byte string
      (define n ((if (lazy-bytes-immediate-only? s)
                     peek-bytes-avail!*
                     peek-bytes-avail!)
                 bstr
                 (+ (- len (lazy-bytes-prefix-len s))
                    (lazy-bytes-skip-amt s)
                    discarded-count)
                 (lazy-bytes-progress-evt s)
                 (lazy-bytes-in s)
                 len))
      (cond
       [(eof-object? n) #f]
       [(zero? n)
        ;; would block or progress evt became ready
        (set-lazy-bytes-failed?! s #t)
        #f]
       [else
        (set-lazy-bytes-end! s (+ n len discarded-count))
        #t])]
     [else
      (define max-peek (lazy-bytes-max-peek s))
      (define prefix-len (and max-peek (lazy-bytes-prefix-len s)))
      (cond
        [(and max-peek
              (len . >= . (- (+ max-peek prefix-len) discarded-count)))
         ;; Not allowed to read any more
         #f]
        [else
         ;; We're going to need a bigger byte string
         (define bstr2 (make-bytes (let ([sz (max 32 (* 2 (bytes-length bstr)))])
                                     (if max-peek
                                         (min sz (- (+ prefix-len max-peek) discarded-count))
                                         sz))))
         (bytes-copy! bstr2 0 bstr 0 len)
         (set-lazy-bytes-bstr! s bstr2)
         (get-more-bytes! s)])])]
   [else #f]))
