#lang racket/base

(provide utf-8-decode!
         utf-8-max-aborts-amt
         
         utf-8-state?
         utf-8-state-pending-amt)

;; The maximum number of characters that might not be consumed
;; by a conversion at the tail of a byte string, assuming that
;; additional bytes could be added to the tail:
(define utf-8-max-aborts-amt 3)

(struct utf-8-state (accum         ; accumulated value for a partial decoding
                     remaining     ; number of bytes expected to finidh decoding
                     pending-amt)) ; number of bytes contributing to `accum`

;; Returns (values bytes-used chars-written (or/c 'complete 'continues 'aborts 'error state-for-aborts)),
;; where the number of bytes used can go negative if a previous abort state is provided
;; and further decoding reveals that earlier bytes were in error.
;;
;; The `abort-mode` argument determines what to do when reaching the end of the input
;; and an encoding needs more ytes:
;;   * 'error : treat the bytes as encoding errors
;;   * 'aborts : report 'aborts
;;   * 'state : return a value that encapsulates the state, so another call can continue
;;
;; The result state is
;;  * 'complete : all input read, all output written
;;  * 'continues : output full, and input contains more
;;  * 'aborts : see `abort-mode` above
;;  * 'error : encoding error, but only when `error-ch` is #f
;;  * state-for-aborts : see `abort-mode` above
;;
;; Beware that there is a similar copy of this code in "../converter/utf-8.rkt",
;; but that one is different enough to make abstraction difficult.
;;
(define (utf-8-decode! in-bstr in-start in-end
                       out-str out-start out-end  ; `out-str` and `out-end` can be #f no string result needed
                       #:error-char [error-ch #f] ; replaces an encoding error if non-#f
                       #:abort-mode [abort-mode 'error] ; 'error, 'aborts, or 'state
                       #:state [state #f])        ; state that was returned in place of a previous 'aborts result
  (define base-i ; start of current encoding sequence
    (if state
        (- in-start (utf-8-state-pending-amt state))
        in-start))
  (define accum ; accumulated value for encoding
    (if state
        (utf-8-state-accum state)
        0))
  (define remaining ; number of bytes still needed for the encoding
    (if state
        (utf-8-state-remaining state)
        0))

  ;; Iterate through the given byte string
  (let loop ([i in-start] [j out-start] [base-i base-i] [accum accum] [remaining remaining])

    ;; Shared handling for encoding failures:
    (define (encoding-failure)
      (cond
       [error-ch
        (when out-str (string-set! out-str j error-ch))
        (define next-j (add1 j))
        (define next-i (add1 base-i))
        (cond
         [(and out-end (= next-j out-end))
          (values (- next-i in-start)
                  (- next-j out-start)
                  'continues)]
         [else
          (loop next-i next-j next-i 0 0)])]
       [else
        (values (- base-i in-start)
                (- j out-start)
                'error)]))
    
    ;; Shared handling for decoding success:
    (define (continue)
      (define next-j (add1 j))
      (define next-i (add1 i))
      (cond
       [(and out-end (= next-j out-end))
        (values (- next-i in-start)
                (- next-j out-start)
                (if (= next-i in-end)
                    'complete
                    'continues))]
       [else
        (loop next-i next-j next-i 0 0)]))
    
    ;; Dispatch on byte:
    (cond
     [(= i in-end)
      ;; End of input
      (cond
       [(zero? remaining)
        (values (- base-i in-start)
                (- j out-start)
                'complete)]
       [(eq? abort-mode 'error)
        (encoding-failure)]
       [(eq? abort-mode 'state)
        (values (- i in-start) ; all bytes used
                (- j out-start)
                (utf-8-state accum remaining (- i base-i)))]
       [else
        (values (- base-i in-start) 
                (- j out-start)
                'aborts)])]
     [(i . < . in-start)
      ;; Happens only if we resume decoding with some state
      ;; and hit a decoding error; treat the byte as another
      ;; encoding error
      (encoding-failure)]
     [else
      (define b (bytes-ref in-bstr i))
      (cond
       [(b . < . 128)
        (cond
         [(zero? remaining)
          ;; Found ASCII
          (when out-str (string-set! out-str j (integer->char b)))
          (continue)]
         [else
          ;; We were accumulating bytes for an encoding, and
          ;; the encoding didn't complete
          (encoding-failure)])]
       [else
        ;; Encoding...
        (cond
          [(= #b10000000 (bitwise-and b #b11000000))
           ;; A continuation byte
          (cond
           [(zero? remaining)
            ;; We weren't continuing
            (encoding-failure)]
           [else
            (define next (bitwise-and b #b00111111))
            (define next-accum (bitwise-ior (arithmetic-shift accum 6) next))
            (cond
              [(= 1 remaining)
               (cond
                 [(and (next-accum . > . 127)
                       (next-accum . <= . #x10FFFF)
                       (not (and (next-accum . >= . #xD800)
                                 (next-accum . <= . #xDFFF))))
                  (when out-str (string-set! out-str j (integer->char next-accum)))
                  (continue)]
                 [else
                  ;; Not a valid character
                  (encoding-failure)])]
              [(and (= 2 remaining)
                    (next-accum . <= . #b11111))
               ;; A shorter byte sequence would work, so this is an
               ;; encoding mistae.
               (encoding-failure)]
              [(and (= 3 remaining)
                    (next-accum . <= . #b1111))
               ;; A shorter byte sequence would work
               (encoding-failure)]
              [else
               ;; Continue an encoding.
               (loop (add1 i) j base-i next-accum (sub1 remaining))])])]
          [(not (zero? remaining))
           ;; Trying to start a new encoding while one is in
           ;; progress
           (encoding-failure)]
          [(= #b11000000 (bitwise-and b #b11100000))
           ;; Start a two-byte encoding
           (define accum (bitwise-and b #b11111))
           ;; If `accum` is zero, that's an encoding mistake,
           ;; because a shorted byte sequence would work.
           (cond
             [(zero? accum) (encoding-failure)]
             [else (loop (add1 i) j i accum 1)])]
         [(= #b11100000 (bitwise-and b #b11110000))
          ;; Start a three-byte encoding
          (define accum (bitwise-and b #b1111))
          (loop (add1 i) j i accum 2)]
         [(= #b11110000 (bitwise-and b #b11111000))
          ;; Start a four-byte encoding
          (define accum (bitwise-and b #b111))
          (loop (add1 i) j i  accum 3)]
         [else
          ;; Five- or six-byte encodings don't produce valid
          ;; characters
          (encoding-failure)])])])))
