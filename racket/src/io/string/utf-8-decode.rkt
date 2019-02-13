#lang racket/base
(require racket/fixnum)

(provide utf-8-decode!
         utf-8-max-aborts-amt

         utf-8-decode-byte

         utf-8-state?
         utf-8-state-pending-amt

         a-bytes->string/utf-8)

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
;; and an encoding needs more bytes:
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
        (fx- in-start (utf-8-state-pending-amt state))
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

    ;; Shared handling for success:
    (define (complete accum)
      (when out-str (string-set! out-str j (integer->char accum)))
      (define next-j (fx+ j 1))
      (define next-i (fx+ i 1))
      (cond
        [(and out-end (fx= next-j out-end))
         (values (fx- next-i in-start)
                 (fx- next-j out-start)
                 (if (fx= next-i in-end)
                     'complete
                     'continues))]
        [else
         (loop next-i next-j next-i 0 0)]))

    ;; Shared handling for encoding failures:
    (define (encoding-failure)
      (cond
       [error-ch
        (when out-str (string-set! out-str j error-ch))
        (define next-j (fx+ j 1))
        (define next-i (fx+ base-i 1))
        (cond
         [(and out-end (fx= next-j out-end))
          (values (fx- next-i in-start)
                  (fx- next-j out-start)
                  'continues)]
         [else
          (loop next-i next-j next-i 0 0)])]
       [else
        (values (fx- base-i in-start)
                (fx- j out-start)
                'error)]))
    
    ;; Dispatch on byte:
    (cond
     [(fx= i in-end)
      ;; End of input
      (cond
       [(fx= remaining 0)
        (values (fx- base-i in-start)
                (fx- j out-start)
                'complete)]
       [(eq? abort-mode 'error)
        (encoding-failure)]
       [(eq? abort-mode 'state)
        (values (fx- i in-start) ; all bytes used
                (fx- j out-start)
                (utf-8-state accum remaining (fx- i base-i)))]
       [else
        (values (fx- base-i in-start) 
                (fx- j out-start)
                'aborts)])]
     [(i . fx< . in-start)
      ;; Happens only if we resume decoding with some state
      ;; and hit a decoding error; treat the byte as another
      ;; encoding error
      (encoding-failure)]
     [else
      (define b (bytes-ref in-bstr i))
      (utf-8-decode-byte/inline b accum remaining
                                complete
                                (lambda (accum remaining)
                                  (loop (fx+ i 1) j i accum remaining))
                                (lambda (accum remaining)
                                  (loop (fx+ i 1) j base-i accum remaining))
                                encoding-failure)])))

(define-syntax-rule (utf-8-decode-byte/inline b accum remaining
                                              complete-k
                                              init-continue-k
                                              next-continue-k
                                              error-k)
  (cond
    [(b . fx< . 128)
     (cond
       [(fx= remaining 0)
        ;; Found ASCII
        (complete-k b)]
       [else
        ;; We were accumulating bytes for an encoding, and
        ;; the encoding didn't complete
        (error-k)])]
    [else
     ;; Encoding...
     (cond
       [(fx= #b10000000 (fxand b #b11000000))
        ;; A continuation byte
        (cond
          [(fx= remaining 0)
           ;; We weren't continuing
           (error-k)]
          [else
           (define next (fxand b #b00111111))
           (define next-accum (fxior (fxlshift accum 6) next))
           (cond
             [(fx= 1 remaining)
              (cond
                [(and (next-accum . fx> . 127)
                      (next-accum . fx<= . #x10FFFF)
                      (not (and (next-accum . fx>= . #xD800)
                                (next-accum . fx<= . #xDFFF))))
                 (complete-k next-accum)]
                [else
                 ;; Not a valid character
                 (error-k)])]
             [(and (fx= 2 remaining)
                   (next-accum . fx<= . #b11111))
              ;; A shorter byte sequence would work, so this is an
              ;; encoding mistae.
              (error-k)]
             [(and (fx= 3 remaining)
                   (next-accum . fx<= . #b1111))
              ;; A shorter byte sequence would work
              (error-k)]
             [else
              ;; Continue an encoding
              (next-continue-k next-accum (fx- remaining 1))])])]
       [(not (fx= remaining 0))
        ;; Trying to start a new encoding while one is in
        ;; progress
        (error-k)]
       [(fx= #b11000000 (fxand b #b11100000))
        ;; Start a two-byte encoding
        (define accum (fxand b #b11111))
        ;; If `accum` is zero, that's an encoding mistake,
        ;; because a shorter byte sequence would work.
        (cond
          [(fx= accum 0) (error-k)]
          [else (init-continue-k accum 1)])]
       [(fx= #b11100000 (fxand b #b11110000))
        ;; Start a three-byte encoding
        (define accum (fxand b #b1111))
        (init-continue-k accum 2)]
       [(fx= #b11110000 (fxand b #b11111000))
        ;; Start a four-byte encoding
        (define accum (fxand b #b111))
        (init-continue-k accum 3)]
       [else
        ;; Five- or six-byte encodings don't produce valid
        ;; characters
        (error-k)])]))

;; Takes a byte and a decoding state and returns
;; one of
;;   - (values code-point 0 'complete)
;;   - (values #f 0 'error)
;;   - (values new-accum new-remaining 'continues)
(define (utf-8-decode-byte b accum remaining)
  (utf-8-decode-byte/inline b accum remaining
                            (lambda (accum)
                              (values accum 0 'complete))
                            (lambda (accum remaining)
                              (values accum remaining 'continues))
                            (lambda (accum remaining)
                              (values accum remaining 'continues))
                            (lambda ()
                              (values #f 0 'error))))

;; ----------------------------------------

(define (a-bytes->string/utf-8 bstr start end [err-char #\uFFFD] #:just-length? [just-length? #f])
  ;; Shortcut for all-ASCII:
  (cond
    [(for/and ([i (in-range start end)])
       ((bytes-ref bstr i) . fx< . 128))
     (cond
       [just-length? (fx- end start)]
       [else
        (define str (make-string (fx- end start)))
        (for ([i (in-range start end)])
          (string-set! str (fx- i start) (integer->char (bytes-ref bstr i))))
        str])]
    [else
     ;; Measure result string:
     (define-values (used-bytes got-chars state)
       (utf-8-decode! bstr start end
                      #f 0 #f 
                      #:error-char err-char
                      #:abort-mode 'error))
     (cond
       [(eq? state 'error) #f]
       [just-length? got-chars]
       [else
        ;; Create result string:
        (define str (make-string got-chars))
        (utf-8-decode! bstr start end
                       str 0 #f
                       #:error-char err-char
                       #:abort-mode 'error)
        str])]))
