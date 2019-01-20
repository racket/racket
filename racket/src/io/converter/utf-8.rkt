#lang racket/base
(require (only-in "../host/rktio.rkt"
                  RKTIO_ERROR_CONVERT_BAD_SEQUENCE
                  RKTIO_ERROR_CONVERT_PREMATURE_END
                  RKTIO_ERROR_CONVERT_NOT_ENOUGH_SPACE)
         "../string/utf-8-encode.rkt"
         "../common/set-two.rkt")

(provide utf-8-converter
         utf-8-converter?
         utf-8-convert-in)

(struct utf-8-converter (from to))

(define big-endian? (system-big-endian?))

(define (utf-8-convert-in c src src-start src-end dest dest-start dest-end)
  (define from (utf-8-converter-from c))
  (define to (utf-8-converter-to c))
  (define-values (in-consumed out-produced status)
    (if (or (eq? from 'utf-16)
            (eq? from 'utf-16-ish)
            (eq? from 'utf-16-assume))
        (utf-16-ish-reencode! src src-start src-end
                              dest dest-start dest-end
                              #:from-utf-16-ish? (eq? from 'utf-16-ish)
                              #:assume-paired-surrogates? (eq? from 'utf-16-assume))
        (utf-8-ish-reencode! src src-start src-end
                             dest dest-start dest-end
                             #:permissive? (or (eq? from 'utf-8-permissive)
                                               (eq? from 'utf-8-ish-permissive))
                             #:from-utf-8-ish? (or (eq? from 'utf-8-ish)
                                                   (eq? from 'utf-8-ish-permissive))
                             #:to-utf-16? (or (eq? to 'utf-16)
                                              (eq? to 'utf-16-ish)
                                              (eq? to 'utf-16-assume)))))
  (values in-consumed
          out-produced
          (case status
            [(error) RKTIO_ERROR_CONVERT_BAD_SEQUENCE]
            [(aborts) RKTIO_ERROR_CONVERT_PREMATURE_END]
            [(continues) RKTIO_ERROR_CONVERT_NOT_ENOUGH_SPACE]
            [else #f])))

;; Similar to `utf-8-decode` in "../string/utf-8-decode.rkt", but
;; "decodes" back to a byte string either as UTF-8 or UTF-16, and also
;; supports a "utf-8-ish" encoding that allows unpaired surrogates.
;;
;; There's a lot of similarly to the implementation of `utf-8-decode`,
;; but with enough differences to make abstraction difficult.
(define (utf-8-ish-reencode! in-bstr in-start in-end
                             out-bstr out-start out-end
                             #:permissive? permissive?
                             #:from-utf-8-ish? from-utf-8-ish?
                             #:to-utf-16? to-utf-16?)
  (let loop ([i in-start] [j out-start] [base-i in-start] [accum 0] [remaining 0])

    ;; Shared handling for encoding failures:
    (define (encoding-failure)
      (cond
        [permissive?
         ;; Try to write #\uFFFD, which is #"\357\277\275" in UTF-8
         (define (continue-after-permissive next-j)
           (define next-i (add1 base-i))
           (cond
             [(= next-j out-end)
              (values (- next-i in-start)
                      (- next-j out-start)
                      'continues)]
             [else
              (loop next-i next-j next-i 0 0)]))
         (cond
           [(and (not to-utf-16?) ((+ j 3) . <= . out-end))
            (bytes-set! out-bstr j #o357)
            (bytes-set! out-bstr (+ j 1) #o277)
            (bytes-set! out-bstr (+ j 2) #o275)
            (continue-after-permissive (+ j 3))]
           [(and to-utf-16? ((+ j 2) . <= . out-end))
            (bytes-set-two! out-bstr j #xFF #xFD)
            (continue-after-permissive (+ j 2))]
           [else
            (values (- base-i in-start)
                    (- j out-start)
                    'continues)])]
       [else
        (values (- base-i in-start)
                (- j out-start)
                'error)]))
    
    ;; Shared handling for decoding success:
    (define (continue next-j)
      (define next-i (add1 i))
      (cond
       [(= next-j out-end)
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
       [else
        (values (- base-i in-start) 
                (- j out-start)
                'aborts)])]
     [else
      (define b (bytes-ref in-bstr i))
      (cond
       [(b . < . 128)
        (cond
          [(zero? remaining)
           ;; Found ASCII
           (cond
             [(and (not to-utf-16?)
                   (j . < . out-end))
              (bytes-set! out-bstr j b)
              (continue (add1 j))]
             [((add1 j) . < . out-end)
              (bytes-set-two! out-bstr j 0 b)
              (continue (+ j 2))]
             [else
              (values (- base-i in-start) 
                      (- j out-start)
                      'continues)])]
          [else
           ;; We were accumulating bytes for an encoding, and
           ;; the encoding didn't complete
           (encoding-failure)])]
       [else
        ;; An encoding...
        (cond
          [(= #b10000000 (bitwise-and b #b11000000))
           ;; A continuation byte
          (cond
           [(zero? remaining)
            ;; We weren't continuing
            (encoding-failure)]
           [else
            (define next (bitwise-and b #b00111111))
            (define next-accum (+ (arithmetic-shift accum 6) next))
            (cond
              [(= 1 remaining)
               ;; This continuation byte finishes an encoding
               (define v next-accum)
               (define next-i (add1 i))
               (cond
                 [(v . < . 128)
                  ;; A shorter byte sequence would work
                  (encoding-failure)]
                 [(or from-utf-8-ish?
                      (not (or (v . > . #x10FFFF)
                               (and (v . >= . #xD800)
                                    (v . <= . #xDFFF)))))
                  ;; A character to write, either in UTF-16 output for UTF-8
                  (cond
                    [to-utf-16?
                     ;; Write one character in UTF-16
                     (cond
                       [(and (v . < . #x10000)
                             ((+ j 2) . <= . out-end))
                        ;; No need for a surrogate pair (so, 2 bytes)
                        (bytes-set-two! out-bstr j (arithmetic-shift v -8) (bitwise-and v #xFF))
                        (continue (+ j 2))]
                       [((+ j 4) . <= . out-end)
                        ;; Write surrogate pair (as 4 bytes)
                        (define av (- v #x10000))
                        (define hi (bitwise-ior #xD800 (bitwise-and (arithmetic-shift av -10) #x3FF)))
                        (define lo (bitwise-ior #xDC00 (bitwise-and av #x3FF)))
                        (bytes-set-two! out-bstr j (arithmetic-shift hi -8) (bitwise-and hi #xFF))
                        (bytes-set-two! out-bstr (+ j 2) (arithmetic-shift lo -8) (bitwise-and lo #xFF))
                        (continue (+ j 4))]
                       [else
                        ;; Not enought space for UTF-16 encoding
                        (values (- base-i in-start)
                                (- j out-start)
                                'continues)])]
                    [else
                     ;; From UTF-8-to-UTF-8 with no "-ish" corrections, we can just copy
                     ;; the input encoding bytes to the output bytes
                     (let loop ([from-i base-i] [to-j j])
                       (cond
                         [(= from-i next-i)
                          (continue to-j)]
                         [(= to-j out-end)
                          (values (- base-i in-start)
                                  (- j out-start)
                                  'continues)]
                         [else
                          (bytes-set! out-bstr to-j (bytes-ref in-bstr from-i))
                          (loop (add1 from-i) (add1 to-j))]))])]
                 [else
                  ;; Not a valid character --- an unpaired surrogate
                  ;; or too-large value in normal UTF-8 decoding (not UTF-8-ish)
                  (encoding-failure)])]
              [(and (= 2 remaining)
                    (next-accum . <= . #b11111))
               ;; A shorter byte sequence would work
               (encoding-failure)]
              [(and (= 3 remaining)
                    (next-accum . <= . #b1111))
               ;; A shorter byte sequence would work
               (encoding-failure)]
              ;; We could check here for 3 remaining and `next-accum`
              ;; >= #b100010000, which implies a result above #x10FFFF.
              ;; The old decoder doesn't do that, and we'll stick to the
              ;; old behavior for now
              [else
               ;; An encoding continues...
               (loop (add1 i) j base-i next-accum (sub1 remaining))])])]
          [(not (zero? remaining))
           ;; Trying to start a new encoding while one is in
           ;; progress
           (encoding-failure)]
          [(= #b11000000 (bitwise-and b #b11100000))
           ;; Start a two-byte encoding
           (define accum (bitwise-and b #b11111))
           ;; If `accum` is zero, that's an encoding mistake
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
          (cond
            [(accum . > . 4)
             ;; Will be greater than #x10FFFF
             (encoding-failure)]
            [else
             (loop (add1 i) j i  accum 3)])]
         [else
          ;; Five- or six-byte encodings don't produce valid
          ;; characters
          (encoding-failure)])])])))

;; Converts UTF-16 into UTF-8
(define (utf-16-ish-reencode! in-bstr in-start in-end
                              out-bstr out-start out-end
                              #:from-utf-16-ish? from-utf-16-ish?
                              #:assume-paired-surrogates? assume-paired-surrogates?)
  (let loop ([i in-start] [j out-start])
    (define (done status)
      (values (- i in-start)
              (- j out-start)
              status))

    (cond
      [(= i in-end)
       (done 'complete)]
      [((+ i 2) . > . in-end)
       (done 'aborts)]
      [else
       (define a (bytes-ref in-bstr i))
       (define b (bytes-ref in-bstr (add1 i)))
       (define v (if big-endian?
                     (+ (arithmetic-shift a 8) b)
                     (+ (arithmetic-shift b 8) a)))
       (define (continue v next-i)
         (define (continue next-j) (loop next-i next-j))
         (utf-8-encode-dispatch v
                                in-start i
                                out-bstr out-start out-end j
                                continue))
       (cond
         [(and (v . >= . #xD800)
               (v . <= . #xDFFF))
          (cond
            [(or assume-paired-surrogates?
                 (v . <= . #xDBFF))
             ;; Look for surrogate pair
             (cond
               [((+ i 4) . > . in-end)
                (done 'aborts)]
               [else
                (define a (bytes-ref in-bstr (+ i 2)))
                (define b (bytes-ref in-bstr (+ i 3)))
                (define v2 (if big-endian?
                               (+ (arithmetic-shift a 8) b)
                               (+ (arithmetic-shift b 8) a)))
                (cond
                  [(or assume-paired-surrogates?
                       (and (v2 . >= . #xDC00)
                            (v2 . <= . #xDFFF)))
                   (define v3 (+ #x10000
                                 (bitwise-ior (arithmetic-shift (bitwise-and v #x3FF) 10)
                                              (bitwise-and v2 #x3FF))))
                   (continue v3 (+ i 4))]
                  [from-utf-16-ish?
                   ;; continue anyway as as unpaired surrogate
                   (continue v (+ i 2))]
                  [else
                   (done 'error)])])]
            [else
             ;; unpaired surrogate
             (cond
               [from-utf-16-ish?
                ;; continue anyway
                (continue v (+ i 2))]
               [else (done 'error)])])]
         [else (continue v (+ i 2))])])))
