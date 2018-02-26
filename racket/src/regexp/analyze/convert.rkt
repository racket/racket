#lang racket/base
(require "../parse/ast.rkt"
         "../common/range.rkt")

;; Convert a string regexp to a byte-string regexp

(provide convert)

(define (convert rx)
  (cond
   [(eq? rx rx:any)
    (rx:unicode-categories null #f)]
   [(exact-integer? rx)
    (cond
     [(< rx 128) rx]
     [else (string->bytes/utf-8 (string (integer->char rx)))])]
   [(rx:range? rx)
    (define range (rx:range-range rx))
    (if (range-within? range 0 127)
        rx
        (range->alts range))]
   [(bytes? rx) (convert (bytes->string/latin-1 rx))]
   [(string? rx) (string->bytes/utf-8 rx)]
   [(rx:alts? rx)
    (rx-alts (convert (rx:alts-rx1 rx))
             (convert (rx:alts-rx2 rx))
             255)]
   [(rx:sequence? rx)
    (struct-copy rx:sequence rx
                 [rxs (for/list ([rx (in-list (rx:sequence-rxs rx))])
                        (convert rx))])]
   [(rx:group? rx)
    (struct-copy rx:group rx
                 [rx (convert (rx:group-rx rx))])]
   [(rx:repeat? rx)
    (struct-copy rx:repeat rx
                 [rx (convert (rx:repeat-rx rx))])]
   [(rx:maybe? rx)
    (struct-copy rx:maybe rx
                 [rx (convert (rx:maybe-rx rx))])]
   [(rx:conditional? rx)
    (struct-copy rx:conditional rx
                 [tst (convert (rx:conditional-tst rx))]
                 [rx1 (convert (rx:conditional-rx1 rx))]
                 [rx2 (convert (rx:conditional-rx2 rx))])]
   [(rx:lookahead? rx)
    (struct-copy rx:lookahead rx
                 [rx (convert (rx:lookahead-rx rx))])]
   [(rx:lookbehind? rx)
    (struct-copy rx:lookbehind rx
                 [rx (convert (rx:lookbehind-rx rx))])]
   [(rx:cut? rx)
    (struct-copy rx:cut rx
                 [rx (convert (rx:cut-rx rx))])]
   [else rx]))

(define (range->alts args)
  (define l (range->list args))
  (let loop ([l l])
    (cond
     [(null? l) rx:never]
     [else
      (let ([start (caar l)]
            [end (cdar l)])
        ;; If this range spans different-sized encodings, split it up
        (define seg-end
          (cond
           [(start . <= . 127) 127]
           [(start . <= . #x7FF) #x7FF]
           [(start . <= . #xFFFF) #xFFFF]
           [(start . <= . #x1FFFFF) #x1FFFFF]))
        (cond
         [(end . > . seg-end)
          (loop (cons (cons start seg-end)
                      (cons (cons (add1 seg-end) end)
                            (cdr l))))]
         [(end . <= . 127)
          (rx-alts (rx-range (range-add-span empty-range start end) 255)
                   (loop (cdr l))
                   255)]
         [else
          (rx-alts (bytes-range (string->bytes/utf-8 (string (integer->char start)))
                                (string->bytes/utf-8 (string (integer->char end))))
                   (loop (cdr l))
                   255)]))])))

(define (bytes-range start-str end-str)
  ;; The `start-str` argument and `end-str` arguments must be the same
  ;; length.
  (cond
   [(equal? start-str end-str)
    start-str]
   [(= 1 (bytes-length start-str))
    (rx-range (range-add-span empty-range (bytes-ref start-str 0) (bytes-ref end-str 0))
              255)]
   [else
    ;; We a range that's has structly more than one value.
    ;;
    ;; At this point, the situation is much like creating a regexp to
    ;; match decimal digits. If we wanted to match the range 28 to 75
    ;; (inclusive), we'd need three parts:
    ;;
    ;;   2[8-9]|[3-6][0-9]|7[0-5]
    ;;
    ;; It gets more complex with three digits, say 
    ;; 128 to 715:
    ;; 
    ;;   12[8-9]|1[3-6][0-9]|[2-6][0-9][0-9]|7[0-0][0-9]|71[0-5]
    ;;
    ;; but you get the idea. Note that rx:any takes the place of
    ;; [0-9].
    (define common (let loop ([i 0])
                     (cond
                      [(= (bytes-ref start-str i) (bytes-ref end-str i))
                       (loop (add1 i))]
                      [else i])))

    ;; Assert: common must be less than the full string length.
    ;; Let `common-str` be the common prefix.
    (define common-str (if (zero? common)
                           #""
                           (subbytes start-str 0 common)))
    (define n (bytes-ref start-str common))
    (define m (bytes-ref end-str common))
    
    ;; Now we have something like nxxxx to mxxxx where n < m.
    ;; Find p such that p >= n and p0000 >= nxxxx, and
    ;; find q such that q0000 <= mxxxx.

    ;; If the xxxxs in nxxxx are 0, then p is n,
    ;; otherwise it's n + 1.
    (define p (if (zero-tail? start-str (add1 common))
                  n
                  (add1 n)))
    
    ;; If the xxxxs in mxxxx are 0, then q is m,
    ;; otherwise it's m - 1.
    (define q (if (zero-tail? end-str (add1 common))
                  m
                  (sub1 m)))
    
    (define tail-len (sub1 (- (bytes-length start-str) common)))
    
    ;; Fill out [nxxxx, nFFFF]
    (define n-to-p
      (rx-sequence (list n
                         (bytes-range (subbytes start-str (add1 common))
                                      (vector-ref FFFF-tails tail-len)))))
    
    ;; Fill out [m0000, mxxxx]
    (define m-and-up
      (rx-sequence (list m
                         (bytes-range (vector-ref 0000-tails tail-len)
                                      (subbytes end-str (add1 common))))))
    
    ;; Fill out [p0000,qFFFF]
    (define p-through-q
      (if (= (add1 p) q)
          rx:never
          (rx-sequence (cons
                        (rx-range (range-add-span empty-range p q) 255)
                        (for/list ([i (in-range tail-len)]) rx:any)))))

    ;; Combine the common prefix with the three filled-out ranges:
    (rx-sequence (list (if (= 1 (bytes-length common-str))
                           (bytes-ref common-str 0)
                           common-str)
                       (rx-alts n-to-p
                                (rx-alts p-through-q
                                         m-and-up
                                         255)
                                255)))]))

(define FFFF-tails '#(#"" #"\xFF" #"\xFF\xFF" #"\xFF\xFF\xFF" #"\xFF\xFF\xFF\xFF"))
(define 0000-tails '#(#"" #"\x00" #"\x00\x00" #"\x00\x00\x00" #"\x00\x00\x00\x00"))

(define (zero-tail? bstr i)
  (for/and ([c (in-bytes bstr i)])
    (= c 0)))
