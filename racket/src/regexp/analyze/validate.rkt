#lang racket/base
(require "../parse/ast.rkt"
         "../common/range.rkt"
         "../common/error.rkt")

(provide validate)

;; Returns max-lookbehind or reports an error
(define (validate rx num-groups)
  (define group-sizes #hasheqv())
  (define depends-sizes #hasheqv())
  (define must-sizes #hasheqv())
  (define (might-be-empty-error)
    (regexp-error "`*`, `+`, or `{...}` operand could be empty"))
  (define-values (min-len max-len max-lookbehind)
    (let validate ([rx rx])
      (cond
       [(eq? rx rx:never)
        (values 1 1 0)]
       [(or (eq? rx rx:any)
            (exact-integer? rx)
            (rx:range? rx))
        (values 1 1 0)]
       [(bytes? rx)
        (define len (bytes-length rx))
        (values len len 0)]
       [(or (eq? rx rx:empty)
            (eq? rx rx:end)
            (eq? rx rx:line-end))
        (values 0 0 0)]
       [(or (eq? rx rx:start)
            (eq? rx rx:line-start))
        (values 0 0 1)]
       [(or (eq? rx rx:word-boundary)
            (eq? rx rx:not-word-boundary))
        (values 0 0 1)]
       [(rx:alts? rx)
        (define-values (min1 max1 lb1) (validate (rx:alts-rx1 rx)))
        (define-values (min2 max2 lb2) (validate (rx:alts-rx2 rx)))
        (values (min min1 min2) (max max1 max2) (max lb1 lb2))]
       [(rx:sequence? rx)       
        (for/fold ([min-len 0] [max-len 0] [max-lb 0]) ([rx (in-list (rx:sequence-rxs rx))])
          (define-values (min1 max1 lb1) (validate rx))
          (values (+ min-len min1) (+ max-len max1) (max max-lb lb1)))]
       [(rx:group? rx)
        (define-values (min1 max1 lb1) (validate (rx:group-rx rx)))
        (set! group-sizes (hash-set group-sizes (rx:group-number rx) min1))
        (values min1 max1 lb1)]
       [(rx:repeat? rx)
        (define old-depends-sizes depends-sizes)
        (set! depends-sizes #hasheqv())
        (define-values (min1 max1 lb1) (validate (rx:repeat-rx rx)))
        (when (zero? min1)
          (might-be-empty-error))
        (set! must-sizes (merge-depends-sizes must-sizes depends-sizes))
        (set! depends-sizes (merge-depends-sizes old-depends-sizes depends-sizes))
        (values (* min1 (rx:repeat-min rx))
                (* max1 (rx:repeat-max rx))
                lb1)]
       [(rx:maybe? rx)
        (define-values (min1 max1 lb1) (validate (rx:maybe-rx rx)))
        (values 0 max1 lb1)]
       [(rx:conditional? rx)
        (define-values (min0 max0 lb0) (validate (rx:conditional-tst rx)))
        (define-values (min1 max1 lb1) (validate (rx:conditional-rx1 rx)))
        (define-values (min2 max2 lb2) (validate (rx:conditional-rx2 rx)))
        (values (min min1 min2) (max max1 max2) (max lb0 lb1 lb2))]
       [(rx:lookahead? rx)
        (define-values (min1 max1 lb1) (validate (rx:lookahead-rx rx)))
        (values 0 0 lb1)]
       [(rx:lookbehind? rx)
        (define-values (min1 max1 lb1) (validate (rx:lookbehind-rx rx)))
        (when (= +inf.0 max1)
          (regexp-error "lookbehind pattern does not match a bounded length"))
        (set-rx:lookbehind-lb-min! rx min1)
        (set-rx:lookbehind-lb-max! rx max1)
        (values 0 0 (max max1 lb1))]
       [(rx:cut? rx)
        (validate (rx:cut-rx rx))]
       [(rx:reference? rx)
        (define n (rx:reference-n rx))
        (unless (n . <= . num-groups)
          (regexp-error "backreference number is larger than the highest-numbered cluster"))
        (define min-size (hash-ref group-sizes n #f))
        (cond
         [min-size
          ;; known minimum:
          (values min-size +inf.0 0)]
         [else
          ;; assume at least one, but check:
          (set! depends-sizes (hash-set depends-sizes (sub1 n) #t))
          (values 1 +inf.0 0)])]
       [(rx:unicode-categories? rx)
        (values 1 4 0)]
       [else (error 'validate "internal error: ~s" rx)])))
  (for ([n (in-hash-keys must-sizes)])
    (unless (positive? (hash-ref group-sizes n 0))
      (might-be-empty-error)))
  max-lookbehind)

(define (merge-depends-sizes ht1 ht2)
  (cond
   [(zero? (hash-count ht1)) ht2]
   [((hash-count ht2) . < . (hash-count ht1))
    (merge-depends-sizes ht2 ht1)]
   [else
    (for/fold ([ht2 ht2]) ([k (in-hash-keys ht1)])
      (hash-set ht2 k #t))]))

(define (range-utf-8-encoding-lengths range)
  (for/fold ([min1 4] [max1 0]) ([seg (in-list '((0 127 1)
                                                 (128 #x7FF 2)
                                                 (#x800 #xFFFF 3)
                                                 (#x10000 #x10FFFF 4)))])
    (if (range-overlaps? range (car seg) (cadr seg))
        (values (min min1 (caddr seg))
                (max max1 (caddr seg)))
        (values min1 max1))))
