#lang racket/base
(require "../parse/ast.rkt"
         "../common/range.rkt")

(provide get-must-string)

;; If there's something expensive in the regexp, look for a string or
;; sequence of ranges that must be in the input, which is useful as a
;; pre-check for matching. A sequence of ranges is useful for
;; detecting a case-insensitive string match.

(define (get-must-string rx)
  (and (something-expensive? rx)
       (choose (must-string rx)
               (must-range rx))))

;; A short byte string is more likely effective
;; than a long range sequence:
(define (choose bstr seq)
  (cond
   [(not seq) bstr]
   [(not bstr) (compile-range-sequence seq)]
   [((bytes-length bstr) . >= . (quotient (length seq) 2)) bstr]
   [else (compile-range-sequence seq)]))

(define (something-expensive? rx)
  (cond
   [(or (rx:alts? rx) (rx:repeat? rx)) #t]
   [(rx:maybe? rx)
    (something-expensive? (rx:maybe-rx rx))]
   [(rx:sequence? rx)
    (for/or ([rx (in-list (rx:sequence-rxs rx))])
      (something-expensive? rx))]
   [(rx:conditional? rx)
    (or (something-expensive? (rx:conditional-rx1 rx))
        (something-expensive? (rx:conditional-rx2 rx)))]
   [(rx:group? rx)
    (something-expensive? (rx:group-rx rx))]
   [(rx:cut? rx)
    (something-expensive? (rx:cut-rx rx))]
   [(rx:lookahead? rx)
    (something-expensive? (rx:lookahead-rx rx))]
   [(rx:lookbehind? rx)
    (something-expensive? (rx:lookbehind-rx rx))]
   [else #f]))

(define (must-string rx)
  (cond
   [(bytes? rx) rx]
   [(integer? rx) (bytes rx)]
   [(rx:sequence? rx)
    (for/fold ([bstr #f]) ([rx (in-list (rx:sequence-rxs rx))])
      (define bstr1 (must-string rx))
      (cond
       [(not bstr) bstr1]
       [(not bstr1) bstr]
       [((bytes-length bstr) . > . (bytes-length bstr1))
        ;; Prefer longer byte string:
        bstr]
       [else bstr1]))]
   [(rx:repeat? rx)
    (and (positive? (rx:repeat-min rx))
         (must-string (rx:repeat-rx rx)))]
   [(rx:group? rx)
    (must-string (rx:group-rx rx))]
   [(rx:cut? rx)
    (must-string (rx:cut-rx rx))]
   [(rx:lookahead? rx)
    (and (rx:lookahead-match? rx)
         (must-string (rx:lookahead-rx rx)))]
   [(rx:lookbehind? rx)
    (and (rx:lookbehind-match? rx)
         (must-string (rx:lookbehind-rx rx)))]
   [else #f]))

(define (must-range rx)
  (cond
   [(bytes? rx) (bytes->list rx)]
   [(integer? rx) (list rx)]
   [(rx:range? rx) (list (rx:range-range rx))]
   [(rx:sequence? rx)
    ;; combine consecutive strings and ranges
    (let loop ([seq null] [l (rx:sequence-rxs rx)])
      (cond
       [(null? l) (and (pair? seq) (reverse seq))]
       [(bytes? (car l))
        (loop (append (reverse (bytes->list (car l))) seq)
              (cdr l))]
       [(rx:range? (car l))
        (loop (cons (rx:range-range (car l))
                    seq)
              (cdr l))]
       [(null? seq) (loop null (cdr l))]
       [else
        (define rest-seq (loop null (cdr l)))
        (cond
         [(and rest-seq
               ((length rest-seq) . > . (length seq)))
          rest-seq]
         [else (reverse seq)])]))]
   [(rx:repeat? rx)
    (and (positive? (rx:repeat-min rx))
         (must-range (rx:repeat-rx rx)))]
   [(rx:group? rx)
    (must-range (rx:group-rx rx))]
   [(rx:cut? rx)
    (must-range (rx:cut-rx rx))]
   [(rx:lookahead? rx)
    (and (rx:lookahead-match? rx)
         (must-range (rx:lookahead-rx rx)))]
   [(rx:lookbehind? rx)
    (and (rx:lookbehind-match? rx)
         (must-range (rx:lookbehind-rx rx)))]
   [else #f]))

(define (compile-range-sequence seq)
  (for/list ([r (in-list seq)])
    (if (exact-integer? r)
        (compile-range (range-add empty-range r))
        (compile-range r))))
