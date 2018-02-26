#lang racket/base
(require "../parse/ast.rkt"
         "../common/range.rkt"
         "match.rkt")

;; Compile to a Spencer-style interpretation of a regular expression,
;; where sequences are implemented by record chaining. Backtracking
;; is implemented by a stack of of success continuations as needed.

;; Spenser's implementation in C dispatches on records, but we compile
;; to closures, instead. A function like `byte-matcher` allocates a
;; closure to implement byte matching. Matcher-creation functions
;; usually take a closure to use as the next step, so the closure tree
;; is built bottom-up.

(provide compile)

(define (compile rx)
  (let compile ([rx rx] [next-m done-m])
    (define-syntax-rule (mode-cond
                         #:tail tail
                         #:general general)
      (cond
       [(eq? next-m done-m) tail]
       [else general]))
    (cond
     [(exact-integer? rx)
      (mode-cond
       #:tail (byte-tail-matcher rx)
       #:general (byte-matcher rx next-m))]
     [(bytes? rx)
      (define len (bytes-length rx))
      (mode-cond
       #:tail (bytes-tail-matcher rx len)
       #:general (bytes-matcher rx len next-m))]
     [(eq? rx rx:empty)
      next-m]
     [(eq? rx rx:never)
      (never-matcher)]
     [(eq? rx rx:any)
      (mode-cond
       #:tail (any-tail-matcher)
       #:general (any-matcher next-m))]
     [(rx:range? rx)
      (define rng (compile-range (rx:range-range rx)))
      (mode-cond
       #:tail (range-tail-matcher rng)
       #:general (range-matcher rng next-m))]
     [(eq? rx rx:start)
      (start-matcher next-m)]
     [(eq? rx rx:end)
      (end-matcher next-m)]
     [(eq? rx rx:line-start)
      (line-start-matcher next-m)]
     [(eq? rx rx:line-end)
      (line-end-matcher next-m)]
     [(eq? rx rx:word-boundary)
      (word-boundary-matcher next-m)]
     [(eq? rx rx:not-word-boundary)
      (not-word-boundary-matcher next-m)]
     [(rx:sequence? rx)
      (define rxs (rx:sequence-rxs rx))
      (let loop ([rxs rxs])
        (cond
         [(null? rxs) next-m]
         [else
          (define rest-node (loop (cdr rxs)))
          (compile (car rxs) rest-node)]))]
     [(rx:alts? rx)
      (alts-matcher (compile (rx:alts-rx1 rx) next-m)
                    (compile (rx:alts-rx2 rx) next-m))]
     [(rx:maybe? rx)
      (if (rx:maybe-non-greedy? rx)
          (alts-matcher next-m
                        (compile (rx:maybe-rx rx) next-m))
          (alts-matcher (compile (rx:maybe-rx rx) next-m)
                        next-m))]
     [(rx:repeat? rx)
      (define actual-r-rx (rx:repeat-rx rx))
      ;; As a special case, handle in non-lazy `repeat` a group around
      ;; a simple pattern:
      (define r-rx (if (and (rx:group? actual-r-rx)
                            (not (rx:repeat-non-greedy? rx))
                            (not (needs-backtrack? (rx:group-rx actual-r-rx))))
                       (rx:group-rx actual-r-rx)
                       actual-r-rx))
      (define simple? (not (needs-backtrack? r-rx)))
      (define group-n (and simple?
                           (rx:group? actual-r-rx)
                           (rx:group-number actual-r-rx)))
      (define min (rx:repeat-min rx))
      (define max (let ([n (rx:repeat-max rx)])
                    (if (= n +inf.0) #f n)))
      (define r-m* (compile*/maybe r-rx min max))
      (cond
       [(and r-m*
             (not (rx:repeat-non-greedy? rx)))
        (repeat-simple-many-matcher r-m* min max group-n next-m)]
       [else
        (define r-m (compile r-rx (if simple? done-m continue-m)))
        (cond
         [(rx:repeat-non-greedy? rx)
          (if simple?
              (lazy-repeat-simple-matcher r-m min max next-m)
              (lazy-repeat-matcher r-m min max next-m))]
         [else
          (if simple?
              (repeat-simple-matcher r-m min max group-n next-m)
              (repeat-matcher r-m min max next-m))])])]
     [(rx:group? rx)
      (define n (rx:group-number rx))
      (define m (compile (rx:group-rx rx) (group-set-matcher n next-m)))
      (group-push-matcher n m)]
     [(rx:reference? rx)
      (define n (rx:reference-n rx))
      (cond
       [(zero? n)
        (never-matcher)]
       [(rx:reference-case-sensitive? rx)
        (reference-matcher (sub1 n) next-m)]
       [else
        (reference-matcher/case-insensitive (sub1 n) next-m)])]
     [(rx:cut? rx)
      (cut-matcher (compile (rx:cut-rx rx) done-m) 
                   (rx:cut-n-start rx)
                   (rx:cut-num-n rx)
                   next-m)]
     [(rx:conditional? rx)
      (define tst (rx:conditional-tst rx))
      (define m1 (compile (rx:conditional-rx1 rx) next-m))
      (define m2 (compile (rx:conditional-rx2 rx) next-m))
      (cond
       [(rx:reference? tst)
        (define n (sub1 (rx:reference-n tst)))
        (conditional/reference-matcher n m1 m2)]
       [else
        (conditional/look-matcher (compile tst done-m) m1 m2
                                  (rx:conditional-n-start rx)
                                  (rx:conditional-num-n rx))])]
     [(rx:lookahead? rx)
      (lookahead-matcher (rx:lookahead-match? rx)
                         (compile (rx:lookahead-rx rx) done-m)
                         (rx:lookahead-n-start rx)
                         (rx:lookahead-num-n rx)
                         next-m)]
     [(rx:lookbehind? rx)
      (lookbehind-matcher (rx:lookbehind-match? rx)
                          (rx:lookbehind-lb-min rx)
                          (rx:lookbehind-lb-max rx)
                          (compile (rx:lookbehind-rx rx) limit-m)
                          (rx:lookbehind-n-start rx)
                          (rx:lookbehind-num-n rx)
                          next-m)]
     [(rx:unicode-categories? rx)
      (unicode-categories-matcher (rx:unicode-categories-symlist rx)
                                  (rx:unicode-categories-match? rx)
                                  next-m)]
     [else (error 'compile/bt "internal error: unrecognized ~s" rx)])))

;; Compile a matcher repeater, if possible; the result is
;; the repeating matcher and the (consistent) length of each match
(define (compile*/maybe rx min max)
  (cond
   [(exact-integer? rx)
    (byte-matcher* rx max)]
   [(bytes? rx)
    (bytes-matcher* rx max)]
   [(eq? rx rx:any)
    (any-matcher* max)]
   [(rx:range? rx)
    (range-matcher* (compile-range (rx:range-range rx)) max)]
   [else
    #f]))

;; Determine the length of the prefix of `l` that needs backtracking:
(define (count-backtrack-prefix l)
  (let loop ([l l] [total 0] [non-bt 0])
    (cond
     [(null? l) (- total non-bt)]
     [(needs-backtrack? (car l))
      (loop (cdr l) (add1 total) 0)]
     [else
      (loop (cdr l) (add1 total) (add1 non-bt))])))
