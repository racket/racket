#lang racket/base
(require "../common/range.rkt"
         "lazy-bytes.rkt"
         "utf-8.rkt")

;; An AST is converted to a pile of matcher closures by "compile.rkt".

;; See "interp.rkt" for the matcher protocol.

(provide done-m
         continue-m
         limit-m
         
         byte-tail-matcher
         byte-matcher
         byte-matcher*
         
         bytes-tail-matcher
         bytes-matcher
         bytes-matcher*
         
         never-matcher
         
         any-tail-matcher
         any-matcher
         any-matcher*
         
         range-tail-matcher
         range-matcher
         range-matcher*
         
         start-matcher
         end-matcher
         line-start-matcher
         line-end-matcher
         word-boundary-matcher
         not-word-boundary-matcher
         
         alts-matcher
         
         repeat-matcher
         repeat-simple-many-matcher
         repeat-simple-matcher
         lazy-repeat-matcher
         lazy-repeat-simple-matcher

         group-push-matcher
         group-set-matcher
          
         reference-matcher
         reference-matcher/case-insensitive
         
         cut-matcher
         conditional/reference-matcher
         conditional/look-matcher
         lookahead-matcher
         lookbehind-matcher
         
         unicode-categories-matcher)

;; ----------------------------------------

(define done-m (lambda (s pos start limit end state stack)
                 pos))
(define continue-m (lambda (s pos start limit end state stack)
                     ((car stack) pos)))
(define limit-m (lambda (s pos start limit end state stack)
                  (= pos limit)))


;; ----------------------------------------

(define-syntax-rule (define-general+tail (general-matcher tail-matcher arg ... next-m)
                      (lambda (s pos start limit end)
                        tst
                        next-pos))
  (begin
    ;; General mode when `next-m` is not just `done-m`:
    (define (general-matcher arg ... next-m)
      (lambda (s pos start limit end state stack)
        (and tst
             (next-m s next-pos start limit end state stack))))
    ;; Tail mode when `next-m` is `done-m`:
    (define (tail-matcher arg ...)
      (lambda (s pos start limit end state stack)
        (and tst
             next-pos)))))

;; An iterator performs a single match as many times as possible, up
;; to a specified max number of times, and it returns the position
;; and the number of items; this mode is used only when each match
;; has a fixed size
(define-syntax-rule (define-iterate (op-matcher* arg ...)
                      outer-defn ... 
                      (lambda (s pos2 start limit end state)
                        inner-defn ...
                        #:size size
                        #:s-test s-tst
                        #:ls-test ls-tst))
  (define (op-matcher* arg ... max)
    outer-defn ...
    (lambda (s pos start limit end state)
      inner-defn ...
      (if (bytes? s)
          (let ([limit (if max
                           (min limit (+ pos (* size max)))
                           limit)])
            (let loop ([pos2 pos] [n 0])
              (define pos3 (+ pos2 size))
              (cond
               [(or (pos3 . > . limit)
                    (not s-tst))
                (values pos2 n size)]
               [else (loop pos3 (add1 n))])))
          (let ([limit (and max (+ pos (* size max)))])
            (let loop ([pos2 pos] [n 0])
              (cond
               [(or (and limit ((+ pos2 size) . > . limit))
                    (not (lazy-bytes-before-end? s (+ pos2 (sub1 size)) limit))
                    (not ls-tst))
                (values pos2 n size)]
               [else
                (loop (+ pos2 size) (add1 n))])))))))

;; When a simple repeat argument is wrapped as a group, `add-repeated-group`
;; is used in the repeating loop to set the group to the last span produced
;; by an iterator
(define-syntax-rule (add-repeated-group group-n-expr state-expr pos-expr n-expr back-amt
                                        group-revert ; bound to an unwind thunk
                                        body ...) ; duplicated in two `cond` branches
  (let ([group-n group-n-expr]
        [state state-expr]
        [n n-expr]
        [pos pos-expr])
    (cond
     [(and group-n state)
      (define old-span (vector-ref state group-n))
      (vector-set! state group-n (if (zero? n)
                                     #f
                                     (cons (- pos back-amt) pos)))
      (define (group-revert) (vector-set! state group-n old-span))
      body ...]
     [else
      (define (group-revert) (void))
      body ...])))

;; ----------------------------------------
;; Single-byte matching

(define-general+tail (byte-matcher byte-tail-matcher b next-m)
  (lambda (s pos start limit end)
    (if (bytes? s)
        (and (pos . < . limit)
             (= b (bytes-ref s pos)))
        (and (lazy-bytes-before-end? s pos limit)
             (= b (lazy-bytes-ref s pos))))
    (add1 pos)))

(define-iterate (byte-matcher* b)
  (lambda (s pos start limit end state)
    #:size 1
    #:s-test (= b (bytes-ref s pos))
    #:ls-test (= b (lazy-bytes-ref s pos))))

;; ----------------------------------------
;; Byte-string matching

(define-general+tail (bytes-matcher bytes-tail-matcher bstr len next-m)
  (lambda (s pos start limit end)
    (if (bytes? s)
        (and ((+ pos len) . <= . limit)
             (for/and ([c1 (in-bytes bstr 0 len)]
                       [c2 (in-bytes s pos (+ pos len))])
               (= c1 c2)))
        (and (lazy-bytes-before-end? s (sub1 (+ pos len)) limit)
             (for/and ([c1 (in-bytes bstr 0 len)]
                       [i (in-naturals pos)])
               (define c2 (lazy-bytes-ref s i))
               (= c1 c2))))
    (+ pos len)))

(define-iterate (bytes-matcher* bstr)
  (define len (bytes-length bstr))
  (lambda (s pos start limit end state)
    #:size len
    #:s-test (for/and ([c1 (in-bytes bstr 0 len)]
                       [c2 (in-bytes s pos (+ pos len))])
               (= c1 c2))
    #:ls-test (for/and ([c1 (in-bytes bstr 0 len)]
                        [i (in-naturals pos)])
                (define c2 (lazy-bytes-ref s i))
                (= c1 c2))))

;; ----------------------------------------
;; An always-fail pattern

(define (never-matcher)
  (lambda (s pos start limit end state stack)
    #f))

;; ----------------------------------------
;; Match any byte

(define-general+tail (any-matcher any-tail-matcher next-m)
  (lambda (s pos start limit end)
    (if (bytes? s)
        (pos . < . limit)
        (lazy-bytes-before-end? s pos limit))
    (add1 pos)))

(define (any-matcher* max-repeat)
  (lambda (s pos start limit end state)
    (cond
     [(bytes? s)
      (define n (if max-repeat
                    (min max-repeat (- limit pos))
                    (- limit pos)))
      (values (+ pos n) n 1)]
     [else
      ;; Search for end position
      (let grow-loop ([size 1])
        (define n (if max-repeat (min size max-repeat) size))
        (define pos2 (+ pos n))
        (cond
         [(and (lazy-bytes-before-end? s (sub1 pos2) limit)
               (or (not max-repeat) (n . < . max-repeat)))
          (grow-loop (* size 2))]
         [else
          (let search-loop ([min pos] [too-high (add1 pos2)])
            (define mid (quotient (+ min too-high) 2))
            (cond
             [(= mid min)
              (values mid (- mid pos) 1)]
             [(lazy-bytes-before-end? s (sub1 mid) limit)
              (search-loop mid too-high)]
             [else
              (search-loop min mid)]))]))])))

;; ----------------------------------------
;; Match any byte in a set

(define-general+tail (range-matcher range-tail-matcher rng next-m)
  (lambda (s pos start limit end)
    (if (bytes? s)
        (and (pos . < . limit)
             (rng-in? rng (bytes-ref s pos)))
        (and (lazy-bytes-before-end? s pos limit)
             (rng-in? rng (lazy-bytes-ref s pos))))
    (add1 pos)))

(define-iterate (range-matcher* rng)
  (lambda (s pos start limit end state)
    #:size 1
    #:s-test (rng-in? rng (bytes-ref s pos))
    #:ls-test (rng-in? rng (lazy-bytes-ref s pos))))

;; ----------------------------------------
;; Matches that don't consume any characters,
;; such as end-of-string or word-boundary

(define-syntax-rule (define-zero-width (op-matcher arg ... next-m)
                      (lambda (s pos start limit end)
                        tst))
  (define (op-matcher arg ... next-m)
    (lambda (s pos start limit end state stack)
      (and tst
           (next-m s pos start limit end state stack)))))

(define-zero-width (start-matcher next-m)
  (lambda (s pos start limit end)
    (= pos start)))

(define-zero-width (end-matcher next-m)
  (lambda (s pos start limit end)
    (if (bytes? s)
        (= pos end)
        (not (lazy-bytes-before-end? s pos end)))))

(define-zero-width (line-start-matcher next-m)
  (lambda (s pos start limit end)
    (or (= pos start)
        (= (char->integer #\newline)
           (if (bytes? s)
               (bytes-ref s (sub1 pos))
               (lazy-bytes-ref s (sub1 pos)))))))

(define-zero-width (line-end-matcher next-m)
  (lambda (s pos start limit end)
    (if (bytes? s)
        (or (= pos end)
            (= (char->integer #\newline) (bytes-ref s pos)))
        (or (not (lazy-bytes-before-end? s pos end))
            (= (char->integer #\newline)
               (lazy-bytes-ref s pos))))))

(define-zero-width (word-boundary-matcher next-m)
  (lambda (s pos start limit end)
    (word-boundary? s pos start limit end)))

(define-zero-width (not-word-boundary-matcher next-m)
  (lambda (s pos start limit end)
    (not (word-boundary? s pos start limit end))))

(define (word-boundary? s pos start limit end)
  (not (eq? (or (= pos start)
                (not (word-byte? (if (bytes? s)
                                     (bytes-ref s (sub1 pos))
                                     (lazy-bytes-ref s (sub1 pos))))))
            (or (if (bytes? s)
                    (= pos end)
                    (not (lazy-bytes-before-end? s pos end)))
                (not (word-byte? (if (bytes? s)
                                     (bytes-ref s pos)
                                     (lazy-bytes-ref s pos))))))))

(define (word-byte? c)
  (or (and (c . >= . (char->integer #\0)) (c . <= . (char->integer #\9)))
      (and (c . >= . (char->integer #\a)) (c . <= . (char->integer #\z)))
      (and (c . >= . (char->integer #\A)) (c . <= . (char->integer #\Z)))
      (= c (char->integer #\_))))

;; ----------------------------------------
;; Alternatives

(define (alts-matcher m1 m2)
  (lambda (s pos start limit end state stack)
    (or (m1 s pos start limit end state stack)
        (m2 s pos start limit end state stack))))

;; ----------------------------------------
;; Repeats, greedy (normal) and non-greedy,
;; in various optimized forms

(define (repeat-matcher r-m min max next-m)
  ;; The tail of `r-m` is set to `continue-m` instead
  ;; of `done-m`, so we can supply a success continuation
  ;; by pushing it onto the stack
  (lambda (s pos start limit end state stack)
    (let rloop ([pos pos] [n 0])
      (cond
       [(n . < . min)
        (define new-stack (cons (lambda (pos)
                                  (rloop pos (add1 n)))
                                stack))
        (r-m s pos start limit end state new-stack)]
       [(and max (= n max)) (next-m s pos start limit end state stack)]
       [else
        (define new-stack (cons (lambda (pos)
                                  (rloop pos (add1 n)))
                                stack))
        (or (r-m s pos start limit end state new-stack)
            (next-m s pos start limit end state stack))]))))

(define r-stack (list (lambda (pos) pos)))

(define (repeat-simple-matcher r-m min max group-n next-m)
  ;; The `r-m` matcher doesn't need backtracking, so
  ;; we don't need to push a success continuation onto
  ;; the stack
  (lambda (s pos start limit end state stack)
    (let rloop ([pos pos] [n 0] [back-amt 0])
      (define pos2
        (and (or (not max) (n . < . max))
             (r-m s pos start limit end state r-stack)))
      (if pos2
          (rloop pos2 (add1 n) (- pos2 pos))
          (let bloop ([pos pos] [n n])
            (cond
             [(n . < . min) #f]
             [else
              (add-repeated-group
               group-n state pos n back-amt group-revert
               (or (next-m s pos start limit end state stack)
                   (begin
                     (group-revert)
                     (bloop (- pos back-amt) (sub1 n)))))]))))))

(define (repeat-simple-many-matcher r-m* min max group-n next-m)
  ;; Instead of `r-m`, we have a `r-m*` that finds as many matches as
  ;; possible (up to max) in one go
  (lambda (s pos start limit end state stack)
    (define-values (pos2 n back-amt) (r-m* s pos start limit end state))
    (let bloop ([pos pos2] [n n])
      (cond
       [(n . < . min) #f]
       [else
        (add-repeated-group
         group-n state pos n back-amt group-revert
         (or (next-m s pos start limit end state stack)
             (begin
               (group-revert)
               (bloop (- pos back-amt) (sub1 n)))))]))))

(define (lazy-repeat-matcher r-m min max next-m)
  ;; Like `repeat-matcher`: the tail of `r-m` is set to `continue-m`
  (lambda (s pos start limit end state stack)
    (let rloop ([pos pos] [n 0] [min min])
      (cond
       [(n . < . min)
        (define new-stack (cons (lambda (pos)
                                  (rloop pos (add1 n) min))
                                stack))
        (r-m s pos start limit end state new-stack)]
       [(and max (= n max))
        (next-m s pos start limit end state stack)]
       [else
        (or (next-m s pos start limit end state stack)
            (rloop pos n (add1 min)))]))))

(define (lazy-repeat-simple-matcher r-m min max next-m)
  ;; Like `repeat-simple-matcher`: no backtracking in `r-m`
  (lambda (s pos start limit end state stack)
    (let rloop ([pos pos] [n 0] [min min])
      (cond
       [(n . < . min)
        (define pos2 (r-m s pos start limit end state stack))
        (and pos2
             (rloop pos2 (add1 n) min))]
       [(and max (= n max))
        (next-m s pos start limit end state stack)]
       [else
        (or (next-m s pos start limit end state stack)
            (rloop pos n (add1 min)))]))))

;; ----------------------------------------
;; Recording and referencing group matches

(define (group-push-matcher n next-m)
  (lambda (s pos start limit end state stack)
    (define new-stack (cons (cons pos (and state (vector-ref state n)))
                            stack))
    (next-m s pos start limit end state new-stack)))

(define (group-set-matcher n next-m)
  (lambda (s pos start limit end state stack)
    (define old-pos+span (car stack))
    (define old-span (cdr old-pos+span))
    (when state
      (vector-set! state n (cons (car old-pos+span) pos)))
    (or (next-m s pos start limit end state (cdr stack))
        (begin
          (when state (vector-set! state n old-span))
          #f))))

(define-syntax-rule (define-reference-matcher reference-matcher chyte=?)
  (define (reference-matcher n next-m)
    (lambda (s pos start limit end state stack)
      (define p (vector-ref state n))
      (cond
       [(not p) #f]
       [else
        (define len (- (cdr p) (car p)))
        (define matches?
          (if (bytes? s)
              (and ((+ pos len) . <= . limit)
                   (for/and ([c1 (in-bytes s (car p) (cdr p))]
                             [c2 (in-bytes s pos (+ pos len))])
                     (chyte=? c1 c2)))
              (and (lazy-bytes-before-end? s (sub1 (+ pos len)) limit)
                   (for/and ([j (in-range (car p) (cdr p))]
                             [i (in-naturals pos)])
                     (define c1 (lazy-bytes-ref s j))
                     (define c2 (lazy-bytes-ref s i))
                     (chyte=? c1 c2)))))
        (and matches?
             (next-m s (+ pos len) start limit end state stack))]))))

(define-reference-matcher reference-matcher =)

(define-reference-matcher reference-matcher/case-insensitive
  (lambda (c1 c2) (= (chyte-to-lower c1) (chyte-to-lower c2))))

(define (chyte-to-lower c)
  (if (and (c . >= . (char->integer #\A)) (c . <= . (char->integer #\Z)))
      (+ c (- (char->integer #\a) (char->integer #\A)))
      c))

;; ----------------------------------------
;; Lookahead, lookbehind, conditionals, and cut

(define (lookahead-matcher match? sub-m n-start num-n next-m)
  (lambda (s pos start limit end state stack)
    (define old-state (save-groups state n-start num-n))
    (define pos2 (sub-m s pos start limit end state null))
    (cond
     [match?
      (and pos2
           (or (next-m s pos start limit end state stack)
               (restore-groups state old-state n-start num-n)))]
     [pos2
      (restore-groups state old-state n-start num-n)]
     [else
      (next-m s pos start limit end state stack)])))

(define (lookbehind-matcher match? lb-min lb-max sub-m n-start num-n next-m)
  (lambda (s pos start limit end state stack)
    (define lb-min-pos (max start (- pos lb-max)))
    (let loop ([lb-pos (- pos lb-min)])
      (cond
       [(lb-pos . < . lb-min-pos)
        (if match?
            #f
            (next-m s pos start limit end state stack))]
       [else
        (define old-state (save-groups state n-start num-n))
        (define pos2 (sub-m s lb-pos start pos end state null))
        (cond
         [match?
          (if pos2
              (or (next-m s pos start limit end state stack)
                  (restore-groups state old-state n-start num-n))
              (loop (sub1 lb-pos)))]
         [pos2
          (restore-groups state old-state n-start num-n)]
         [else
          (next-m s pos start limit end state stack)])]))))

(define (conditional/reference-matcher n m1 m2)
  (lambda (s pos start limit end state stack)
    (if (vector-ref state n)
        (m1 s pos start limit end state stack)
        (m2 s pos start limit end state stack))))

(define (conditional/look-matcher tst-m m1 m2 n-start num-n)
  (lambda (s pos start limit end state stack)
    (define old-state (save-groups state n-start num-n))
    (or (if (tst-m s pos start limit end state null)
            (m1 s pos start limit end state stack)
            (m2 s pos start limit end state stack))
        (restore-groups state old-state n-start num-n))))

(define (cut-matcher sub-m n-start num-n next-m)
  (lambda (s pos start limit end state stack)
    (define old-state (save-groups state n-start num-n))
    (define pos2 (sub-m s pos start limit end state null))
    (and pos2
         (or (next-m s pos2 start limit end state stack)
             (restore-groups state old-state n-start num-n)))))


(define (save-groups state n-start num-n)
  (cond
   [(zero? num-n) #f]
   [(not state) #f]
   [else
    (define vec (make-vector num-n))
    (vector-copy! vec 0 state n-start (+ n-start num-n))
    vec]))

(define (restore-groups state old-state n-start num-n)
  (when old-state
    (vector-copy! state n-start old-state))
  #f)

;; ----------------------------------------
;; Unicode characters in UTF-8 encoding

(define (unicode-categories-matcher cats match? next-m)
  (lambda (s pos start limit end state stack)
    (let loop ([pos pos] [accum null])
      (define b
        (if (bytes? s)
            (and (pos . < . limit)
                 (bytes-ref s pos))
            (and (lazy-bytes-before-end? s pos limit)
                 (lazy-bytes-ref s pos))))
      (cond
       [(not b) #f]
       [else
        (define c (bytes->char/utf-8 b accum))
        (cond
         [(char? c)
          (if (eq? match?
                   (let ([c-cat (char-general-category c)])
                     (if (list? cats)
                         (for/or ([cat (in-list cats)])
                           (eq? cat c-cat))
                         (eq? cats c-cat))))
              (next-m s (add1 pos) start limit end state stack)
              #f)]
         [(eq? c 'fail)
          #f]
         [else
          ;; c must be 'continue
          (loop (add1 pos) (cons b accum))])]))))
