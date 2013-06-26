#lang racket/base
;; owned by ryanc
(require racket/contract/base
         racket/promise
         racket/dict
         data/splay-tree)

;; Interval-maps support only half-open exact-integer intervals.

;; An interval-map is (interval-map adj-splay-tree)
;; splay-tree maps Start => (cons End-Start Value)
;; Invariant: intervals are disjoint (but the end of one interval
;; can be the same as the start of the next, since half-open).

(define (interval-map-ref im key [default (interval-map-error key)])
  (let* ([s (interval-map-s im)]
         [istart (splay-tree-iterate-greatest/<=? s key)])
    (cond [istart
           (let ([istartkey (splay-tree-iterate-key s istart)]
                 [istartvalue (splay-tree-iterate-value s istart)])
             (if (< (- key istartkey) (car istartvalue))
                 (cdr istartvalue)
                 (if (procedure? default) (default) default)))]
          [else
           (if (procedure? default) (default) default)])))

(define ((interval-map-error x))
  (error 'interval-map-ref "no mapping found for: ~e" x))

;; (POST x) =
;;   (if (start <= x < end)
;;       (updater (PRE x default))
;;       (PRE x))
(define (interval-map-update*! im start end updater
                               [default (error-for 'interval-map-update*!)])
  (define updated-defaultp
    (delay (updater (if (procedure? default) (default) default))))
  (let ([s (interval-map-s im)])
    (check-interval start end 'interval-map-update*!)
    (split! s start)
    (split! s end)
    ;; Interval ix needs updating iff start <= key(ix) < end
    ;; (Also need to insert missing intervals)
    ;; Main loop:
    (let loop ([start start] [ix (splay-tree-iterate-least/>=? s start)])
      (let ([ixstart (and ix (splay-tree-iterate-key s ix))])
        (cond [(and ix (< ixstart end))
               ;; First do leading gap, [ start, key(ix) )
               (when (< start ixstart)
                 (splay-tree-set! s start
                                 (cons (- ixstart start)
                                       (force updated-defaultp))))
               ;; Then interval, [ ixstart, end(ix) )
               (let ([ixvalue (splay-tree-iterate-value s ix)])
                 (splay-tree-set! s ixstart
                                  (cons (car ixvalue)
                                        (updater (cdr ixvalue))))
                 (loop (+ ixstart (car ixvalue)) (splay-tree-iterate-next s ix)))]
              [else
               ;; Do gap, [ start, end )
               (when (< start end)
                 (splay-tree-set! s start
                                 (cons (- end start)
                                       (force updated-defaultp))))])))))

(define (interval-map-cons*! im start end obj [default null])
  (check-interval start end 'interval-map-cons*!)
  (interval-map-update*! im start end (lambda (old) (cons obj old)) default))

(define ((error-for who))
  (error who "no mapping found"))

;; (POST x) = (if (start <= x < end) value (PRE x))
(define (interval-map-set! im start end value)
  (check-interval start end 'interval-map-set!)
  (interval-map-remove! im start end)
  (interval-map-update*! im start end (lambda (old) value) #f))

(define (interval-map-remove! im start end)
  (let ([s (interval-map-s im)])
    (check-interval start end 'interval-map-remove!)
    (let ([start (norm s start 0)]
          [end (norm s end 1)])
      (when (and start end) ;; ie, s not empty
        (split! s start)
        (split! s end)
        (splay-tree-remove-range! s start end)))))

(define (interval-map-contract! im from to)
  (check-interval from to 'interval-map-contract!)
  (interval-map-remove! im from to)
  (let* ([s (interval-map-s im)])
    (splay-tree-contract! s from to)))

(define (interval-map-expand! im from to)
  (check-interval from to 'interval-map-expand!)
  (let* ([s (interval-map-s im)])
    (split! s from)
    (splay-tree-expand! s from to)))

(define (norm s pos adjust)
  (cond [(= pos -inf.0)
         (let ([iter (splay-tree-iterate-least s)])
           (and iter (splay-tree-iterate-key s iter)))]
        [(= pos +inf.0)
         (let ([iter (splay-tree-iterate-greatest s)])
           ;; add 1 to *include* max (recall, half-open intervals)
           (and iter (+ 1 (splay-tree-iterate-key s iter))))]
        [else pos]))

;; split! 
;; Ensures that if an interval contains x, it starts at x
(define (split! s x)
  (let* ([ix (splay-tree-iterate-greatest/<? s x)]
         [ixstart (and ix (splay-tree-iterate-key s ix))])
    ;; (ix = #f) or (key(ix) < x)
    (cond [(eq? ix #f)
           ;; x <= all existing intervals; that is, either
           ;;   1) x starts its own interval (=), or
           ;;   2) x < all existing intervals (<)
           ;; Either way, nothing to split.
           (void)]
          [else
           (let* ([ixvalue (splay-tree-iterate-value s ix)]
                  [ixrun (car ixvalue)])
             (cond [(< x (+ ixstart ixrun))
                    ;; Split; adjust ix to end at x, insert [x, ixend)
                    (splay-tree-set! s ixstart (cons (- x ixstart) (cdr ixvalue)))
                    (splay-tree-set! s x (cons (- (+ ixstart ixrun) x) (cdr ixvalue)))]
                   [else
                    ;; x not in ix
                    (void)]))])))

(define (check-interval start end who)
  (unless (< start end)
    (error who "bad interval: start ~e not less than end ~e" start end)))

;; Iteration

(struct interval-map-iter (si))

(define (interval-map-iterate-first im)
  (cond [(splay-tree-iterate-first (interval-map-s im))
         => interval-map-iter]
        [else #f]))

(define (interval-map-iterate-next im iter)
  (cond [(splay-tree-iterate-next (interval-map-s im)
                                 (interval-map-iter-si iter))
         => interval-map-iter]
        [else #f]))

(define (interval-map-iterate-key im iter)
  (let ([s (interval-map-s im)]
        [is (interval-map-iter-si iter)])
    (let ([key (splay-tree-iterate-key s is)])
      (cons key (+ key (car (splay-tree-iterate-value s is)))))))

(define (interval-map-iterate-value im iter)
  (let ([s (interval-map-s im)]
        [is (interval-map-iter-si iter)])
    (cdr (splay-tree-iterate-value s is))))

;; ============================================================

;; Interval map

(define dict-methods
  (vector-immutable interval-map-ref
                    #f ;; set!
                    #f ;; set
                    #f ;; remove!
                    #f ;; remove
                    (lambda (im) (error 'interval-map-count "not supported"))
                    interval-map-iterate-first
                    interval-map-iterate-next
                    interval-map-iterate-key
                    interval-map-iterate-value))

;; Can't use prop:dict/contract, because we don't really
;; follow the dict interface!

(struct interval-map (s)
        #:property prop:dict dict-methods)

(struct interval-map* interval-map (key-c value-c)
        #:property prop:dict dict-methods)

(define (make-interval-map #:key-contract [key-contract any/c]
                           #:value-contract [value-contract any/c])
  (cond [(and (eq? key-contract any/c) (eq? value-contract any/c))
         (interval-map (make-adjustable-splay-tree))]
        [else
         (interval-map* (make-adjustable-splay-tree) key-contract value-contract)]))

;; ============================================================

(define (key-c im)
  (cond [(interval-map*? im)
         (let ([c (interval-map*-key-c im)])
           (if (eq? c any/c) exact-integer? (and/c exact-integer? c)))]
        [else exact-integer?]))
(define (val-c im)
  (cond [(interval-map*? im)
         (interval-map*-value-c im)]
        [else any/c]))

(provide/contract
 [make-interval-map
  (->* ()
       (#:key-contract contract? #:value-contract contract?)
       interval-map?)]
 [interval-map?
  (-> any/c boolean?)]
 [interval-map-ref
  (->i ([im interval-map?] [k (im) (key-c im)]) ([d any/c]) any)]
 [interval-map-set!
  (->i ([im interval-map?]
        [start (im) (key-c im)]
        [end (im) (key-c im)]
        [v (im) (val-c im)])
       [_r void?])]
 [interval-map-update*!
  (->i ([im interval-map?]
        [start (im) (key-c im)]
        [end (im) (key-c im)]
        [f (im) (-> (val-c im) (val-c im))])
       ([default any/c]) ;; imprecise
       [_r void?])]
 [interval-map-cons*!
  (->i ([im interval-map?]
        [start (im) (key-c im)]
        [end (im) (key-c im)]
        [v any/c]) ;; imprecise
       ([d any/c]) ;; imprecise
       [_r void?])]
 [interval-map-remove!
  (->i ([im interval-map?]
        [start (im) (or/c -inf.0 (key-c im))]
        [end (im) (or/c +inf.0 (key-c im))])
       [_r void?])]
 [interval-map-contract!
  (->i ([im interval-map?]
        [start (im) (key-c im)]
        [end (im) (key-c im)])
       [_r void?])]
 [interval-map-expand!
  (->i ([im interval-map?]
        [start (im) (key-c im)]
        [end (im) (key-c im)])
       [_r void?])]

 [interval-map-iterate-first
  (-> interval-map?
      (or/c interval-map-iter? #f))]
 [interval-map-iterate-next
  (-> interval-map? interval-map-iter?
      (or/c interval-map-iter? #f))]
 [interval-map-iterate-key
  (->i ([im interval-map?] [i interval-map-iter?])
       [_r (im) (let ([k (key-c im)]) (cons/c k k))])]
 [interval-map-iterate-value
  (->i ([im interval-map?] [i interval-map-iter?])
       [_r (im) (val-c im)])]

 [interval-map-iter?
  (-> any/c boolean?)])

#|
;; Testing
(define (dump im)
  (dict-map (interval-map-s im) list))

(define im (make-interval-map* = <))
(interval-map-set! im 1 3 '(a))
(interval-map-set! im 4 7 '(b))
(dump im)
;;(interval-map-remove! im 2 5)
(interval-map-cons*! im 2 5 'c null)
(dump im)
|#

#|
(define sim (make-interval-map* string=? string<?))
(interval-map-set! sim "apple" "orange" 'fruit)
(interval-map-set! sim "banana" "guava" 'tropical-fruit)
(dump sim)
|#
