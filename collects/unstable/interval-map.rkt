#lang racket/base
;; owned by ryanc
(require racket/contract
         racket/promise
         racket/dict
         unstable/skip-list)

;; NOTE-1
;; I need to be able to split intervals. So I can either have 
;; closed intervals on the integers or half-open intervals of
;; arbitrary total orders. I'm going to do half-open intervals.

;; An interval-map is (make-interval-map skip-list =? <? translate)
;; skip-list maps Start => (cons End Value)
;; Invariant: intervals are disjoint (but the end of one interval
;; can be the same as the start of the next, since half-open).

(define make-interval-map*
  (let ([make-interval-map
         (lambda (=? <? [translate #f])
           (make-interval-map (make-skip-list =? <?) =? <? translate))])
    make-interval-map))

(define (make-numeric-interval-map)
  (define (translate x y)
    (let ([offset (- y x)])
      (lambda (z) (+ z offset))))
  (make-interval-map* = < translate))

(define (interval-map-ref im key [default (interval-map-error key)])
  (let* ([s (interval-map-s im)]
         [<? (interval-map-<? im)]
         [istart (skip-list-iterate-greatest/<=? s key)])
    (cond [istart
           (let ([istartvalue (skip-list-iterate-value s istart)])
             (if (<? key (car istartvalue))
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
(define (interval-map-update*! im start end updater [default (error-for 'interval-map-update*!)])
  (define updated-defaultp
    (delay (updater (if (procedure? default) (default) default))))
  (let ([s (interval-map-s im)]
        [<? (interval-map-<? im)]
        [=? (interval-map-=? im)])
    (check-interval im start end 'interval-map-update*!)
    (split! s start <?)
    (split! s end <?)
    ;; Interval ix needs updating iff start <= key(ix) < end
    ;; (Also need to insert missing intervals)
    ;; Main loop:
    (let loop ([start start] [ix (skip-list-iterate-least/>=? s start)])
      (let ([ixstart (and ix (skip-list-iterate-key s ix))])
        (cond [(and ix (<? ixstart end))
               ;; First do leading gap, [ start, key(ix) )
               (when (<? start ixstart)
                 (skip-list-set! s start (cons ixstart (force updated-defaultp))))
               ;; Then interval, [ ixstart, end(ix) )
               (let ([ixvalue (skip-list-iterate-value s ix)])
                 (skip-list-iterate-set-value! s ix
                   (cons (car ixvalue) (updater (cdr ixvalue))))
                 (loop (car ixvalue) (skip-list-iterate-next s ix)))]
              [else
               ;; Do gap, [ start, end )
               (when (<? start end)
                 (skip-list-set! s start (cons end (force updated-defaultp))))])))))


(define (interval-map-cons*! im start end obj [default null])
  (check-interval im start end 'interval-map-cons*!)
  (interval-map-update*! im start end (lambda (old) (cons obj old)) default))

(define ((error-for who))
  (error who "no mapping found"))

;; (POST x) = (if (start <= x < end) value (PRE x))
(define (interval-map-set! im start end value)
  (check-interval im start end 'interval-map-set!)
  (interval-map-remove! im start end)
  (interval-map-update*! im start end values value))

(define (interval-map-remove! im start end)
  (let ([s (interval-map-s im)]
        [<? (interval-map-<? im)]
        [=? (interval-map-=? im)])
    (check-interval im start end 'interval-map-remove!)
    (split! s start <?)
    (split! s end <?)
    ;; Interval ix needs removing iff start <= key(ix) < end
    ;; FIXME: add batch remove to skip-lists
    (let loop ([ix (skip-list-iterate-least/>=? s start)])
      (when ix
        (let ([ixstart (skip-list-iterate-key s ix)])
          (when (<? ixstart end)
            ;; Get next before we remove current
            (let ([next (skip-list-iterate-next s ix)])
              (skip-list-remove! s ixstart)
              (loop next))))))))

;; split! 
;; Ensures that if an interval contains x, it starts at x
(define (split! s x <?)
  (let* ([ix (skip-list-iterate-greatest/<? s x)]
         [ixstart (and ix (skip-list-iterate-key s ix))])
    ;; (ix = #f) or (key(ix) < x)
    (cond [(eq? ix #f)
           ;; x <= all existing intervals; that is, either
           ;;   1) x starts its own interval (=), or
           ;;   2) x < all existing intervals (<)
           ;; Either way, nothing to split.
           (void)]
          [else
           (let* ([ixvalue (skip-list-iterate-value s ix)]
                  [ixend (car ixvalue)])
             (cond [(<? x ixend)
                    ;; Split; adjust ix to start at x, insert [ixstart, x)
                    (skip-list-iterate-set-key! s ix x)
                    (skip-list-set! s ixstart (cons x (cdr ixvalue)))]
                   [else
                    ;; x not in ix
                    (void)]))])))

(define (check-interval im start end who)
  (let ([<? (interval-map-<? im)])
    (unless (<? start end)
      (error who "bad interval: start ~e not less than end ~e" start end))))

;; Iteration

(define-struct interval-map-iter (si))

(define (interval-map-iterate-first im)
  (cond [(skip-list-iterate-first (interval-map-s im))
         => make-interval-map-iter]
        [else #f]))

(define (interval-map-iterate-next im iter)
  (cond [(skip-list-iterate-next (interval-map-s im)
                                 (interval-map-iter-si iter))
         => make-interval-map-iter]
        [else #f]))

(define (interval-map-iterate-key im iter)
  (let ([s (interval-map-s im)]
        [is (interval-map-iter-si iter)])
    (cons (skip-list-iterate-key s is)
          (car (skip-list-iterate-value s is)))))

(define (interval-map-iterate-value im iter)
  (let ([s (interval-map-s im)]
        [is (interval-map-iter-si iter)])
    (cdr (skip-list-iterate-value s is))))

;; Interval map

(define-struct interval-map (s =? <? translate)
  #:property prop:dict
             (vector interval-map-ref
                     #f ;; set!
                     #f ;; set
                     #f ;; remove!
                     #f ;; remove
                     (lambda (im) (error 'interval-map-count "not supported"))
                     interval-map-iterate-first
                     interval-map-iterate-next
                     interval-map-iterate-key
                     interval-map-iterate-value))

(define (interval-map-with-translate? x)
  (and (interval-map? x)
       (procedure? (interval-map-translate x))))

(define (interval-map-contract! im from to)
  (let ([<? (interval-map-<? im)])
    (unless (<? from to)
      (error 'interval-map-contract!
             "start ~e not less than end ~e" from to)))
  (interval-map-remove! im from to)
  (let* ([s (interval-map-s im)]
         [translate ((interval-map-translate im) to from)])
    (apply-offset! s translate to)))

(define (interval-map-expand! im from to)
  (let ([<? (interval-map-<? im)])
    (unless (<? from to)
      (error 'interval-map-expand!
             "start ~e not less than end ~e" from to))
    (let* ([s (interval-map-s im)]
           [translate ((interval-map-translate im) from to)])
      (split! s from <?)
      (apply-offset! s translate from))))

(define (apply-offset! s translate to)
  (let loop ([ix (skip-list-iterate-least/>=? s to)])
    (when ix
      (let* ([ixkey (skip-list-iterate-key s ix)]
             [ixvalue (skip-list-iterate-value s ix)])
        (skip-list-iterate-set-key! s ix (translate ixkey))
        (skip-list-iterate-set-value! s ix
          (cons (translate (car ixvalue)) (cdr ixvalue))))
      (loop (skip-list-iterate-next s ix)))))

(provide/contract
 [rename make-interval-map* make-interval-map
  (-> procedure? procedure? interval-map?)]
 [make-numeric-interval-map
  (-> interval-map-with-translate?)]
 [interval-map?
  (-> any/c any)]
 [interval-map-with-translate?
  (-> any/c any)]
 [interval-map-ref
  (->* (interval-map? any/c) (any/c) any)]
 [interval-map-set!
  (-> interval-map? any/c any/c any/c any)]
 [interval-map-update*!
  (->* (interval-map? any/c any/c (-> any/c any/c)) (any/c) any)]
 [interval-map-cons*!
  (->* (interval-map? any/c any/c any/c) (any/c) any)]
 [interval-map-remove!
  (-> interval-map? any/c any/c any)]
 [interval-map-contract!
  (-> interval-map-with-translate? any/c any/c any)]
 [interval-map-expand!
  (-> interval-map-with-translate? any/c any/c any)]
 [interval-map-iterate-first
  (-> interval-map? (or/c interval-map-iter? #f))]
 [interval-map-iterate-next
  (-> interval-map? interval-map-iter? (or/c interval-map-iter? #f))]
 [interval-map-iterate-key
  (-> interval-map? interval-map-iter? any)]
 [interval-map-iterate-value
  (-> interval-map? interval-map-iter? any)]
 [interval-map-iter?
  (-> any/c any)])

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
