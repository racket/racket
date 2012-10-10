#lang racket/base

(provide vector-set*! vector-copy vector-map vector-map! vector-append
         vector-take vector-drop vector-split-at
         vector-take-right vector-drop-right vector-split-at-right
         vector-filter vector-filter-not
         vector-count vector-argmin vector-argmax
         vector-member vector-memq vector-memv)
(require racket/unsafe/ops)

(define (vector-set*! v . pairs)
  (unless (even? (length pairs))
    (error 'vector-set*! "expected an even number of association elements, but received an odd number: ~e" pairs))
  (let loop ([pairs pairs])
    (unless (null? pairs)
      (vector-set! v (car pairs) (cadr pairs))
      (loop (cddr pairs)))))

;; unchecked version of `vector-copy'
;; used at the implementation of many functions in this file
(define (vector-copy* v start end)
  (define new-v (make-vector (- end start)))
  (vector-copy! new-v 0 v start end)
  new-v)

(define (vector-copy v [start 0] [end (and (vector? v) (vector-length v))])
  (unless (vector? v)
    (raise-argument-error 'vector-copy "vector?" v))
  (unless (exact-nonnegative-integer? start)
    (raise-argument-error 'vector-copy "exact-nonnegative-integer?" start))
  (let ([len (vector-length v)])
    (cond
      [(= len 0)
       (unless (= start 0)
         (raise-range-error 'vector-copy "vector" "starting " start v 0 0))
       (unless (= end 0)
         (raise-range-error 'vector-copy "vector" "ending " end v 0 0))
       (vector)]
      [else
       (unless (and (<= 0 start len))
         (raise-range-error 'vector-copy "vector" "starting " start v 0 len))
       (unless (and (<= start end len))
         (raise-range-error 'vector-copy "vector" "ending " end v start len 0))
       (vector-copy* v start end)])))

;; do vector-map, putting the result in `target'
;; length is passed to save the computation
(define (vector-map/update f target length vs)
  (for ([i (in-range length)])
    (unsafe-vector-set!
     target i
     (apply f (map (lambda (vec) (unsafe-vector-ref vec i)) vs)))))

;; check that `v' is a vector
;; that `v' and all the `vs' have the same length
;; and that `f' takes |v + vs| args
;; uses name for error reporting
(define (varargs-check f v vs name)
  (unless (procedure? f)
    (apply raise-argument-error name "procedure?" 0 f v vs))
  (unless (vector? v)
    (apply raise-argument-error name "vector?" 1 f v vs))
  (let ([len (unsafe-vector-length v)])
    (for ([e (in-list vs)]
          [i (in-naturals 2)])
      (unless (vector? e)
        (apply raise-argument-error name "vector?" e i f v vs))
      (unless (= len (unsafe-vector-length e))
        (raise
         (make-exn:fail:contract
          (format "~e: all vectors must have same size; ~a"
                  name
                  (let ([args (list* f v vs)])
                    (if ((length args) . < . 10)
                      (apply string-append
                             "arguments were:"
                             (for/list ([i (list* f v vs)])
                               (format " ~e" i)))
                      (format "given ~a arguments total"
                              (sub1 (length args))))))
          (current-continuation-marks)))))
    (unless (procedure-arity-includes? f (add1 (length vs)))
      (raise-arguments-error name "mismatch between procedure arity and argument count"
                             "procedure" f
                             "expected arity" (add1 (length vs))))
    len))

(define (vector-map f v . vs)
  (let* ([len (varargs-check f v vs 'vector-map)]
         [new-v (make-vector len)])
    (vector-map/update f new-v len (cons v vs))
    new-v))

(define (vector-map! f v . vs)
  (define len (varargs-check f v vs 'vector-map!))
  (vector-map/update f v len (cons v vs))
  v)

;; check that `v' is a vector and that `f' takes one arg
;; uses name for error reporting
(define (one-arg-check f v name)
  (unless (and (procedure? f) (procedure-arity-includes? f 1))
    (raise-argument-error name "(any/c . -> . any/c)" 0 f)))

(define (vector-filter f v)
  (one-arg-check f v 'vector-filter)
  (list->vector (for/list ([i (in-vector v)] #:when (f i)) i)))

(define (vector-filter-not f v)
  (one-arg-check f v 'vector-filter-not)
  (list->vector (for/list ([i (in-vector v)] #:unless (f i)) i)))

(define (vector-count f v . vs)
  (unless (procedure? f)
    (raise-argument-error 'vector-count "procedure?" f))
  (unless (procedure-arity-includes? f (add1 (length vs)))
    (raise-arguments-error 'vector-count "mismatch between procedure arity and argument count"
                           "procedure" f
                           "expected arity" (add1 (length vs))))
  (unless (and (vector? v) (andmap vector? vs))
    (raise-argument-error
     'vector-count "vector?"
     (ormap (lambda (x) (and (not (list? x)) x)) (cons v vs))))
  (if (pair? vs)
    (let ([len (vector-length v)])
      (if (andmap (lambda (v) (= len (vector-length v))) vs)
        (for/fold ([c 0])
            ([i (in-range len)]
             #:when
             (apply f
                    (unsafe-vector-ref v i)
                    (map (lambda (v) (unsafe-vector-ref v i)) vs)))
          (add1 c))
        (raise-arguments-error 'vector-count "all vectors must have same size")))
    (for/fold ([cnt 0]) ([i (in-vector v)] #:when (f i))
      (add1 cnt))))

(define (check-vector/index v n name)
  (unless (vector? v)
    (raise-argument-error name "vector?" v))
  (unless (exact-nonnegative-integer? n)
    (raise-argument-error name "exact-nonnegative-integer?" n))
  (let ([len (unsafe-vector-length v)])
    (unless (<= 0 n len)
      (raise-range-error name "vector" "" n v 0 len))
    len))

(define (vector-take v n)
  (check-vector/index v n 'vector-take)
  (vector-copy* v 0 n))

(define (vector-drop v n)
  (vector-copy* v n (check-vector/index v n 'vector-drop)))

(define (vector-split-at v n)
  (let ([len (check-vector/index v n 'vector-split-at)])
    (values (vector-copy* v 0 n) (vector-copy* v n len))))

(define (vector-take-right v n)
  (let ([len (check-vector/index v n 'vector-take-right)])
    (vector-copy* v (unsafe-fx- len n) len)))

(define (vector-drop-right v n)
  (let ([len (check-vector/index v n 'vector-drop-right)])
    (vector-copy* v 0 (unsafe-fx- len n))))

(define (vector-split-at-right v n)
  (let ([len (check-vector/index v n 'vector-split-at-right)])
    (values (vector-copy* v 0 (unsafe-fx- len n))
            (vector-copy* v (unsafe-fx- len n) len))))

(define (vector-append . vs)
  (let* ([lens (for/list ([e (in-list vs)] [i (in-naturals)])
                 (if (vector? e)
                   (unsafe-vector-length e)
                   (apply raise-argument-error 'vector-append "vector?" i vs)))]
         [new-v (make-vector (apply + lens))])
    (let loop ([start 0] [lens lens] [vs vs])
      (when (pair? lens)
        (let ([len (car lens)] [v (car vs)])
          (for ([i (in-range len)])
            (unsafe-vector-set! new-v (+ i start) (unsafe-vector-ref v i)))
          (loop (+ start len) (cdr lens) (cdr vs)))))
    new-v))

;; copied from `racket/list'
(define (mk-min cmp name f xs)
  (unless (and (procedure? f)
               (procedure-arity-includes? f 1))
    (raise-argument-error name "(any/c . -> . real?)" f))
  (unless (and (vector? xs)
               (< 0 (unsafe-vector-length xs)))
    (raise-argument-error name "(and/c vector? (lambda (v) (positive? (vector-length v))))" xs))
  (let ([init-min-var (f (unsafe-vector-ref xs 0))])
    (unless (real? init-min-var)
      (raise-result-error name "real?" init-min-var))
    (if (unsafe-fx= (unsafe-vector-length xs) 1)
        (unsafe-vector-ref xs 0)
        (let-values ([(min* min-var*)
                  (for/fold ([min (unsafe-vector-ref xs 0)]
                             [min-var init-min-var])
                      ([e (in-vector xs 1)])
                    (let ([new-min (f e)])
                      (unless (real? new-min)
                        (raise-result-error
                         name "real?" new-min))
                      (cond [(cmp new-min min-var)
                             (values e new-min)]
                            [else (values min min-var)])))])
      min*))))

(define (vector-argmin f xs) (mk-min < 'vector-argmin f xs))
(define (vector-argmax f xs) (mk-min > 'vector-argmax f xs))

(define-syntax-rule (vm-mk name cmp)
  (define (name val vec)
    (unless (vector? vec)
      (raise-argument-error 'name "vector?" 1 val vec))
    (let ([sz (unsafe-vector-length vec)])
      (let loop ([k 0])
        (cond [(= k sz) #f]
              [(cmp val
                    (unsafe-vector-ref vec k))
               k]
              [else (loop (unsafe-fx+ 1 k))])))))

(vm-mk vector-member equal?)
(vm-mk vector-memq eq?)
(vm-mk vector-memv eqv?)
