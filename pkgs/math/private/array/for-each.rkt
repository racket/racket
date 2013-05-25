#lang typed/racket/base

(require racket/performance-hint
         "../unsafe.rkt"
         "utils.rkt")

(provide (all-defined-out))

(define-syntax-rule (for-each-array+data-index ds-expr f-expr)
  (let*: ([ds : Indexes  ds-expr]
          [dims : Index  (vector-length ds)])
    (define-syntax-rule (f js j)
      ((ann f-expr (Indexes Nonnegative-Fixnum -> Void)) js j))
    (cond
      [(= dims 0)  (f ds 0)]
      [else
       (define: js : Indexes (make-vector dims 0))
       (case dims
         [(1)  (define: d0 : Index (unsafe-vector-ref ds 0))
               (let: j0-loop : Void ([j0 : Nonnegative-Fixnum  0])
                 (when (j0 . < . d0)
                   (unsafe-vector-set! js 0 j0)
                   (f js j0)
                   (j0-loop (+ j0 1))))]
         [(2)  (define: d0 : Index (unsafe-vector-ref ds 0))
               (define: d1 : Index (unsafe-vector-ref ds 1))
               (let: j0-loop : Void ([j0 : Nonnegative-Fixnum  0]
                                     [j : Nonnegative-Fixnum  0])
                 (when (j0 . < . d0)
                   (unsafe-vector-set! js 0 j0)
                   (let: j1-loop : Void ([j1 : Nonnegative-Fixnum  0]
                                         [j : Nonnegative-Fixnum  j])
                     (cond [(j1 . < . d1)
                            (unsafe-vector-set! js 1 j1)
                            (f js j)
                            (j1-loop (+ j1 1) (unsafe-fx+ j 1))]
                           [else
                            (j0-loop (+ j0 1) j)]))))]
         [else  (let: i-loop : Nonnegative-Fixnum ([i : Nonnegative-Fixnum  0]
                                                   [j : Nonnegative-Fixnum  0])
                  (cond [(i . < . dims)
                         (define: di : Index (unsafe-vector-ref ds i))
                         (let: ji-loop : Nonnegative-Fixnum ([ji : Nonnegative-Fixnum  0]
                                                             [j : Nonnegative-Fixnum  j])
                           (cond [(ji . < . di)
                                  (unsafe-vector-set! js i ji)
                                  (ji-loop (+ ji 1) (i-loop (+ i 1) j))]
                                 [else  j]))]
                        [else  (f js j)
                               (unsafe-fx+ j 1)]))
                (void)])])))

(define-syntax-rule (for-each-array-index ds-expr f-expr)
  (let*: ([ds : Indexes  ds-expr]
          [dims : Index  (vector-length ds)])
    (define-syntax-rule (f js)
      ((ann f-expr (Indexes -> Void)) js))
    (cond
      [(= dims 0)  (f ds)]
      [else
       (define: js : Indexes (make-vector dims 0))
       (case dims
         [(1)  (define: d0 : Index (unsafe-vector-ref ds 0))
               (let: j0-loop : Void ([j0 : Nonnegative-Fixnum  0])
                 (when (j0 . < . d0)
                   (unsafe-vector-set! js 0 j0)
                   (f js)
                   (j0-loop (+ j0 1))))]
         [(2)  (define: d0 : Index (unsafe-vector-ref ds 0))
               (define: d1 : Index (unsafe-vector-ref ds 1))
               (let: j0-loop : Void ([j0 : Nonnegative-Fixnum  0])
                 (when (j0 . < . d0)
                   (unsafe-vector-set! js 0 j0)
                   (let: j1-loop : Void ([j1 : Nonnegative-Fixnum  0])
                     (cond [(j1 . < . d1)
                            (unsafe-vector-set! js 1 j1)
                            (f js)
                            (j1-loop (+ j1 1))]
                           [else
                            (j0-loop (+ j0 1))]))))]
         [else  (let: i-loop : Void ([i : Nonnegative-Fixnum  0])
                  (cond [(i . < . dims)
                         (define: di : Index (unsafe-vector-ref ds i))
                         (let: ji-loop : Void ([ji : Nonnegative-Fixnum  0])
                           (when (ji . < . di)
                             (unsafe-vector-set! js i ji)
                             (i-loop (+ i 1))
                             (ji-loop (+ ji 1))))]
                        [else  (f js)]))])])))

(define-syntax-rule (for-each-data-index ds-expr f-expr)
  (let*: ([ds : Indexes  ds-expr]
          [dims : Index  (vector-length ds)])
    (define-syntax-rule (f j)
      ((ann f-expr (Nonnegative-Fixnum -> Void)) j))
    (cond
      [(= dims 0)  (f 0)]
      [else
       (case dims
         [(1)  (define: d0 : Index (unsafe-vector-ref ds 0))
               (let: j0-loop : Void ([j0 : Nonnegative-Fixnum  0])
                 (when (j0 . < . d0)
                   (f j0)
                   (j0-loop (+ j0 1))))]
         [(2)  (define: d0 : Index (unsafe-vector-ref ds 0))
               (define: d1 : Index (unsafe-vector-ref ds 1))
               (let: j0-loop : Void ([j0 : Nonnegative-Fixnum  0]
                                     [j : Nonnegative-Fixnum  0])
                 (when (j0 . < . d0)
                   (let: j1-loop : Void ([j1 : Nonnegative-Fixnum  0]
                                         [j : Nonnegative-Fixnum  j])
                     (cond [(j1 . < . d1)
                            (f j)
                            (j1-loop (+ j1 1) (unsafe-fx+ j 1))]
                           [else
                            (j0-loop (+ j0 1) j)]))))]
         [else  (let: i-loop : Nonnegative-Fixnum ([i : Nonnegative-Fixnum  0]
                                                   [j : Nonnegative-Fixnum  0])
                  (cond [(i . < . dims)
                         (define: di : Index (unsafe-vector-ref ds i))
                         (let: ji-loop : Nonnegative-Fixnum ([ji : Nonnegative-Fixnum  0]
                                                             [j : Nonnegative-Fixnum  j])
                           (cond [(ji . < . di)
                                  (ji-loop (+ ji 1) (i-loop (+ i 1) j))]
                                 [else  j]))]
                        [else  (f j)
                               (unsafe-fx+ j 1)]))
                (void)])])))

(define-syntax-rule (inline-build-array-data ds-expr g-expr A)
  (let*: ([ds : Indexes  ds-expr]
          [dims : Index  (vector-length ds)])
    (define-syntax-rule (g js j)
      ((ann g-expr (Indexes Nonnegative-Fixnum -> A)) js j))
    (define: size : Nonnegative-Fixnum
      (let: loop : Nonnegative-Fixnum ([k : Nonnegative-Fixnum  0] [size : Nonnegative-Fixnum  1])
        (cond [(k . < . dims)  (loop (+ k 1) (unsafe-fx* size (unsafe-vector-ref ds k)))]
              [else  size])))
    (cond [(= size 0)  (ann (vector) (Vectorof A))]
          [else
           (define: js0 : Indexes (make-vector dims 0))
           (define: vs : (Vectorof A) (make-vector size (g js0 0)))
           (for-each-array+data-index ds (λ (js j) (unsafe-vector-set! vs j (g js j))))
           vs])))
