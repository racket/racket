#lang racket/base
(require racket/future
         ffi/unsafe/atomic
         ffi/unsafe/alloc)

(define (make-false i) #f)

(define (make-usually-false i)
  (if (= i 10)
      (current-continuation-marks)
      #f))

(define f ((allocator void) make-false))
(define f/uninterruptible ((allocator void #:merely-uninterruptible? #t) make-false))

(define f/block ((allocator void) make-usually-false))
(define f/block/uninterruptible ((allocator void #:merely-uninterruptible? #t) make-usually-false))

(define g (lambda (x) (dynamic-wind void (lambda () x) void)))

(define (run mode [factor 1])
  (define n (* 1000000 factor))
  (println mode)
  (define fs
    (for/list ([i (in-range 8)])
      (future (lambda ()
                (for ([i (in-range n 0 -1)])
                  (case mode
                    [(allocator)
                     (f i)]
                    [(allocator/uninterruptible)
                     (f/uninterruptible i)]
                    [(allocator/block)
                     (f/block i)]
                    [(allocator/block/uninterruptible)
                     (f/block/uninterruptible i)]
                    [(dynamic-wind)
                     (for ([j (in-range 10)])
                       (g i))]
                    [(atomic)
                     (begin
                       (start-atomic)
                       (end-atomic))]
                    [(uninterruptible)
                     (begin
                       (start-uninterruptible)
                       (end-uninterruptible))]
                    [(call-as-atomic)
                     (call-as-atomic
                      (lambda ()
                        (void)))]
                    [(call-as-uninterruptible)
                     (call-as-uninterruptible
                      (lambda ()
                        (void)))]
                    [else (error "unknown mode")]))))))

  (time (for-each touch fs)))

(run 'allocator)
(run 'allocator/uninterruptible)
(run 'allocator/block)
(run 'allocator/block/uninterruptible)
(run 'dynamic-wind)
(run 'atomic 10)
(run 'uninterruptible 10)
(run 'call-as-atomic)
(run 'call-as-uninterruptible)
