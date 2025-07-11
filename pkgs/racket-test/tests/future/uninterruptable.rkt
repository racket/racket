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
(define f/uninterruptable ((allocator void #:merely-uninterruptable? #t) make-false))

(define f/block ((allocator void) make-usually-false))
(define f/block/uninterruptable ((allocator void #:merely-uninterruptable? #t) make-usually-false))

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
                    [(allocator/uninterruptable)
                     (f/uninterruptable i)]
                    [(allocator/block)
                     (f/block i)]
                    [(allocator/block/uninterruptable)
                     (f/block/uninterruptable i)]
                    [(dynamic-wind)
                     (for ([j (in-range 10)])
                       (g i))]
                    [(atomic)
                     (begin
                       (start-atomic)
                       (end-atomic))]
                    [(uninterruptable)
                     (begin
                       (start-uninterruptable)
                       (end-uninterruptable))]
                    [(call-as-atomic)
                     (call-as-atomic
                      (lambda ()
                        (void)))]
                    [(call-as-uninterruptable)
                     (call-as-uninterruptable
                      (lambda ()
                        (void)))]
                    [else (error "unknown mode")]))))))

  (time (for-each touch fs)))

(run 'allocator)
(run 'allocator/uninterruptable)
(run 'allocator/block)
(run 'allocator/block/uninterruptable)
(run 'dynamic-wind)
(run 'atomic 10)
(run 'uninterruptable 10)
(run 'call-as-atomic)
(run 'call-as-uninterruptable)
