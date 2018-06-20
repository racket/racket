#lang racket/base
(require racket/place
         ffi/unsafe
         ffi/unsafe/alloc)

(module+ test
  (define-syntax-rule (test expect got)
    (let ([e expect]
          [g got])
      (unless (equal? e g)
        (error 'test "failed: ~s => ~v != ~s => ~v" 'expect e 'got g))))

  (define (check-allocator
           #:retain? [retain? #f]
           #:release? [release? #f]
           #:deallocate? [deallocate? #f])
    (printf "~s\n" (list retain? release? deallocate?))
    (define done? #f)
    (define released? #f)
    (let ([x (((allocator (lambda (v) (set! done? (symbol? v))))
               gensym))])
      (collect-garbage)
      (sync (system-idle-evt))
      (test #f done?)
      (when retain?
        (((retainer (lambda (v) (set! released? (symbol? v)))) void) x))
      (when release?
        (((releaser) void) x))
      (when deallocate?
        (((deallocator) void) x))
      (void/reference-sink x)
      (collect-garbage)
      (sync (system-idle-evt))
      (when retain?
        (collect-garbage)
        (sync (system-idle-evt)))
      (test (or (not deallocate?) (and retain? (not release?))) done?)
      (test (and retain? (not (or deallocate? release?))) released?)))

  (unless (eq? 'cgc (system-type 'gc))
    (check-allocator)
    (check-allocator #:deallocate? #t)
    (check-allocator #:retain? #t #:deallocate? #t)
    (check-allocator #:retain? #t #:deallocate? #t #:release? #t)
    (check-allocator #:retain? #t #:deallocate? #f #:release? #t))

  (when (place-enabled?)
    ;; Make sure deallocators are run when a place exits,
    ;; and make sure they're run in the right order:
    (define p (go-in-place))
    (define bstr (make-shared-bytes 2 0))
    (place-channel-put p bstr)
    (void (place-wait p))
    (test #"\x0A\x64" bstr)))

(define (go-in-place)
  (place p
         (define bstr (place-channel-get p))
         ;; These 10 should be deallocated later
         (define orig
           (for/list ([i 10])
             (((allocator (lambda (v) (bytes-set! bstr 1 (+ (bytes-ref bstr 1) (bytes-ref bstr 0)))))
               gensym))))
         ;; These 10 should be deallocated earlier:
         (define next
           (for/list ([i 10])
             (((allocator (lambda (v) (bytes-set! bstr 0 (+ (bytes-ref bstr 0) 1))))
               gensym))))
         (list next orig)))

