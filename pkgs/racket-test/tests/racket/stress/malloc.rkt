#lang racket/base
(require racket/system
         compiler/find-exe)

;; Check that if we allocate large chunks of memory quickly, a garbage
;; collection happens soon enough to keep memory use in line.

(define (check mode make-malloc)
  (printf "Checking ~a\n" mode)

  (define o (open-output-bytes))
  (define exe (find-exe))

  ;; An amount of memory much larger than Racket's base memory use:
  (define len (* 100 1024 1024))

  (define ok?
    (parameterize ([current-error-port o])
      (system* exe
               "-W"
               "debug@GC error"
               "-l" "racket/base"
               "-l" "ffi/unsafe"
               "-e" (format "~s" '(define saves (make-vector 1)))
               "-e" (format "~s"
                            `(let loop ([n 0])
                               (unless (> n 100)
                                 (vector-set! saves 0 #f)
                                 ;; using `malloc` to allocate memory that doesn't
                                 ;; have to be initialized
                                 (vector-set! saves 0 ,(make-malloc len))
                                 (loop (+ n 1))))))))

  (unless ok?
    (error "running test loop failed"))

  (define str (get-output-string o))
  (define pk (regexp-match #rx"peak ([0-9,]*)K" str))
  (unless pk
    (error "did not find peak-memory logging output"))

  (define peak (* 1024 (string->number (regexp-replace* #rx"," (cadr pk) ""))))

  (when (peak . > . (* 5 len))
    (error "peak memory use was too high:" peak)))

(check "malloc" (lambda (len) `(malloc ,len _byte 'atomic)))
(check "bytes" (lambda (len) `(make-bytes ,len 0)))
(check "phantom" (lambda (len) `(make-phantom-bytes ,len)))
(check "phantom-set!" (lambda (len) `(let ([p (make-phantom-bytes 1)])
                                       (set-phantom-bytes! p ,len)
                                       p)))
