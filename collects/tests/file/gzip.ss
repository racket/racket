#lang scheme/base
(require file/gzip file/gunzip scheme/file tests/eli-tester)

(define ((io->str-op io) buf [check-ratio #f])
  (let* ([b? (bytes? buf)]
         [i (if b? (open-input-bytes buf) (open-input-string buf))]
         [o (if b? (open-output-bytes)    (open-output-string))])
    (io i o)
    (let ([res (if b? (get-output-bytes o) (get-output-string o))])
      (when check-ratio
        (if b?
          (check-ratio (bytes-length  buf) (bytes-length  res))
          (check-ratio (string-length buf) (string-length res))))
      res)))

(define deflate* (io->str-op deflate))
(define inflate* (io->str-op inflate))

(define (id* buf [ratio #f])
  (test (inflate* (deflate* buf (and ratio (lambda (i o)
                                             (test (< (/ o i) ratio))))))
        => buf))

(define (test-big-file)
  (define big-file
    (build-path (collection-path "drscheme/private") "unit.ss"))
  ;; should be around 6 times smaller
  (id* (file->bytes big-file) 4))

(define (run-tests)
  (define (rand-bytes)
    (list->bytes (for/list ([j (in-range (random 1000))]) (random 256))))
  (test-big-file)
  (for ([i (in-range 100)]) (id* (rand-bytes))))

(provide tests)
(define (tests) (test do (run-tests)))
