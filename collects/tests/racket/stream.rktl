
(load-relative "loadtest.rktl")

(Section 'stream)

(require racket/stream)

;; >>> Many basic stream tests are in "sequence.rktl" <<<

(test #f stream? '#(1 2))

(test 1 'stream-length (stream-length (stream-cons 1 empty-stream)))

(define infinite-ones (stream-cons 1 infinite-ones))

(test 1 stream-first infinite-ones)
(test 1 stream-ref infinite-ones 100)

(test 1 stream-first (stream-cons 1 (let loop () (loop))))
(test 2 stream-length (stream-cons (let loop () (loop)) 
                                   (stream-cons (let loop () (loop)) 
                                                empty)))

(test #t stream-empty? (sequence->stream (in-producer void (void))))
(test #t stream-empty? (sequence->stream (in-port read-byte (open-input-string ""))))

(test "hello" stream-first (sequence->stream (in-producer (lambda () "hello") (void))))
(test 65 stream-first (sequence->stream (in-port read-byte (open-input-string "A"))))

(report-errs)
