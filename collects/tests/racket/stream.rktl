
(load-relative "loadtest.rktl")

(Section 'stream)

(require racket/stream)

;; >>> Many basic stream tests are in "sequence.rktl" <<<

(test #f stream? '#(1 2))

(test 1 'stream-length (stream-length (stream-cons 1 empty-stream)))
(test 1 'stream-length (stream-length (stream 1)))

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

(define one-to-four (stream 1 2 3 4))
(test 1 stream-first one-to-four)
(test 2 stream-ref one-to-four 1)
(test 3 stream-ref one-to-four 2)
(test 4 stream-ref one-to-four 3)
(test 2 'stream (stream-length (stream 1 (/ 0))))
(test 'a 'stream (stream-first (stream 'a (/ 0))))

(report-errs)
