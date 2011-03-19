
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

(report-errs)
