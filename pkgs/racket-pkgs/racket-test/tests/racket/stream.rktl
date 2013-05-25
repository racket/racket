
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

;; make sure stream operations work on lists
(test #t stream-empty? '())
(test 1 stream-first '(1 2 3))
(test '(2 3) stream-rest '(1 2 3))
(test 3 stream-length '(1 2 3))
(test 1 stream-ref '(1 2 3) 0)
(test '(2 3) stream-tail '(1 2 3) 1)
(test '(1 2 3 4 5) stream->list (stream-append '(1 2 3) '(4 5)))
(test '(1 2 3) stream->list (stream-map values '(1 2 3)))
(test #f stream-andmap even? '(1 2 3))
(test #t stream-ormap even? '(1 2 3))
(test #t void? (stream-for-each void '(1 2 3)))
(test 6 stream-fold + 0 '(1 2 3))
(test 1 stream-count even? '(1 2 3))
(test '(1 3) stream->list (stream-filter odd? '(1 2 3)))
(test '(1 a 2 a 3) stream->list (stream-add-between '(1 2 3) 'a))

(report-errs)
