(load-relative "loadtest.rktl")

(Section 'sequence)

(require racket/sequence
         racket/stream)

(define (try-basic-sequence-ops sequence?
                                empty-sequence
                                sequence->list
                                sequence-length
                                sequence-ref
                                sequence-tail
                                sequence-append
                                sequence-map
                                sequence-andmap
                                sequence-ormap
                                sequence-for-each
                                sequence-fold
                                sequence-filter
                                sequence-add-between
                                sequence-count)

  (test #t sequence? empty-sequence)
  (test #t sequence? (in-range 10))
  (test #t sequence? '(1 2 3))
  (test #f sequence? 'symbol)
  
  (test '(0 1 2) 'sequence->list (sequence->list (in-range 3)))
  (arity-test sequence->list 1 1)
  (err/rt-test (sequence->list 'a))

  (test '() 'empty-sequence (sequence->list empty-sequence))

  (arity-test sequence-length 1 1)
  (err/rt-test (sequence-length 'a))
  (test 3 'sequence-length (sequence-length (in-range 3)))

  (arity-test sequence-ref 2 2)
  (err/rt-test (sequence-ref 'a 0))
  (err/rt-test (sequence-ref (in-naturals) -1) exn:fail?)
  (err/rt-test (sequence-ref (in-naturals) 1.0) exn:fail?)
  (test 0 'sequence-ref (sequence-ref (in-naturals) 0))
  (test 1 'sequence-ref (sequence-ref (in-naturals) 1))
  (test 25 'sequence-ref (sequence-ref (in-naturals) 25))
  (test #f 'sequence-ref (sequence-ref '(#t #t #f) 2))
  (when (sequence? 10)
    (test 3 sequence-ref 10 3))
  (when (sequence? #hash())
    (test-values '(a "a") (lambda () (sequence-ref (in-hash #hash((a . "a"))) 0))))
  (err/rt-test (sequence-ref (in-string "a") 2) exn:fail?)

  (arity-test sequence-tail 2 2)
  (err/rt-test (sequence-tail (in-naturals) -1) exn:fail?)
  (err/rt-test (sequence-tail (in-naturals) 1.0) exn:fail?)
  (test 4 'sequence-ref (sequence-ref (sequence-tail (in-naturals) 4) 0))
  (test 5 'sequence-ref (sequence-ref (sequence-tail (in-naturals) 4) 1))
  (test 29 'sequence-ref (sequence-ref (sequence-tail (in-naturals) 4) 25))
  (when (sequence? 10)
    (test 29 'sequence-ref (sequence-ref (sequence-tail 100 4) 25)))

  ;; XXX Check for rest
  (err/rt-test (sequence-append 'a) exn:fail?)
  (err/rt-test (sequence-append (in-naturals) 'a) exn:fail?)
  (test '() 'sequence-append (sequence->list (sequence-append)))
  (test 5 'sequence-append (sequence-ref (sequence-append (in-naturals)) 5))
  (test 5 'sequence-append
        (sequence-ref (sequence-append (in-range 3) (in-range 3 10)) 5))
  (when (sequence? 10)
    (test 5 sequence-ref (sequence-append 4 10) 9))

  (arity-test sequence-map 2 2)
  (err/rt-test (sequence-map 2 (in-naturals)) exn:fail?)
  (test '(1 2 3) 'sequence-map (sequence->list (sequence-map add1 (in-range 3))))
  (test 3 'sequence-map (sequence-ref (sequence-map add1 (in-naturals)) 2))

  (arity-test sequence-andmap 2 2)
  (err/rt-test (sequence-andmap 2 (in-naturals)))
  (test #t 'sequence-andmap (sequence-andmap even? '(2)))
  (test #f 'sequence-andmap (sequence-andmap even? (in-naturals)))

  (arity-test sequence-ormap 2 2)
  (err/rt-test (sequence-ormap 2 (in-naturals)))
  (test #t 'sequence-ormap (sequence-ormap even? '(2)))
  (test #f 'sequence-ormap (sequence-ormap even? '(1)))
  (test #t 'sequence-ormap (sequence-ormap even? (in-naturals)))

  (arity-test sequence-for-each 2 2)
  (err/rt-test (sequence-for-each 2 (in-naturals)))
  (test (vector 0 1 2)
        'sequence-for-each
        (let ([v (vector #f #f #f)])
          (sequence-for-each (λ (i) (vector-set! v i i)) (in-range 3))
          v))

  (arity-test sequence-fold 3 3)
  (err/rt-test (sequence-fold 2 (in-naturals) 0))
  (test 6 'sequence-fold (sequence-fold + 0 (in-range 4)))

  (arity-test sequence-filter 2 2)
  (err/rt-test (sequence-filter 2 (in-naturals)) exn:fail?)
  (test 4 'sequence-filter (sequence-ref (sequence-filter even? (in-naturals)) 2))
  (test 0 sequence-length (sequence-filter (thunk* #t) empty-sequence))

  (arity-test sequence-add-between 2 2)
  (test 0 'sequence-add-between
        (sequence-ref (sequence-add-between (in-naturals) #t) 0))
  (test #t 'sequence-add-between
        (sequence-ref (sequence-add-between (in-naturals) #t) 1))
  (test 1 'sequence-add-between
        (sequence-ref (sequence-add-between (in-naturals) #t) 2))
  (test #t 'sequence-add-between
        (sequence-ref (sequence-add-between (in-naturals) #t) 3))
  (test 3 'sequence-add-between
        (sequence-length (sequence-add-between (in-range 2) #t)))

  (arity-test sequence-count 2 2)
  (test 0 'sequence-count (sequence-count even? empty-sequence))
  (test 1 'sequence-count (sequence-count even? (in-range 1)))
  (test 5 'sequence-count (sequence-count even? (in-range 10)))
  (let* ([r (random 100)]
         [a (if (even? r)
                (/ r 2)
                (ceiling (/ r 2)))])
    (test a 'sequence-count (sequence-count even? (in-range r))))

  (test '(0 1 2 5 6 5 6)
        'no-state-in-iter-over-append
        (let ([k #f]
              [l null])
          (call-with-continuation-prompt
           (lambda ()
             (for ([i (stream-append (in-range 3) (in-range 5 7))])
               (set! l (cons i l))
               (when (= i 2) (let/cc _k (set! k _k))))))
          (call-with-continuation-prompt
           (lambda ()
             (k #f)))
          (reverse l))))

(try-basic-sequence-ops sequence?
                        empty-sequence
                        sequence->list
                        sequence-length
                        sequence-ref
                        sequence-tail
                        sequence-append
                        sequence-map
                        sequence-andmap
                        sequence-ormap
                        sequence-for-each
                        sequence-fold
                        sequence-filter
                        sequence-add-between
                        sequence-count)
(try-basic-sequence-ops stream?
                        empty-stream
                        stream->list
                        stream-length
                        stream-ref
                        stream-tail
                        stream-append
                        stream-map
                        stream-andmap
                        stream-ormap
                        stream-for-each
                        stream-fold
                        stream-filter
                        stream-add-between
                        stream-count)

(test 3 'sequence-length (sequence-length #hasheq((1 . 'a) (2 . 'b) (3 . 'c))))

(test-values '(2 3) (lambda () (sequence-ref (in-parallel '(2) '(3)) 0)))
(test-values '(8 12) (lambda () (sequence-ref (in-parallel '(2 5 8 -1) '(3 9 12 0)) 2)))

(test #t stream? (sequence-append))
(test #t stream? (sequence-append (in-range 10) '(1 2 3)))
(test #f stream? (sequence-append (in-range 10) (vector 1 2 3) '(1 2 3)))

(test #t stream? (sequence-map add1 (in-range 3)))
(test #f stream? (sequence-map add1 (vector 1 2 3)))

(test #t stream? (sequence-filter odd? (in-range 3)))
(test #f stream? (sequence-filter odd? (vector 1 2 3)))

;; ----------------------------------------

;; Check interaction of sequence operations and side-effecting streams:

(let ([s (open-input-string "012345")])
  (test #\0 peek-char s)
  (let ([t (sequence-tail s 3)])
    (test #\0 peek-char s)
    (test (char->integer #\3) 'tail (for/first ([c t]) c))))

(let ([s (open-input-string "012345")])
  (test #\0 peek-char s)
  (let ([t (sequence-map add1 s)])
    (test #\0 peek-char s)
    (test (list (char->integer #\1) 
                (char->integer #\2) 
                (char->integer #\3))
          'map
          (for/list ([c t]
                     [n (in-range 3)]) 
            c))
    ;; #\3 was read, but loop ended by `in-range'
    (test #\4 peek-char s)))

(let ([s (open-input-string "012345")])
  (let ([t (sequence-tail s 6)])
    (test '() 'tail (for/list ([i t]) i))))

(let ([s (open-input-string "01234567")])
  (test #\0 peek-char s)
  (let ([t (sequence-filter even? s)])
    (test #\0 peek-char s)
    (test (list (char->integer #\0) 
                (char->integer #\2) 
                (char->integer #\4))
          'map
          (for/list ([c t]
                     [n (in-range 3)]) 
            c))
    ;; #\6 was read, but loop ended by `in-range'
    (test #\7 peek-char s)))

(let ([s (open-input-string "0123")])
  (test #\0 peek-char s)
  (let ([t (sequence-add-between s #f)])
    (test #\0 peek-char s)
    (test (list (char->integer #\0)
                #f
                (char->integer #\1)
                #f
                (char->integer #\2)
                #f
                (char->integer #\3))
          'map
          (for/list ([c t]
                     [n (in-range 30)]) 
            c))
    (test eof peek-char s)))

(let ([s (open-input-string "012345")])
  (test #\0 peek-char s)
  (let ([t (sequence-add-between s #f)])
    (test #\0 peek-char s)
    (test (list (char->integer #\0)
                #f
                (char->integer #\1))
          'map
          (for/list ([c t]
                     [n (in-range 3)]) 
            c))
    ;; #\2 was read, but loop ended by `in-range'
    (test #\3 peek-char s)))

;; ----------------------------------------
;; Check sequence constructors with keywords

(define (in-X #:x seq) seq)
(test '(1 2 3) 'kw-seq (for/list ([x (in-X #:x '(1 2 3))]) x))
(define-sequence-syntax in-X* (lambda () #'in-X) (lambda (stx) #f))
(test '(1 2 3) 'kw-seq (for/list ([x (in-X* #:x '(1 2 3))]) x))

;; ----------------------------------------

(report-errs)
