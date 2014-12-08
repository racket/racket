#lang racket

(define (ref-bench size factor maker get)
  (parameterize {[current-output-port (open-output-nowhere)]}
    (define d (maker size))
    (for {[i (in-range size)]}
      (define k (modulo (* i factor) size))
      (printf "~v => ~v\n" k (get d k)))))

(define (in-dict-bench size maker)
  (parameterize {[current-output-port (open-output-nowhere)]}
    (for {[{k v} (in-dict (maker size))]}
      (printf "~v => ~v\n" k v))))

(define (slow-in-dict-bench size maker)
  (parameterize {[current-output-port (open-output-nowhere)]}
    (for {[{k v} (apply in-dict (maker size) '())]}
      (printf "~v => ~v\n" k v))))

(define (in-stream-bench size maker)
  (parameterize {[current-output-port (open-output-nowhere)]}
    (for {[k (in-naturals)]
          [v (in-stream (maker size))]}
      (printf "~v => ~v\n" k v))))

(define (slow-in-stream-bench size maker)
  (parameterize {[current-output-port (open-output-nowhere)]}
    (for {[k (apply in-naturals '())]
          [v (apply in-stream (maker size) '())]}
      (printf "~v => ~v\n" k v))))

(define (in-set-bench size maker)
  (parameterize {[current-output-port (open-output-nowhere)]}
    (for {[k (in-naturals)]
          [v (in-set (maker size))]}
      (printf "~v => ~v\n" k v))))

(define (slow-in-set-bench size maker)
  (parameterize {[current-output-port (open-output-nowhere)]}
    (for {[k (apply in-naturals '())]
          [v (apply in-set (maker size) '())]}
      (printf "~v => ~v\n" k v))))

(define (imm-hash n)
  (for/fold {[h (hash)]} {[i (in-range n)]}
    (hash-set h i (number->string i))))

(define (mut-hash n)
  (define h (make-hash))
  (for {[i (in-range n)]}
    (hash-set! h i (number->string i)))
  h)

(define (mut-vec n)
  (build-vector n number->string))

(define (imm-vec n)
  (vector->immutable-vector (mut-vec n)))

(define (imm-assoc n)
  (build-list n (lambda (i) (cons i (number->string i)))))

(define (imm-list n)
  (build-list n identity))

(define (imm-stream n)
  (let loop ([i 0])
    (cond
      [(= i n) empty-stream]
      [else (stream-cons i (loop (add1 i)))])))

(define (imm-set n)
  (list->set (imm-list n)))

(define dict-makers (list imm-hash mut-hash imm-vec mut-vec imm-assoc))
(define stream-makers (list imm-list imm-stream))
(define little (expt 10 4))
(define big (expt 10 6))
(define factor 137)

(define-syntax-rule (timing e)
  (get-timing 'e (lambda () e)))

(define (get-timing expr proc)
  (collect-garbage)
  (collect-garbage)
  (define-values (results cpu real gc)
    (time-apply proc '()))
  (printf "~a ms for: ~s\n" cpu expr)
  (apply values results))

;; --------------------
;; Speed of dict-ref
(displayln "-- dict-ref")

(timing (ref-bench big factor imm-hash dict-ref))
(timing (ref-bench big factor mut-hash dict-ref))
(timing (ref-bench big factor mut-vec dict-ref))
(timing (ref-bench little factor imm-assoc dict-ref))

;; --------------------
;; Speed of stream-ref
(displayln "-- stream-ref")

(timing (ref-bench little factor imm-stream stream-ref))
(timing (ref-bench little factor imm-list stream-ref))

;; --------------------
;; Speed of in-stream
(displayln "-- in-stream")

;; lazy streams
(timing (in-stream-bench big imm-stream))
(timing (slow-in-stream-bench big imm-stream))

;; lists
(timing (in-stream-bench big imm-list))
(timing (slow-in-stream-bench big imm-list))

;; sets
(timing (in-stream-bench big imm-set))
(timing (slow-in-stream-bench big imm-set))

;; --------------------
;; Speed of in-dict
(displayln "-- in-dict")

;; immutable hash tables
(timing (in-dict-bench big imm-hash))
(timing (slow-in-dict-bench big imm-hash))

;; mutable hash tables
(timing (in-dict-bench big mut-hash))
(timing (slow-in-dict-bench big mut-hash))

;; vectors
(timing (in-dict-bench big mut-vec))
(timing (slow-in-dict-bench big mut-vec))

;; alists
(timing (in-dict-bench big imm-assoc))
(timing (slow-in-dict-bench big imm-assoc))

;; --------------------
;; Speed of in-set:
(displayln "-- in-set")

;; sets
(timing (in-set-bench big imm-set))
(timing (slow-in-set-bench big imm-set))

;; lists
(timing (in-set-bench big imm-list))
(timing (slow-in-set-bench big imm-list))

;; ----------------------------------------

(module+ test
  (module config info
    (define random? #t)))
