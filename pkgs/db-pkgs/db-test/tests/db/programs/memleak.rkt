#lang racket/base
(require (prefix-in db: db)
         racket/match
         racket/class)

;; Test for db memory leaks

;; FIXME: mysql quickly exhausts prepared statement limit

(define the-dsn
  (match (current-command-line-arguments)
    [(vector x)
     (string->symbol x)]
    [_
     (printf "No dsn argument given, exiting.\n")
     (exit 0)]))

;; 'tx, 'no
(define start-tx-mode 'no)
(define query-tx-mode 'no)

;; boolean
(define reconnect? #f)
(define random-query? #f)

;; ----

(define (get-c)
  (printf "-- connect\n")
  (let ([c (db:dsn-connect the-dsn)])
    (case start-tx-mode
      ((tx) (db:start-transaction c))
      ((no) (void)))
    c))

(define c #f)

(define (c-test)
  ;; Randomize to prevent statement caching
  (define (go)
    (db:query-value c (if random-query?
                          (format "SELECT ~a" (random #e1e6))
                          "SELECT 1")))
  (case query-tx-mode
    ((tx) (db:call-with-transaction c go))
    ((no) (go))))

(let loop ()
  (collect-garbage)
  (collect-garbage)
  (when (or (not c) reconnect?)
    (set! c (get-c)))
  (displayln (quotient (current-memory-use) #e1e6))
  (for ([i (in-range 10000)])
    (c-test))
  (loop))
