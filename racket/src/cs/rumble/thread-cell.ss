;; A "thread cell" is actually an "engine cell" at the Rumble level

;; Need at least one mutable field, so allocated each time
(define-record-type (thread-cell create-thread-cell thread-cell?)
  (fields default-value
          preserved?
          (mutable mutated?))
  (sealed #t))

(define make-thread-cell
  (case-lambda
    [(v) (make-thread-cell v #f)]
    [(v preserved?) (create-thread-cell v (and preserved? #t) #f)]))

(define/who (thread-cell-ref c)
  (check who thread-cell? c)
  (unsafe-thread-cell-ref c))

(define (unsafe-thread-cell-ref c)
  (if (thread-cell-mutated? c)
      (let* ([t (current-engine-thread-cell-values)])
        (if t
            (eq-hashtable-ref t c (thread-cell-default-value c))
            (thread-cell-default-value c)))
      (thread-cell-default-value c)))

(define/who (thread-cell-set! c v)
  (check who thread-cell? c)
  (thread-cell-mutated?-set! c #t)
  (let ([p (eq-hashtable-try-atomic-cell (current-engine-thread-cell-values) c v)])
    (cond
     [p (set-cdr! p v)]
     [else
      ;; Contention, so try again
      (thread-cell-set! c v)])))

;; ----------------------------------------

(define-record thread-cell-values (t))

(define/who current-preserved-thread-cell-values
  (case-lambda
   [() (make-thread-cell-values (new-engine-thread-cell-values))]
   [(tcvs)
    (check who thread-cell-values? tcvs)
    (set-current-engine-thread-cell-values! (thread-cell-values-t tcvs))]))
