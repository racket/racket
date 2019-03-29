(define region-times (make-eq-hashtable))
(define region-gc-times (make-eq-hashtable))
(define region-counts (make-eq-hashtable))
(define region-memories (make-eq-hashtable))

(define current-start-time '())
(define current-gc-start-time '())

(define performance-thread-id (get-thread-id))

;; List keys for passes related to register allocation as recorded by
;; Chez Scheme and reported from `$pass-stats`:
(define register-allocation-passes
  '(do-live-analysis!
    record-call-live!
    identify-poison!
    do-spillable-conflict!
    assign-frame!
    assign-new-frame!
    finalize-register-locations!
    do-unspillable-conflict!
    assign-registers!
    finalize-frame-locations!
    select-instructions!))

(define (time->ms t)
  (inexact->exact
   (floor
    (+ (* 1000. (time-second t))
       (/ (time-nanosecond t) 1000000.)))))

;; Beware that `performance-region` doesn't really handle escapes, and
;; Racket-level thread swaps during `performance-region` can cause
;; strange results.

(define-syntax performance-region
  (syntax-rules ()
    [(_ label e ...) (measure-performance-region label (lambda () e ...))]))

(define (measure-performance-region label thunk)
  (cond
   [(and measure-performance?
         (eqv? (get-thread-id) performance-thread-id))
    (with-interrupts-disabled
     (set! current-start-time (cons (current-process-milliseconds) current-start-time))
     (set! current-gc-start-time (cons (current-gc-milliseconds) current-gc-start-time)))
    (begin0
     (thunk)
     (with-interrupts-disabled
      (let ([delta (- (current-process-milliseconds) (car current-start-time))]
            [gc-delta (- (current-gc-milliseconds) (car current-gc-start-time))])
        (hashtable-update! region-times label (lambda (v) (+ v delta)) 0)
        (hashtable-update! region-gc-times label (lambda (v) (+ v gc-delta)) 0)
        (hashtable-update! region-counts label add1 0)
        (set! current-start-time (cdr current-start-time))
        (set! current-gc-start-time (cdr current-gc-start-time))
        (let loop ([l current-start-time] [gc-l current-gc-start-time])
          (unless (null? l)
            (set-car! l (+ (car l) delta))
            (set-car! gc-l (+ (car gc-l) gc-delta))
            (loop (cdr l) (cdr gc-l)))))))]
   [else (thunk)]))

(define (add-performance-memory! label delta)
  (when measure-performance?
    (with-interrupts-disabled
     (hashtable-update! region-memories label (lambda (v) (+ v delta)) 0))))

(define (linklet-performance-init!)
  (hashtable-set! region-times 'boot
                  (time->ms (sstats-cpu (statistics)))))

(define (linklet-performance-report!)
  (when measure-performance?
    (for-each (lambda (s)
                (let ([label (if (#%memq (car s) register-allocation-passes)
                                 'regalloc
                                 'other)])
                  (let-values ([(count cpu gc-cpu bytes) (apply values (cdr s))])
                    (hashtable-update! region-times label (lambda (v) (+ v (time->ms cpu))) 0)
                    (hashtable-update! region-gc-times label (lambda (v) (+ v (time->ms gc-cpu))) 0)
                    (hashtable-update! region-counts label (lambda (v) (max count v)) 0))))
              (#%$pass-stats))
    (let* ([total (apply + (hash-table-map region-times (lambda (k v) (round (inexact->exact v)))))]
           [gc-total (apply + (hash-table-map region-gc-times (lambda (k v) v)))]
           [name-len (apply max (hash-table-map region-times (lambda (k v) (string-length (symbol->string k)))))]
           [len (string-length (number->string total))]
           [gc-len (string-length (number->string gc-total))]
           [categories '((read (read-bundle faslin-code))
                         (comp-ffi (comp-ffi-call comp-ffi-back))
                         (run (instantiate outer))
                         (compile (compile-linklet compile-nested))
                         (compile-pass (regalloc other)))]
           [region-subs (make-eq-hashtable)]
           [region-gc-subs (make-eq-hashtable)])
      (define (lprintf fmt . args)
        (log-message root-logger 'error (apply #%format fmt args) #f))
      (define (pad v w combine)
        (let ([s (#%format "~a" v)])
          (combine (make-string (max 0 (- w (string-length s))) #\space)
                   s)))
      (define (pad-left v w) (pad v w string-append))
      (define (pad-right v w) (pad v w (lambda (p s) (string-append s p))))
      (define (report level label n n-extra units extra)
        (lprintf ";; ~a~a~a  ~a~a ~a~a"
                 (make-string (* level 2) #\space)
                 (pad-right label name-len)
                 (make-string (* (- 3 level) 2) #\space)
                 (pad-left (round (inexact->exact n)) len)
                 n-extra
                 units
                 extra))
      (define (ht->sorted-list ht)
        (list-sort (lambda (a b) (< (cdr a) (cdr b)))
                   (hash-table-map ht cons)))
      (define (sum-values ht keys key subs)
        (define sub-ht (make-eq-hashtable))
        (hashtable-set! subs key sub-ht)
        (let loop ([keys keys])
          (cond
           [(null? keys) 0]
           [else
            (let* ([sub-key (car keys)]
                   [v (hashtable-ref ht sub-key 0)])
              (hashtable-set! sub-ht sub-key v)
              (hashtable-delete! ht sub-key)
              (+ v (loop (cdr keys))))])))
      (define (report-time level label n gc-ht)
        (report level label n
                (#%format " [~a]" (pad-left (hashtable-ref gc-ht label 0) gc-len))
                'ms
                (let ([c (hashtable-ref region-counts label 0)])
                  (if (zero? c)
                      ""
                      (#%format " ; ~a times" c)))))
      (for-each (lambda (l)
                  (let* ([cat (car l)]
                         [subs (cadr l)]
                         [t (sum-values region-times subs cat region-subs)]
                         [gc-t (sum-values region-gc-times subs cat region-gc-subs)])
                    (unless (and (zero? t) (zero? gc-t))
                      (hashtable-set! region-times cat t)
                      (hashtable-set! region-gc-times cat gc-t))))
                categories)
      (let loop ([ht region-times] [gc-ht region-gc-times] [level 0])
        (for-each (lambda (p)
                    (let ([label (car p)]
                          [n (cdr p)])
                      (report-time level label n gc-ht)
                      (let ([sub-ht (hashtable-ref region-subs label #f)]
                            [sub-gc-ht (hashtable-ref region-gc-subs label #f)])
                        (when sub-ht
                          (loop sub-ht sub-gc-ht (add1 level))))))
                  (ht->sorted-list ht)))
      (report 0 'total total (#%format " [~a]" gc-total) 'ms "")
      (lprintf ";;")
      (for-each (lambda (p) (report 0 (car p) (/ (cdr p) 1024 1024) "" 'MB ""))
                (ht->sorted-list region-memories)))))
