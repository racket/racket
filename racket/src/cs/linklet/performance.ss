(define perf-regions (make-eq-hashtable))
(define-record perf-region (time gc-time count memory))

(define current-perf-frame #f)

(define-record perf-frame (start gc-start nested-delta nested-gc-delta next))

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

(define (label->perf-region label)
  (or (hashtable-ref perf-regions label #f)
      (let ([r (make-perf-region 0 0 0 0)])
        (hashtable-set! perf-regions label r)
        r)))

(define (measure-performance-region label thunk)
  (cond
   [(and measure-performance?
         (eqv? (get-thread-id) performance-thread-id))
    (set! current-perf-frame (make-perf-frame (current-process-milliseconds)
                                              (current-gc-milliseconds)
                                              0
                                              0
                                              current-perf-frame))
    (begin0
     (thunk)
     (let ([f current-perf-frame])
       (when f ; avoid crash if thread swaps mangle stack update
         (let ([delta (- (current-process-milliseconds) (perf-frame-start f) (perf-frame-nested-delta f))]
               [gc-delta (- (current-gc-milliseconds) (perf-frame-gc-start f) (perf-frame-nested-gc-delta f))]
               [r (label->perf-region label)])
           (set-perf-region-time! r (+ (perf-region-time r) delta))
           (set-perf-region-gc-time! r (+ (perf-region-gc-time r) gc-delta))
           (set-perf-region-count! r (+ (perf-region-count r) 1))
           (let ([next (perf-frame-next f)])
             (set! current-perf-frame next)
             (when next
               (set-perf-frame-nested-delta! next (+ delta (perf-frame-nested-delta f) (perf-frame-nested-delta next)))
               (set-perf-frame-nested-gc-delta! next (+ gc-delta (perf-frame-nested-gc-delta f) (perf-frame-nested-gc-delta next)))))))))]
   [else (thunk)]))

(define (add-performance-memory! label delta)
  (when measure-performance?
    (with-interrupts-disabled
     (let ([r (label->perf-region label)])
       (set-perf-region-memory! r (+ (perf-region-memory r) delta))))))

(define (linklet-performance-init!)
  (let ([r (label->perf-region 'boot)])
    (set-perf-region-time! r (time->ms (sstats-cpu (statistics))))))

(define (linklet-performance-report!)
  (when measure-performance?
    (let ([region-times (make-eq-hashtable)]
          [region-gc-times (make-eq-hashtable)]
          [region-counts (make-eq-hashtable)]
          [region-memories (make-eq-hashtable)])
      (hash-table-for-each perf-regions
                           (lambda (k r)
                             (hashtable-set! region-times k (perf-region-time r))
                             (hashtable-set! region-gc-times k (perf-region-gc-time r))
                             (hashtable-set! region-counts k (perf-region-count r))
                             (let ([m (perf-region-memory r)])
                               (unless (zero? m)
                                 (hashtable-set! region-memories k m)))))
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
             [categories '((read (read-bundle faslin-code faslin-literals))
                           (comp-ffi (comp-ffi-call comp-ffi-back))
                           (run (instantiate))
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
                  (ht->sorted-list region-memories))))))
