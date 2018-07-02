(define region-times (make-eq-hashtable))
(define region-gc-times (make-eq-hashtable))
(define region-counts (make-eq-hashtable))
(define region-memories (make-eq-hashtable))

(define current-start-time '())
(define current-gc-start-time '())

;; Beware that `performance-region` doesn't really handle escapes, and
;; Racket-level thread swaps during `performance-region` can cause
;; strange results.

(define-syntax performance-region
  (syntax-rules ()
    [(_ label e ...) (measure-performance-region label (lambda () e ...))]))

(define (measure-performance-region label thunk)
  (cond
   [measure-performance?
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
                  (let ([t (sstats-cpu (statistics))])
                    (+ (* 1000.0 (time-second t))
                       (/ (time-nanosecond t) 1000000.0)))))

(define (linklet-performance-report!)
  (when measure-performance?
    (let* ([total (apply + (hash-table-map region-times (lambda (k v) (round (inexact->exact v)))))]
           [gc-total (apply + (hash-table-map region-gc-times (lambda (k v) v)))]
           [name-len (apply max (hash-table-map region-times (lambda (k v) (string-length (symbol->string k)))))]
           [len (string-length (number->string total))]
           [gc-len (string-length (number->string gc-total))])
      (define (pad v w)
        (let ([s (chez:format "~a" v)])
          (string-append (make-string (max 0 (- w (string-length s))) #\space)
                         s)))
      (define (report label n n-extra units extra)
        (chez:printf ";; ~a:  ~a~a ~a~a\n"
                     (pad label name-len)
                     (pad (round (inexact->exact n)) len)
                     n-extra
                     units
                     extra))
      (define (ht->sorted-list ht)
        (list-sort (lambda (a b) (< (cdr a) (cdr b)))
                   (hash-table-map ht cons)))
      (for-each (lambda (p)
                  (let ([label (car p)]
                        [n (cdr p)])
                    (report label n
                            (chez:format " [~a]" (pad (hashtable-ref region-gc-times label 0) gc-len))
                            'ms
                            (let ([c (hashtable-ref region-counts label 0)])
                              (if (zero? c)
                                  ""
                                  (chez:format " ; ~a times" c))))))
                (ht->sorted-list region-times))
      (report 'total total (#%format " [~a]" gc-total) 'ms "")
      (chez:printf ";;\n")
      (for-each (lambda (p) (report (car p) (/ (cdr p) 1024 1024) "" 'MB ""))
                (ht->sorted-list region-memories)))))
