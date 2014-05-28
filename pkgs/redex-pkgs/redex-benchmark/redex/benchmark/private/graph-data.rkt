#lang racket/base

(require racket/list
         racket/match
         math/statistics
         math/distributions
         plot
         "logging.rkt")

(provide type-colors)

(define all-types '(grammar search enum ordered))
(define colors  '(1 2 3 4))    
(define type-colors
  (for/hash ([t all-types] [c colors])
    (values t c)))

(define (plot/log-directory path)
  (make-plot
   (extract-data/log-directory path)
   (extract-names/log-directory path)))

(define (extract-log-data ld-list)
  (for/list ([d (in-list (filter
                          (make-event-filter 'counterexample)
                          ld-list))])
    (list ((datum-selector '#:model) d)
          ((datum-selector '#:type) d)
          ((datum-selector '#:time) d))))

(define (extract-data/log-directory dir-path)
  (append-map
   extract-log-data
   (map read-logfile 
        (filter
         file-exists?
         (directory-list dir-path
                         #:build? dir-path)))))

(define (extract-log-name ld-list)
  (match (filter (make-event-filter 'finished) ld-list)
    [(list fin)
     ((datum-selector '#:model) fin)]))

(define (extract-names/log-directory dir-path)
  (remove-duplicates
   (map
    extract-log-name
    (map read-logfile 
         (filter
          file-exists?
          (directory-list dir-path
                         #:build? dir-path))))))

(define (make-plot data all-names)
  (parameterize ([plot-x-tick-label-angle 75]
                 [plot-x-tick-label-anchor 'right]
                 [error-bar-width 12]
                 [plot-y-transform (axis-transform-bound log-transform 0.00001 +inf.0)]
                 [plot-legend-anchor 'bottom-right])
    
    (define (error-bar times)
      (define sdev (stddev times #:bias #t))
      (define this-z (if (> (length times) 30)
                         z
                         (hash-ref t-inv-cdf-97.5 (sub1 (length times)))))
      (if (confidence-interval)
          (/ (* z sdev) (sqrt (length times)))
          (/ (stddev times #:bias #t) (sqrt (length times)))))
    
    (define name-avgs (make-hash))
  
    (define (rewrite-name n)
      (regexp-replace #rx"verification-" n "rvm-"))
    
    (for ([b (in-list all-names)])
      (hash-set! name-avgs (rewrite-name b) '()))
    
    (define data-stats
      (let loop ([d data]
                 [sorted-times (hash)])
        (match d
          [(cons (list name type time) rest)
           (loop rest
                 (hash-set sorted-times (cons name type)
                           (cons (/ time 1000)
                                 (hash-ref sorted-times (cons name type)
                                           (λ () '())))))]
          ['()
           (for/list ([(name/type times) (in-hash sorted-times)]
                      #:unless (and ((length times) . < . (min-trials))
                                    (not (equal? (cdr name/type) 'ordered))))
             (define name (rewrite-name (last (regexp-split #rx"/" (car name/type)))))
             (cond 
               [(equal? (cdr name/type) (order-by))
                (hash-set! name-avgs name (mean times))]
               [(list? (hash-ref name-avgs name '()))
                (hash-set! name-avgs 
                           name
                           (cons (mean times) (hash-ref name-avgs name '())))]) 
             (list (car name/type)
                   (cdr name/type)
                   (mean times)
                   (if (equal? (cdr name/type) 'ordered)
                       0
                       (error-bar times))))])))
    
    
  (define name-order 
    ;; this function is mysteriously called a LOT...
    (let ([memo (make-hash)])
      (λ (name)
        (hash-ref memo name
                  (λ ()
                    (define ans
                      (length
                       (or
                        (memf
                         (λ (n) (equal? n name))
                         (sort (sort (hash-keys name-avgs)
                                     string>?)
                               >
                               #:key (λ (k)
                                       (define val (hash-ref name-avgs k))
                                       (cond 
                                         [(number? val) val]
                                         [(empty? val) +inf.0]
                                         [else (mean val)]))))
                        '())))
                    (hash-set! memo name ans)
                    ans)))))
    
    (define (get-name-num name n)
      (+ (if (offset?)
             (+ (/ n 12)
                (- (/ (sub1 (length (types))) 12)))
             0)
         (name-order name)))
    
    (define (plot-type type n)
      (define this-type
        (map (λ (d)
               (define name (rewrite-name (last (regexp-split #rx"/" (car d)))))
               (cons name (cdr d)))
             (filter
              (λ (l)
                (and (equal? type (list-ref l 1))
                     (list-ref l 2)))
              data-stats)))
      (define ps
        (points
         (map
          (λ (l)
            (list (get-name-num (list-ref l 0) n) (list-ref l 2)))
          this-type)
         #:label (string-append (hash-ref type-names type)
                                (format " (~s successes)" (length this-type)))
         #:sym (hash-ref type-symbols type)
         #:size (* (point-size) 1.5)
         #:color (hash-ref type-colors type)))
      (if (equal? type 'ordered)
          ps
          (list (error-bars
                 (map (λ (d)
                        (list (get-name-num (list-ref d 0) n) 
                              (list-ref d 2)
                              (list-ref d 3)))
                      this-type)
                 #:y-min 0.01
                 #:color "dark gray")
                ps)))
    
    (define (zero->neg n)
      (if (zero? n)
          (- 500)
          n))
    
    (define (tlabel pre-tick)
      (define v (pre-tick-value pre-tick))
      (define label-list
        (filter
         (λ (n) (= (name-order n) v))
         (hash-keys name-avgs)))
      (if (empty? label-list) 
          ""
          (car label-list)))
    
    (parameterize ([plot-x-ticks 
                    (ticks (linear-ticks-layout #:number 30 #:base 10
                                                #:divisors '(1))
                           (λ (_1 _2 pts) (map tlabel pts)))]
                   [plot-y-ticks 
                    (log-ticks #:number 20 #:base 10)]
                   [plot-y-label "Average Number of Seconds to Find Each Bug"]
                   [plot-x-label ""])
      
      (if (output-file)
          (plot
           (for/list ([t (if (empty? (types)) all-types (types))]
                      [n (in-naturals)])
             (plot-type t n))
           #:x-min 0
           #:y-min (min-y)
           #:y-max (if (max-t)
                       (* 60 (max-t))
                       (+ 5 (/ (apply max (filter values (map (λ (d) (list-ref d 2)) data))) 1000)))
           #:x-max (+ 0.5 (length (hash-keys name-avgs)))
           #:out-file (output-file)
           #:out-kind 'jpeg)
          (plot-pict
           (for/list ([t (if (empty? (types)) all-types (types))]
                      [n (in-naturals)])
             (plot-type t n))
           #:x-min 0
           #:y-min (min-y)
           #:y-max (if (max-t)
                       (* 60 (max-t))
                       (+ 5 (/ (apply max (filter values (map (λ (d) (list-ref d 2)) data))) 1000)))
           #:x-max (+ 0.5 (length (hash-keys name-avgs))))))))

#;
(define (process-data data)
  (let loop ([d data]
             [sorted-times (hash)]) 
    (match d
      [(cons (list name type time) rest)
       (loop rest
             (hash-set sorted-times (cons name type)
                       (cons (/ time 1000)
                             (hash-ref sorted-times (cons name type)
                                       (λ () '())))))]
      ['()
       (for/fold ([dstats '()] [name-avgs (for/hash ([b (in-list all-names)])
                                            (values b '()))])
         ([(name/type times) (in-hash sorted-times)]
          #:unless (and ((length times) . < . (min-trials))
                        (not (equal? (cdr name/type) 'ordered))))
         (define name (last (regexp-split #rx"/" (car name/type))))
         (values
          (cons (list (car name/type)
                      (cdr name/type)
                      (mean times)
                      (if (equal? (cdr name/type) 'ordered)
                          0
                          (error-bar times))) dstats)
          (cond 
            [(equal? (cdr name/type) (order-by))
             (hash-set name-avgs name (mean times))]
            [(list? (hash-ref name-avgs name '()))
             (hash-set name-avgs 
                       name
                       (cons (mean times) (hash-ref name-avgs name '())))]
            [else name-avgs])))])))

(define min-trials (make-parameter 2))
(define (error-bar times)
  (define sdev (stddev times #:bias #t))
  (define this-z (if (> (length times) 30)
                     z
                     (hash-ref t-inv-cdf-97.5 (sub1 (length times)))))
  (if (confidence-interval)
      (/ (* this-z sdev) (sqrt (length times)))
      (/ (stddev times #:bias #t) (sqrt (length times)))))


(define types (make-parameter '()))
(define names '("adhoc random" "search" "uniform random"
                               "in-order enumeration"))
(define symbols '(circle triangle diamond plus))
(define type-names
  (for/hash ([t all-types] [n names])
    (values t n)))
(define type-symbols
  (for/hash ([t all-types] [s symbols])
    (values t s)))

(define confidence-interval (make-parameter #f))
(define order-by (make-parameter #f))
(define max-t (make-parameter #f))
(define offset? (make-parameter #f))
(define min-y (make-parameter 0.01))

(define output-file (make-parameter #f))

(define z (inv-cdf (normal-dist) 0.975))


(define t-inv-cdf-97.5
  (hash 1 12.706
        2 4.303
        3 3.182
        4 2.776
        5 2.571
        6 2.447
        7 2.365
        8 2.306
        9 2.262
        10 2.228
        11 2.201
        12 2.129
        13 2.160
        14 2.145
        15 2.131
        16 2.120
        17 2.110
        18 2.101
        19 2.093
        20 2.086
        21 2.080
        22 2.074
        23 2.069
        24 2.064
        25 2.060
        26 2.056
        27 2.052
        28 2.048
        29 2.045
        30 2.042))

