#lang racket/base

(require racket/list
         racket/match
         racket/function
         math/statistics
         math/distributions
         plot/no-gui
         (only-in plot/utils known-point-symbols)
         "logging.rkt")

(provide type-colors
         type-names
         type-symbols
         extract-data/log-directory
         extract-log-data
         plot/log-directory
         extract-log-names
         extract-names/log-directory
         process-data)

(define type-colors
  (make-parameter (let ([cur-c 0]
                        [cur-tcs (make-hash)])
                    (λ (type)
                      (hash-ref! cur-tcs type
                                 (begin0
                                   cur-c
                                   (set! cur-c (add1 cur-c))))))))

(define type-symbols
  (make-parameter (let ([syms (drop known-point-symbols 3)]
                        [cur-tss (make-hash)])
                    (λ (type)
                      (hash-ref! cur-tss type
                                 (if (empty? syms)
                                     (error 'type-symbols "no more symbols available!")
                                     (begin0 (car syms)
                                             (set! syms (cdr syms)))))))))

(define type-names (make-parameter symbol->string))

(define (plot/log-directory path)
  (make-plot
   (extract-data/log-directory path)
   (extract-names/log-directory path)))

(define (plot/log-data data)
  (make-plot
   (extract-log-data data)
   (extract-log-names data)))

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

(define (extract-log-names ld-list)
  (remove-duplicates
   (map ((curry datum-selector) '#:model)
        (filter (make-event-filter 'finished) ld-list))))

(define (extract-names/log-directory dir-path)
  (remove-duplicates
   (append-map
    extract-log-names
    (map read-logfile 
         (filter
          file-exists?
          (directory-list dir-path
                          #:build? dir-path))))))
(define (make-plot data all-names)
  (define-values (data-stats name-avgs)
    (process-data data all-names))
  (define name-order (make-name-order name-avgs))
  (define (tlabel pre-tick)
    (define v (pre-tick-value pre-tick))
    (define label-list
      (filter
       (λ (n) (= (name-order n) v))
       (hash-keys name-avgs)))
    (if (empty? label-list) 
        ""
        (car label-list)))
  (parameterize ([plot-x-tick-label-angle 75]
                 [plot-x-tick-label-anchor 'right]
                 [error-bar-width 12]
                 [plot-y-transform (axis-transform-bound log-transform 0.00001 +inf.0)]
                 [plot-legend-anchor 'bottom-right]
                 [plot-x-ticks (ticks (linear-ticks-layout #:number 30 #:base 10
                                              #:divisors '(1))
                         (λ (_1 _2 pts) (map tlabel pts)))]
                 [plot-y-ticks (log-ticks #:number 20 #:base 10)])
    (plot-pict
     (make-plot-renderers/internal data-stats name-avgs)
     #:x-min 0
     #:y-min 0.01
     #:y-max (* 1.1 (/ (apply max (filter values (map (λ (d) (list-ref d 2)) data))) 1000))
     #:x-max (+ 0.5 (length all-names)))))

(define (make-plot-renderers data all-names)
  (define-values (data-stats name-avgs)
    (process-data data all-names))
  (make-plot-renderers/internal data-stats name-avgs))

(define (make-plot-renderers/internal data-stats name-avgs)
  
  (define types (remove-duplicates
                 (map second data-stats)))
  
  (define name-order (make-name-order name-avgs))
  
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
          (list (name-order (list-ref l 0)) (list-ref l 2)))
        this-type)
       #:label (string-append ((type-names) type)
                              (format " (~s successes)" (length this-type)))
       #:sym ((type-symbols) type)
       #:size (* (point-size) 1.5)
       #:color ((type-colors) type)))
    (if (equal? type 'ordered)
        ps
        (list (error-bars
               (map (λ (d)
                      (list (name-order (list-ref d 0)) 
                            (list-ref d 2)
                            (list-ref d 3)))
                    this-type)
               #:y-min 0.01
               #:color "dark gray")
              ps)))
  
  (for/list ([t (in-list types)]
             [n (in-naturals)])
    (plot-type t n)))


(define (process-data data all-names)
  (define name-avgs (make-hash))
  (for ([b (in-list all-names)])
    (hash-set! name-avgs (rewrite-name b) '()))
  (define d-stats
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
                    #:unless (and ((length times) . < . Min-Trials)
                                  (not (equal? (cdr name/type) 'ordered))))
           (define name (rewrite-name (last (regexp-split #rx"/" (car name/type)))))
           (hash-set! name-avgs 
                      name
                      (cons (mean times) (hash-ref name-avgs name '()))) 
           (list (car name/type)
                 (cdr name/type)
                 (mean times)
                 (if (equal? (cdr name/type) 'ordered)
                     0
                     (error-bar times))))])))
  (values d-stats
          name-avgs))

(define (make-name-order name-avgs)
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

(define (rewrite-name n)
  (regexp-replace #rx"verification-" n "rvm-"))

(define Min-Trials 2)

(define (error-bar times)
  (define sdev (stddev times #:bias #t))
  (define this-z (if (> (length times) 30)
                     z
                     (hash-ref t-inv-cdf-97.5 (sub1 (length times)))))
  (/ (* this-z sdev) (sqrt (length times))))

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

