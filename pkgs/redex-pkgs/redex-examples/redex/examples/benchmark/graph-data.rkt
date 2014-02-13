#lang racket/base

(require plot
         racket/cmdline
         racket/list
         racket/match
         math/statistics
         math/distributions)

(provide (all-defined-out))


(define types (make-parameter '()))
(define all-types '(grammar search search-gen search-gen-enum search-gen-ref search-gen-enum-ref))
(define names '("grammar" "search" "backjumping" "backjumping, ordered space" "backjumping, with refresh"
                          "backjumping, ordered space with refresh"))
(define symbols '(circle triangle square asterisk diamond plus))
(define type-names
  (for/hash ([t all-types] [n names])
    (values t n)))
(define type-symbols
  (for/hash ([t all-types] [s symbols])
    (values t s)))

(define confidence-interval (make-parameter #f))
(define min-trials (make-parameter 2))
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

(define (make-plot filenames)
  
  (define data
    (let ([raw-data
           (apply append
                  (for/list ([f filenames])
                    (call-with-input-file f
                      (λ (in)
                        (read in)))))])
      (let loop ([fixed-data '()]
                 [rest raw-data])
        (cond
          [(null? rest) fixed-data]
          [((length rest) . >= . 3)
           (loop (cons (take rest 3) fixed-data)
                 (drop rest 3))]
          [else
           (error 'data "is the wrong length!")]))))
  
  
  
  (define (error-bar times)
    (define sdev (stddev times #:bias #t))
    (define this-z (if (> (length times) 30)
                       z
                       (hash-ref t-inv-cdf-97.5 (sub1 (length times)))))
    (if (confidence-interval)
        (/ (* z sdev) (sqrt (length times)))
        (/ (stddev times #:bias #t) (sqrt (length times)))))
  
  (define name-avgs (make-hash))
  
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
                    #:unless ((length times) . < . (min-trials)))
           (when (or
                  (and (not (order-by))
                       (empty? (types))
                       (member (cdr name/type) (types)))
                  (equal? (cdr name/type) (order-by)))
             (hash-set! name-avgs (last (regexp-split #rx"/" (car name/type)))
                        (cons (mean times) (hash-ref name-avgs (last (regexp-split #rx"/" (car name/type))) '()))))
           (list (car name/type)
                 (cdr name/type)
                 (mean times)
                 (error-bar times)))])))
  
  
  (plot-x-tick-label-angle 75)
  (plot-x-tick-label-anchor 'right)
  (plot-font-size 20)
  (error-bar-line-width 3)
  (error-bar-width 12)
  (plot-line-width 3)
  
  (plot-y-transform (axis-transform-bound log-transform 0.00001 +inf.0))
  
  (define (name-order name)
    (length
     (or
      (memf
       (λ (n) (equal? n name))
       (sort (hash-keys name-avgs)
             >
             #:key (λ (k) 
                     (mean (hash-ref name-avgs k)))))
      '())))
  
  (define (get-name-num name n)
    (+ (if (offset?)
           (+ (/ n 12)
              (- (/ (sub1 (length (types))) 12)))
           0)
       (name-order name)))
  
  (define (plot-type type n)
    (define this-type
      (map (λ (d)
             (define name (last (regexp-split #rx"/" (car d))))
             (cons name (cdr d)))
           (filter
            (λ (l)
              (and (equal? type (list-ref l 1))
                   (list-ref l 2)))
            data-stats)))
    (list
     (points
      (map
       (λ (l)
         (list (get-name-num (list-ref l 0) n) (list-ref l 2)))
       this-type)
      #:label (string-append (hash-ref type-names type)
                             (format " (~s successes)" (length this-type)))
      #:sym (hash-ref type-symbols type)
      #:size 20
      #:line-width 2
      #:color (add1 n))
     (error-bars
      (map (λ (d)
             (list (get-name-num (list-ref d 0) n) 
                   (list-ref d 2)
                   (list-ref d 3)))
           this-type)
      #:y-min 0.01
      #:line-width 1
      #:color (length (member type all-types)))))
  
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
  
  (plot-x-ticks (ticks (linear-ticks-layout #:number 30 #:base 10
                                            #:divisors '(1))
                       (λ (_1 _2 pts) (map tlabel pts))))
  
  (define (loggy-ticks _1 _2)
    (for*/list
        ([f (in-list '(1 10 100 1000 10000))]
         [n (in-range 1 10)])
      (pre-tick (* f n) (or
                         (= n 5)
                         (= n 1)))))
  
  (plot-y-ticks 
   (log-ticks #:number 20 #:base 10))
  
  (if (output-file)
      (plot
       (for/list ([t (if (empty? (types)) all-types (types))]
                  [n (in-naturals)])
         (plot-type t n))
       #:y-label "avg. seconds to find bug"
       #:x-label "file"
       #:x-min 0
       #:y-min (min-y)
       #:y-max (if (max-t)
                   (* 60 (max-t))
                   (+ 5 (/ (apply max (filter values (map (λ (d) (list-ref d 2)) data))) 1000)))
       #:x-max (+ 0.5 (length (hash-keys name-avgs)))
       #:legend-anchor 'top-left
       #:width 1024
       #:height 768
       #:out-file (output-file)
       #:out-kind 'jpeg)
      (plot-pict
       (for/list ([t (if (empty? (types)) all-types (types))]
                  [n (in-naturals)])
         (plot-type t n))
       #:y-label "avg. seconds to find bug"
       #:x-label "file"
       #:x-min 0
       #:y-min (min-y)
       #:y-max (if (max-t)
                   (* 60 (max-t))
                   (+ 5 (/ (apply max (filter values (map (λ (d) (list-ref d 2)) data))) 1000)))
       #:x-max (+ 0.5 (length (hash-keys name-avgs)))
       #:legend-anchor 'top-left
       #:width 1024
       #:height 768))
  )


(module+
    main
  (output-file "results.jpg")
  (command-line
   #:once-each
   [("-c" "--confidence-interval") "plot 95% confidence intervals"
                                   (confidence-interval #t)]
   [("-s" "--min-trials") num
                          "minimum number of trials for a data point"
                          (let ([n (string->number num)])
                            (unless (> n 1)
                              (error "min-trials must be greater than 1"))
                            (min-trials n))]
   [("-o" "--order-by") t
                        "type to order plot by"
                        (order-by (string->symbol t))]
   [("-x" "--max") max
                   "graph max in minutes"
                   (max-t (string->number max))]
   [("-m" "--min") min
                   "graph min in seconds"
                   (min-y (string->number min))]
   #:multi
   [("-t" "--type") t
                    "types to plot"
                    (types (cons (string->symbol t) (types)))]
   #:args fnames
   (unless (empty? fnames)
     (make-plot fnames))))
