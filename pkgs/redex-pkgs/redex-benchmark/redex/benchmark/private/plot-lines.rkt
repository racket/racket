#lang racket/base

(require racket/list
         plot/no-gui
         "graph-data.rkt")

(provide line-plot
         line-plot-renderer/log-directory
         line-plot-renderer)

(define (line-plot dir [output #f])
  (parameterize ([plot-x-transform log-transform]
                 [plot-x-label "Time in Seconds"]
                 [plot-y-label "Number of Bugs Found"]
                 [plot-width 600] 
                 [plot-height 300]
                 [plot-x-ticks (log-ticks #:number 20 #:base 10)])
    (if output
        (plot-file (line-plot-renderer/log-directory dir)
                   output
                   #:x-min 0.05)
        (plot-pict (line-plot-renderer/log-directory dir)
                   #:x-min 0.05))))

(define (line-plot-renderer/log-directory dir)
  (define-values (data _)
    (process-data (extract-data/log-directory dir)
                  (extract-names/log-directory dir)))
  (make-renderers data))

(define (line-plot-renderer log-data)
  (define-values (data _)
    (process-data (extract-log-data log-data)
                  (extract-log-names log-data)))
  (make-renderers data))
                    
(define line-styles
  (list 'solid 'dot 'long-dash
        'short-dash 'dot-dash))

(define (make-renderers stats)
  (define max-t (apply max (map third stats)))
  
  ;; (listof (list/c type data))
  (define types+datas
    (for/list ([(type avgs) (in-hash (sort-stats stats))]
               [n (in-naturals)])
      (define pts 
        (for/fold ([l '()]) 
          ([a (sort avgs <)]
           [c (in-naturals)])
          (cons (list a (add1 c))
                (cons 
                 (list a c) 
                 l))))
      (list type (reverse (cons (list max-t (/ (length pts) 2)) pts)))))
  
  (unless (= 3 (length types+datas)) 
    (error 'plot-lines.rkt "ack: assuming that there are only three competitors"))
  (define-values (_ crossover-points)
    (for/fold ([last-winner #f]
               [crossover-points '()])
      ([grammar-pr (in-list (list-ref (assoc 'grammar types+datas) 1))]
       [enum-pr (in-list (list-ref (assoc 'enum types+datas) 1))]
       [ordered-pr (in-list (list-ref (assoc 'ordered types+datas) 1))])
      (unless (and (= (list-ref grammar-pr 1)
                      (list-ref enum-pr 1))
                   (= (list-ref grammar-pr 1)
                      (list-ref ordered-pr 1)))
        (error 'plot-lines.rkt "ack: expected points to match up ~s ~s ~s"
               grammar-pr
               enum-pr
               ordered-pr))
      (define y-position (list-ref grammar-pr 1))
      (define grammar-time (list-ref grammar-pr 0))
      (define enum-time (list-ref enum-pr 0))
      (define ordered-time (list-ref ordered-pr 0))
      (define best (min grammar-time enum-time ordered-time))
      (define current-winner
        (cond
          [(= grammar-time best) 'grammar]
          [(= ordered-time best) 'ordered]
          [(= enum-time best) 'enum]))
      (values current-winner
              (cond
                [(and last-winner (not (equal? last-winner current-winner)))
                 (cons (point-label (vector best y-position)
                                    (format "~a, ~a"
                                            (number+unit/s y-position "bug")
                                            (format-time best))
                                    #:anchor 'bottom-right)
                       crossover-points)]
                [else
                 crossover-points]))))
  (append 
   crossover-points
   (for/list ([type+pts (in-list types+datas)]
              [n (in-naturals)])
     (define type (list-ref type+pts 0))
     (define pts (list-ref type+pts 1))
     (lines
      (reverse pts)
      ;#:width 2
      #:color ((type-colors) type)
      #:style (list-ref line-styles n)
      #:label ((type-names) type)))))

(define (format-time number)
  (cond
    [(<= number 60) (number+unit/s number "second")]
    [(<= number (* 60 60)) (number+unit/s (/ number 60) "minute")]
    [(<= number (* 60 60 24)) (number+unit/s (/ number 60 60) "hour")]
    [else (number+unit/s (/ number 60 60 24) "day")]))

(define (number+unit/s raw-n unit) 
  (define n (round raw-n))
  (format "~a ~a~a" n unit (if (= n 1) "" "s")))

(module+ test
  (require rackunit)
  (check-equal? (format-time 0) "0 seconds")
  (check-equal? (format-time 1) "1 second")
  (check-equal? (format-time 59) "59 seconds")
  (check-equal? (format-time 70) "1 minute")
  (check-equal? (format-time 110) "2 minutes")
  (check-equal? (format-time (* 60 60 2)) "2 hours")
  (check-equal? (format-time (* 60 60 #e2.2)) "2 hours")
  (check-equal? (format-time (* 60 60 #e8.2)) "8 hours")
  (check-equal? (format-time (* 60 60 24 3)) "3 days"))

(define (sort-stats stats)
  (for/fold ([h (hash)])
    ([s (in-list stats)])
    (hash-set h (second s)
              (cons (third s)
                    (hash-ref h (second s) '())))))