#lang racket/base

(provide parse-logger-args)

(define (parse-logger-args str)
  (define levels '(none fatal error warning info debug))
  (define arglist
    (let loop ([args (regexp-split #rx" +" str)]
               [first? #t]
               [result '()])
      (cond
        [(null? args) result]
        [(equal? (car args) "") (loop (cdr args) first? result)]
        [else
         (define arg (car args))
         (cond
           [(and first? (member (string->symbol arg) levels))
            (loop (cdr args) #f (list* (string->symbol arg) #f result))]
           [(regexp-match #rx"^(.*)@([^@]*)$" arg)
            =>
            (λ (m)
              (define level (string->symbol (list-ref m 1)))
              (define name (string->symbol (list-ref m 2)))
              (cond
                [(member level levels) 
                 (loop (cdr args) #f (list* level name result))]
                [else #f]))]
           [else #f])])))
  (if (null? arglist)
      #f
      arglist))

(module+ test 
  (require rackunit)
  (check-equal? (parse-logger-args "") #f)
  (check-equal? (parse-logger-args "    ") #f)
  (check-equal? (parse-logger-args "info") '(info #f))
  (check-equal? (parse-logger-args "debug") '(debug #f))
  (check-equal? (parse-logger-args " info ") '(info #f))
  (check-equal? (parse-logger-args " info ") '(info #f))
  (check-equal? (parse-logger-args "info debug@GC") '(debug GC info #f))
  (check-equal? (parse-logger-args "info   debug@GC  ") '(debug GC info #f))
  (check-equal? (parse-logger-args "info   debug@GC@  ") #f)
  (check-equal? (parse-logger-args "info   debug@GC none@GC@  ") #f)
  (check-equal? (parse-logger-args "info   debug@GC none@GC  ") '(none GC debug GC info #F))
  (check-equal? (parse-logger-args " debug@GC  ") '(debug GC)))
