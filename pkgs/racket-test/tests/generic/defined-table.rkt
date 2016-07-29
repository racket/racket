#lang racket/base

(require racket/generic)

(define-generics cost
  (money-cost cost)
  (time-cost cost)
  (happiness-cost cost)
  #:defined-table cost-types)

(struct food (name price calories)
  #:methods gen:cost
  [(define (money-cost x)
     (food-price x))
   (define (happiness-cost x)
     ;; Guilt from eating too many calories
     (/ (- (max (food-calories x) 1000) 1000) 200))])

(struct laundry (num-loads)
  #:methods gen:cost
  [(define (money-cost x)
     (* (laundry-num-loads x) 3))
   (define (time-cost x)
     ;; It's really annoying to start laundry, but subsequent loads are okay
     (+ 5 (laundry-num-loads x)))])

(define methods
  (hash 'money-cost money-cost
        'time-cost time-cost
        'happiness-cost happiness-cost))

(define (total-cost stuff)
  (for/sum ([thing stuff])
    (define supported-costs (cost-types thing))
    (for/sum ([cost-type (hash-keys methods)])
      (if (hash-ref supported-costs cost-type)
          ((hash-ref methods cost-type) thing)
          0))))

(module+ test
  (require rackunit rackunit/text-ui racket/port)

  (define (check-basics table)
    (check-true (hash? table))
    (for ([method (hash-keys table)])
      (check-true (symbol? method))
      (check-true (boolean? (hash-ref table method)))))

  (define pizza (food 'pizza 8 1200))
  (define stuff (list pizza (laundry 1)))
  (parameterize {[current-output-port (open-output-nowhere)]}
    (run-tests
     (test-suite
      "defined-table"
      (check-basics (cost-types pizza))
      (check-true (hash-ref (cost-types pizza) 'money-cost))
      (check-false (hash-ref (cost-types pizza) 'time-cost))
      (check-true (hash-ref (cost-types pizza) 'happiness-cost))

      (check-basics (cost-types (car stuff)))
      (check-basics (cost-types (cadr stuff)))
      (check-equal? (total-cost stuff) 18)))
    (void)))
