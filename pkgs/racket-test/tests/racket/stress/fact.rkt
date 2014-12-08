#lang racket/base

(void (let fact ([n 100000])
        (cond [(zero? n) 1]
              [else (* n (fact (- n 1)))])))
