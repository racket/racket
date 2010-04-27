#lang scheme/base

(for/fold ([acc 0])
    ([n (in-lines)])
  (+ acc (string->number n)))
