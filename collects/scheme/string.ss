#lang scheme/base

(provide string-append*)

(define string-append*
  (case-lambda [(strs) (apply string-append strs)] ; optimize common case
               [(str . strss) (apply string-append (apply list* str strss))]))
