#lang scheme
(require frtime/erl)

(define t (spawn/name 'test (receive [#f (error 'test "Got an #f")])))

(! t #t)