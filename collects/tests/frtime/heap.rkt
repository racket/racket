#lang scheme
(require frtime/core/heap
         scheme/package
         tests/eli-tester)

(package-begin
 (define h (make-heap > eq?))
 (test
  (heap? h) => #t
  (heap-empty? h) => #t
  (non-empty-heap? h) => #f
  (heap-insert h 99) => (void)
  (heap-empty? h) => #f
  (non-empty-heap? h) => #t
  (heap-peak h) => 99
  (heap-pop h) => 99
  (heap-empty? h) => #t
  (non-empty-heap? h) => #f
  (heap-contains h 99) => #f
  (heap-insert h 99) => (void)
  (heap-contains h 99) => #t
  (heap-remove h 99) => #t
  (heap-contains h 99) => #f
  (heap-remove h 99) => #f
  (heap-insert h 1) => (void)
  (heap-insert h 2) => (void)
  (heap-insert h 3) => (void)
  (heap-pop h) => 3
  (heap-peak h) => 2
  (heap-pop h) => 2
  (heap-pop h) => 1
  (heap-empty? h) => #t
  (heap-insert h 3) => (void)
  (heap-insert h 1) => (void)
  (heap-insert h 4) => (void)
  (heap-insert h 2) => (void)
  (heap-pop h) => 4
  (heap-pop h) => 3
  (heap-pop h) => 2
  (heap-pop h) => 1
  ))
