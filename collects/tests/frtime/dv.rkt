#lang scheme
(require frtime/core/dv
         scheme/package
         tests/eli-tester)

(define (dv:ref* d l)
  (for/list ([i (in-list l)])
    (dv:ref d i)))

(test
 (dv:length (dv:make 5)) => 0)

(package-begin
 (define d (dv:make 5))
 (test
  (dv? d) => true
  (dv:append d 1) => (void)
  (dv:length d) => 1
  (dv:ref d 0) => 1
  (dv:set! d 0 2) => (void)
  (dv:ref d 0) => 2
  (dv:remove-last d) => (void)
  (dv:length d) => 0
  (dv:ref d 0) => 0
  
  (dv:append d 1) => (void)
  (dv:append d 2) => (void)
  (dv:append d 3) => (void)
  (dv:ref* d (list 0 1 2)) => (list 1 2 3)
  (dv:set! d 0 4) => (void)
  (dv:ref* d (list 0 1 2)) => (list 4 2 3)
  (dv:remove-last d) => (void)
  (dv:ref* d (list 0 1 2)) => (list 4 2 0)
  (dv:append d 5) => (void)
  (dv:ref* d (list 0 1 2)) => (list 4 2 5)
  
  ))
