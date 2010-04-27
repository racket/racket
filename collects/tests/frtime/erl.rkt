#lang scheme
(require frtime/core/erl
         tests/eli-tester)

(define ch (make-channel))
(define t
  (spawn/name 
   'test
   (let loop ()
     (receive 
      [after 10 
             (channel-put ch "Timeout")
             (loop)]
      ['self
       (channel-put ch (self))
       (loop)]
      [v
       (channel-put ch v)
       (loop)]))))

(test
 (! t #t) => (void)
 (channel-get ch) => #t
 (! t (list 1 2)) => (void)
 (channel-get ch) => (list 1 2)
 (! t 'self) => (void)
 (channel-get ch) => #s(tid test)
 (self) => #s(tid thread1))
