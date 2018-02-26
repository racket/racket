#lang racket/base

(time
 (let loop ([j 10])
   (unless (zero? j)
     (let ()
       (define p (open-input-file "compiled/io.rktl"))
       (port-count-lines! p)
       (let loop ()
         (define s (read-string 100 p))
         (unless (eof-object? s)
           (loop)))
       (close-input-port p)
       (loop (sub1 j))))))

(time
 (let loop ([j 10])
   (unless (zero? j)
     (let ()
       (define p (open-input-file "compiled/io.rktl"))
       (port-count-lines! p)
       (let loop ()
         (unless (eof-object? (read-byte p))
           (loop)))
       (close-input-port p)
       (loop (sub1 j))))))

'read-line
(time
 (let loop ([j 10])
   (unless (zero? j)
     (let ()
       (define p (host:open-input-file "compiled/io.rktl"))
       (let loop ()
         (unless (eof-object? (host:read-line p))
           (loop)))
       (host:close-input-port p)
       (loop (sub1 j))))))

(time
 (let loop ([j 10])
   (unless (zero? j)
     (let ()
       (define p (open-input-file "compiled/io.rktl"))
       (let loop ()
         (unless (eof-object? (read-line p))
           (loop)))
       (close-input-port p)
       (loop (sub1 j))))))
