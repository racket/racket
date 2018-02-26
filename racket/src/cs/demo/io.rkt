#lang racket/base
(require "io-impl.rkt"
         (only-in racket/base
                  [open-input-file c:open-input-file]
                  [port-count-lines! c:port-count-lines!]
                  [read-string c:read-string]
                  [close-input-port c:close-input-port]
                  [bytes->string/utf-8 c:bytes->string/utf-8]
                  [string->bytes/utf-8 c:string->bytes/utf-8]))

(time
 (let loop ([j 10])
   (unless (zero? j)
     (let ()
       (define p (open-input-file "compiled/io.scm"))
       (port-count-lines! p)
       (let loop ()
         (define s (read-string 100 p))
         (unless (eof-object? s)
           (loop)))
       (close-input-port p)
       (loop (sub1 j))))))

'|Same, but in C....|
(time
 (let loop ([j 10])
   (unless (zero? j)
     (let ()
       (define p (c:open-input-file "compiled/io.scm"))
       (c:port-count-lines! p)
       (let loop ()
         (define s (c:read-string 100 p))
         (unless (eof-object? s)
           (loop)))
       (c:close-input-port p)
       (loop (sub1 j))))))


(time
 (let loop ([j 10])
   (unless (zero? j)
     (let ()
       (define p (open-input-file "compiled/io.scm"))
       (port-count-lines! p)
       (let loop ()
         (unless (eof-object? (read-byte p))
           (loop)))
       (close-input-port p)
       (loop (sub1 j))))))

(time
 (let loop ([i 1000000] [v #f])
   (if (zero? i)
       v
       (loop (sub1 i)
             (bytes->string/utf-8 (string->bytes/utf-8 "ap\x3BB;ple"))))))

'|Same, but in C...|
(time
 (let loop ([i 1000000] [v #f])
   (if (zero? i)
       v
       (loop (sub1 i)
             (c:bytes->string/utf-8 (c:string->bytes/utf-8 "ap\x3BB;ple"))))))
