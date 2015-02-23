#lang racket/base
(require file/md5 tests/eli-tester)

(provide tests)

(module+ main (tests))
(define (tests)
  (test
   (md5 #"")
   => #"d41d8cd98f00b204e9800998ecf8427e"
   (md5 "")
   => #"d41d8cd98f00b204e9800998ecf8427e"
   (md5 (open-input-bytes #""))
   => #"d41d8cd98f00b204e9800998ecf8427e"
   (md5 #"a")
   => #"0cc175b9c0f1b6a831c399e269772661"
   (md5 #"abc")
   => #"900150983cd24fb0d6963f7d28e17f72"
   (md5 #"message digest")
   => #"f96b697d7cb7938d525a2f31aaf161d0"
   (md5 #"abcdefghijklmnopqrstuvwxyz")
   => #"c3fcd3d76192e4007dfb496cca67e13b"
   (md5 #"ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789")
   => #"d174ab98d277d9f5a5611c2c9f419d9f"
   (md5 (let* ([bs #"1234567890"]) (bytes-append bs bs bs bs bs bs bs bs)))
   => #"57edf4a22be3c955ac49da2e2107b67a"
   (md5 #"" #f)
   => #"\324\35\214\331\217\0\262\4\351\200\t\230\354\370B~"
   ))
