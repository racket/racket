(module |66| mzscheme 
  (provide (rename bytes? u8vector?)
           (rename make-bytes make-u8vector)
           (rename bytes u8vector)
           (rename bytes->list u8vector->list)
           (rename list->bytes list->u8vector)
           (rename bytes-length u8vector-length)
           (rename bytes-ref u8vector-ref)
           (rename bytes-set! u8vector-set!)
           (rename bytes-copy u8vector-copy)
           u8vector=?
           u8vector-compare
           u8vector-copy!)
 
  (define (u8vector=? v1 v2)
    (bytes=? v1 v2))
  
  (define (u8vector-compare v1 v2)
    (cond ((bytes<? v1 v2) -1)
          ((bytes>? v1 v2)  1)
          (else             0)))
    
  (define (u8vector-copy! src src-start dest dest-start n)
    (bytes-copy! dest dest-start src src-start (+ src-start n))))