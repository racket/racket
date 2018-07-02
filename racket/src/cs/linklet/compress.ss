(define (bytevector-compressed? bv)
  (and (> (bytevector-length bv) 8)
       (zero? (bytevector-u8-ref bv 0))))
