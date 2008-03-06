#!r6rs

(library (tests r6rs bytevectors)
  (export run-bytevectors-tests)
  (import (rnrs)
          (tests r6rs test))

  (define (run-bytevectors-tests)
    
    (test (let ((b (u8-list->bytevector '(1 2 3 4 5 6 7 8))))
            (bytevector-copy! b 0 b 3 4)
            (bytevector->u8-list b)) 
          '(1 2 3 1 2 3 4 8))

    (test (let ((b1 (make-bytevector 16 -127))
                (b2 (make-bytevector 16 255)))
            (list
             (bytevector-s8-ref b1 0)
             (bytevector-u8-ref b1 0)
             (bytevector-s8-ref b2 0)
             (bytevector-u8-ref b2 0)))
          '(-127 129 -1 255))

    (test (let ((b (make-bytevector 16 -127)))
            
            (bytevector-s8-set! b 0 -126)
            (bytevector-u8-set! b 1 246)
            
            (list
             (bytevector-s8-ref b 0)
             (bytevector-u8-ref b 0)
             (bytevector-s8-ref b 1)
             (bytevector-u8-ref b 1)))
          '(-126 130 -10 246))

    (let ([b (make-bytevector 16 -127)])
      (test/unspec
       (bytevector-uint-set! b 0 (- (expt 2 128) 3)
                             (endianness little) 16))
       
      (test (bytevector-uint-ref b 0 (endianness little) 16)
            #xfffffffffffffffffffffffffffffffd)

      (test (bytevector-sint-ref b 0 (endianness little) 16)
            -3)

      (test (bytevector->u8-list b)
            '(253 255 255 255 255 255 255 255
                  255 255 255 255 255 255 255 255))

      (test/unspec (bytevector-uint-set! b 0 (- (expt 2 128) 3)
                                         (endianness big) 16))
      (test (bytevector-uint-ref b 0 (endianness big) 16)
            #xfffffffffffffffffffffffffffffffd)

      (test (bytevector-sint-ref b 0 (endianness big) 16) -3)

      (test (bytevector->u8-list b) 
            '(255 255 255 255 255 255 255 255
                  255 255 255 255 255 255 255 253))

      (test
       (let ((b (u8-list->bytevector '(1 2 3 255 1 2 1 2))))
         (bytevector->sint-list b (endianness little) 2)) 
       '(513 -253 513 513))

      (test (let ((b (u8-list->bytevector '(1 2 3 255 1 2 1 2))))
              (bytevector->uint-list b (endianness little) 2)) 
            '(513 65283 513 513)))

    (let ([b (u8-list->bytevector
              '(255 255 255 255 255 255 255 255
                    255 255 255 255 255 255 255 253))])
      
      (test (bytevector-u16-ref b 14 (endianness little)) 65023)
      (test (bytevector-s16-ref b 14 (endianness little)) -513)
      (test (bytevector-u16-ref b 14 (endianness big)) 65533)
      (test (bytevector-s16-ref b 14 (endianness big)) -3)

      (test/unspec (bytevector-u16-set! b 0 12345 (endianness little)))
      (test (bytevector-u16-ref b 0 (endianness little)) 12345)
      
      (test/unspec (bytevector-u16-native-set! b 0 12345))
      (test (bytevector-u16-native-ref b 0) 12345)

      (test/unspec (bytevector-u16-ref b 0 (endianness little))))

    (let ([b (u8-list->bytevector
              '(255 255 255 255 255 255 255 255
                    255 255 255 255 255 255 255 253))])

      (test (bytevector-u32-ref b 12 (endianness little)) 4261412863)
      (test (bytevector-s32-ref b 12 (endianness little)) -33554433)
      (test (bytevector-u32-ref b 12 (endianness big)) 4294967293)
      (test (bytevector-s32-ref b 12 (endianness big)) -3))

    (let ([b (u8-list->bytevector
              '(255 255 255 255 255 255 255 255
                    255 255 255 255 255 255 255 253))])
      (test (bytevector-u64-ref b 8 (endianness little)) 18302628885633695743)
      (test (bytevector-s64-ref b 8 (endianness little)) -144115188075855873)
      (test (bytevector-u64-ref b 8 (endianness big)) 18446744073709551613)
      (test (bytevector-s64-ref b 8 (endianness big)) -3))

    ;;
    ))



