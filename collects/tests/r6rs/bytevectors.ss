#!r6rs

(library (tests r6rs bytevectors)
  (export run-bytevectors-tests)
  (import (rnrs)
          (tests r6rs test))

  (define (run-bytevectors-tests)
    
    ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    ;; Tests originally from R6RS
    
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
    
    ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    ;; Tests originally from Ikarus

    (test (bytevector? (make-bytevector 1)) #t)
    (test (bytevector? (make-bytevector 1 17)) #t)
    (test (bytevector? (make-bytevector 10 -17)) #t)
    (test (bytevector? 'foo) #f)
    (test (bytevector? "hey") #f)
    (test (bytevector? '#(2837 2398 239)) #f)
    (test (bytevector-length (make-bytevector 0)) 0)
    (test (bytevector-length (make-bytevector 100 -30)) 100)
    (test (let ((b (u8-list->bytevector '(1 2 3 4 5 6 7 8))))
            (bytevector-copy! b 0 b 3 4)
            (bytevector->u8-list b))
          '(1 2 3 1 2 3 4 8))
    (test (bytevector-uint-ref 
           (u8-list->bytevector '(17))
           0 'little 1)
          17)
    (test (bytevector-uint-ref 
           (u8-list->bytevector '(17))
           0 'big 1)
          17)
    (test (bytevector-uint-ref 
           (u8-list->bytevector '(17 54))
           0 'little 2)
          (+ 17 (* 54 256)))
    (test (bytevector-uint-ref 
           (u8-list->bytevector (reverse '(17 54)))
           0 'big 2)
          (+ 17 (* 54 256)))
    (test (bytevector-uint-ref 
           (u8-list->bytevector '(17 54 98))
           0 'little 3)
          (+ 17 (* 54 256) (* 98 256 256)))
    (test (bytevector-uint-ref 
           (u8-list->bytevector (reverse '(17 54 98)))
           0 'big 3)
          (+ 17 (* 54 256) (* 98 256 256)))
    (test (bytevector-uint-ref 
           (u8-list->bytevector '(17 54 98 120))
           0 'little 4)
          (+ 17 (* 54 256) (* 98 256 256) (* 120 256 256 256)))

    (test (bytevector-uint-ref 
           (u8-list->bytevector
            '(#x89 #x04 #x39 #x82 #x49 #x20 #x93 #x48 #x17
                   #x83 #x79 #x94 #x38 #x87 #x34 #x97 #x38 #x12))
           0 'little 18)
          #x123897348738947983174893204982390489)
    (test (bytevector-uint-ref 
           (u8-list->bytevector
            (reverse
             '(#x89 #x04 #x39 #x82 #x49 #x20 #x93 #x48 #x17
                    #x83 #x79 #x94 #x38 #x87 #x34 #x97 #x38 #x12)))
           0 'big 18)
          #x123897348738947983174893204982390489)
    (test (let ((b (u8-list->bytevector '(1 2 3 255 1 2 1 2))))
            (bytevector->uint-list b 'little 2))
          '(513 65283 513 513))
    (test (let ((b (u8-list->bytevector '(1 2 3 255 1 2 1 2))))
            (bytevector->sint-list b 'little 2))
          '(513 -253 513 513))
    (test (let ((b (make-bytevector 16 -127)))
            (bytevector-uint-set! b 0 (- (expt 2 128) 3) 'little 16)
            (list 
             (bytevector-uint-ref b 0 'little 16)
             (bytevector-sint-ref b 0 'little 16)
             (bytevector->u8-list b)))
          '(#xfffffffffffffffffffffffffffffffd
            -3
            (253 255 255 255 255 255 255 255
                 255 255 255 255 255 255 255 255)))
    (test (let ((b (make-bytevector 16 -127)))
            (bytevector-uint-set! b 0 (- (expt 2 128) 3) 'big 16)
            (list 
             (bytevector-uint-ref b 0 'big 16)
             (bytevector-sint-ref b 0 'big 16)
             (bytevector->u8-list b)))
          '(#xfffffffffffffffffffffffffffffffd
            -3
            (255 255 255 255 255 255 255 255
                 255 255 255 255 255 255 255 253)))
    (test (bytevector->u8-list '#vu8(1 2 3 4))
          '(1 2 3 4))
    (test (let ((b (make-bytevector 4 0)))
            (bytevector-sint-set! b 0 -1 'little 4)
            (bytevector-uint-ref b 0 'little 4))
          #xFFFFFFFF)
    (test (let ((b (make-bytevector 4 0)))
            (bytevector-sint-set! b 0 -256 'little 4)
            (bytevector-uint-ref b 0 'little 4))
          #xFFFFFF00)
    (test (let ((b (make-bytevector 4 0)))
            (bytevector-sint-set! b 0 (- (expt 256 2)) 'little 4)
            (bytevector-uint-ref b 0 'little 4))
          #xFFFF0000)
    (test (let ((b (make-bytevector 8 0)))
            (bytevector-sint-set! b 0 (- (expt 256 2)) 'little 8)
            (bytevector-uint-ref b 0 'little 8))
          #xFFFFFFFFFFFF0000)
    (test (let ((b (make-bytevector 8 0)))
            (bytevector-sint-set! b 0 (- (expt 256 4)) 'little 8)
            (bytevector-uint-ref b 0 'little 8))
          #xFFFFFFFF00000000)
    (test (let ((b (make-bytevector 8 0)))
            (bytevector-sint-set! b 0 (- (expt 256 7)) 'little 8)
            (bytevector-uint-ref b 0 'little 8))
          #xFF00000000000000)
    (test (let ((b (make-bytevector 8 0)))
            (bytevector-sint-set! b 0 (- 1 (expt 2 63)) 'little 8)
            (bytevector-sint-ref b 0 'little 8))
          (- 1 (expt 2 63)))
    (test (let ((b (make-bytevector 4 38)))
            (bytevector-sint-set! b 0 (- (expt 2 31) 1) 'little 4)
            (bytevector-sint-ref b 0 'little 4))
          #x7FFFFFFF)
    (test (let ((b (make-bytevector 4 38)))
            (bytevector-sint-set! b 0 (- (expt 2 31)) 'little 4)
            (bytevector-sint-ref b 0 'little 4))
          #x-80000000)
    (test (let ((b (make-bytevector 5 38)))
            (bytevector-sint-set! b 0 (- (expt 2 32)) 'little 5)
            (bytevector-sint-ref b 0 'little 5))
          #x-100000000)
    (test (let ((b (make-bytevector 4 0)))
            (bytevector-sint-set! b 0 -1 'big 4)
            (bytevector-uint-ref b 0 'big 4))
          #xFFFFFFFF)
    (test (let ((b (make-bytevector 4 0)))
            (bytevector-sint-set! b 0 -256 'big 4)
            (bytevector-uint-ref b 0 'big 4))
          #xFFFFFF00)
    (test (let ((b (make-bytevector 4 0)))
            (bytevector-sint-set! b 0 (- (expt 256 2)) 'big 4)
            (bytevector-uint-ref b 0 'big 4))
          #xFFFF0000)
    (test (let ((b (make-bytevector 8 0)))
            (bytevector-sint-set! b 0 (- (expt 256 2)) 'big 8)
            (bytevector-uint-ref b 0 'big 8))
          #xFFFFFFFFFFFF0000)
    (test (let ((b (make-bytevector 8 0)))
            (bytevector-sint-set! b 0 (- (expt 256 4)) 'big 8)
            (bytevector-uint-ref b 0 'big 8))
          #xFFFFFFFF00000000)
    (test (let ((b (make-bytevector 8 0)))
            (bytevector-sint-set! b 0 (- (expt 256 7)) 'big 8)
            (bytevector-uint-ref b 0 'big 8))
          #xFF00000000000000)
    (test (let ((b (make-bytevector 8 0)))
            (bytevector-sint-set! b 0 (- 1 (expt 2 63)) 'big 8)
            (bytevector-sint-ref b 0 'big 8))
          (- 1 (expt 2 63)))
    (test (let ((b (make-bytevector 4 38)))
            (bytevector-sint-set! b 0 (- (expt 2 31) 1) 'big 4)
            (bytevector-sint-ref b 0 'big 4))
          #x7FFFFFFF)
    (test (let ((b (make-bytevector 4 38)))
            (bytevector-sint-set! b 0 (- (expt 2 31)) 'big 4)
            (bytevector-sint-ref b 0 'big 4))
          #x-80000000)
    (test (let ((b (make-bytevector 5 38)))
            (bytevector-sint-set! b 0 (- (expt 2 32)) 'big 5)
            (bytevector-sint-ref b 0 'big 5))
          #x-100000000)
    (test (bytevector-u16-ref '#vu8(255 253) 0 'little)
          65023)
    (test (bytevector-u16-ref '#vu8(255 253) 0 'big)
          65533)
    (test (bytevector-s16-ref '#vu8(255 253) 0 'little)
          -513)
    (test (bytevector-s16-ref '#vu8(255 253) 0 'big)
          -3)
    (test (let ((v (make-bytevector 2)))
            (bytevector-u16-native-set! v 0 12345)
            (bytevector-u16-native-ref v 0))
          12345)
    (test (let ((v (make-bytevector 2)))
            (bytevector-u16-set! v 0 12345 'little)
            (bytevector-u16-ref v 0 'little))
          12345)
    (test (let ((v (make-bytevector 2)))
            (bytevector-u16-set! v 0 12345 'big)
            (bytevector-u16-ref v 0 'big))
          12345)

    ;;
    ))
