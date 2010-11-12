#lang racket/base
(require racket/port)

(provide read-bmp)

(define BI_RGB 0)
(define BI_RLE8 1)
(define BI_RLE4 2)

(define (read-byte* in)
  (let ([c (read-byte in)])
    (if (eof-object? c)
        (error 'read-bmp "unexpected end of BMP stream: ~e" in)
        c)))

(define (int4 in)
  (+ (read-byte* in)
     (arithmetic-shift (read-byte* in) 8)
     (arithmetic-shift (read-byte* in) 16)
     (arithmetic-shift (read-byte* in) 24)))

(define (int2 in)
  (+ (read-byte* in)
     (arithmetic-shift (read-byte* in) 8)))

(define (make-rle8-port in)
  (make-input-port/read-to-peek
   (object-name in)
   (let ([remaining 0]
         [value 0]
         [absolute 0]
         [abs-skip? #f])
     (lambda (bstr)
       (cond
        [(positive? absolute)
         (let ([v (read-byte* in)])
           (set! absolute (sub1 absolute))
           (when (zero? absolute)
             (when abs-skip?
               (read-byte* in)))
           (bytes-set! bstr 0 v)
           1)]
        [(zero? remaining)
         (let ([r (read-byte in)])
           (if (eof-object? r)
               r
               (if (zero? r)
                   ;; special:
                   (let ([c (read-byte* in)])
                     (case c
                       [(0) 0] ; end-of-line
                       [(1) 0] ; end-of-bitmap
                       [(2) (error 'read-bmp 
                                   "RLE8 cursor command not supported in stream: ~e" 
                                   in)]
                       [else
                        (set! absolute c)
                        (set! abs-skip? (odd? c))
                        0]))
                   ;; normal encoding:
                   (let ([v (read-byte* in)])
                     (set! remaining (sub1 r))
                     (set! value v)
                     (bytes-set! bstr 0 v)
                     1))))]
        [else
         (set! remaining (sub1 remaining))
         (bytes-set! bstr 0 value)
         1])))
   #f
   void))

(define (make-rle4-port in)
  (make-input-port/read-to-peek
   (object-name in)
   (let ([remaining 0]
         [value 0]
         [absolute 0]
         [abs-skip? #f]
         [nibble #f]) ; leftover half-byte to be delivered
     (lambda (bstr)
       (let ([nibbles
              (cond
               [(positive? absolute)
                (let ([v (read-byte* in)])
                  (set! absolute (- absolute 2))
                  (when (absolute . < . 1)
                    (when abs-skip?
                      (read-byte* in)))
                  (bytes-set! bstr 0 v)
                  (if (absolute . < . 0)
                      1
                      2))]
               [(zero? remaining)
                (let ([r (read-byte in)])
                  (if (eof-object? r)
                      r
                      (if (zero? r)
                          ;; special:
                          (let ([c (read-byte* in)])
                            (case c
                              [(0) 0] ; end-of-line
                              [(1) 0] ; end-of-bitmap
                              [(2) (error 'read-bmp 
                                          "RLE8 cursor command not supported in stream: ~e" 
                                          in)]
                              [else
                               (set! absolute c)
                               (set! abs-skip? (positive? (bitwise-and c 3)))
                               0]))
                          ;; normal encoding:
                          (let ([v (read-byte* in)])
                            (set! remaining r)
                            (set! value v)
                            0))))]
               [(= remaining 1)
                (bytes-set! bstr 0 value)
                1]
               [else
                (set! remaining (- remaining 2))
                (bytes-set! bstr 0 value)
                2])])
         (cond
          [(eof-object? nibbles)
           (if nibble
               (begin
                 (bytes-set! bstr 0 (arithmetic-shift nibble 4))
                 (set! nibble #f)
                 1)
               nibbles)]
          [(zero? nibbles)
           0]
          [(and (not nibble) (= 2 nibbles))
           1]
          [(and (not nibble) (= 1 nibbles))
           (set! nibble (bitwise-and (bytes-ref bstr 0) #xF0))
           0]
          [(and nibble (= 1 nibbles))
           (bytes-set! bstr 0 (bitwise-ior nibble
                                           (arithmetic-shift (bytes-ref bstr 0) -4)))
           (set! nibble #f)
           1]
          [else ;; (and nibble (= 2 nibbles))
           (let ([old nibble])
             (set! nibble (arithmetic-shift (bitwise-and (bytes-ref bstr 0) #xF) 4))
             (bytes-set! bstr 0 (bitwise-ior old
                                             (arithmetic-shift (bytes-ref bstr 0) -4)))
             1)]))))
   #f
   void))

(define (read-bmp in)
  (unless (and (= (read-byte* in) (char->integer #\B))
               (= (read-byte* in) (char->integer #\M)))
    (error 'read-bmp "not a BMP stream: ~e" in))
  (let ([file-size (int4 in)]
        [reserved1 (int2 in)]
        [reserved2 (int2 in)]
        [offset (int4 in)])
    ;; Start DIB header
    (let ([header-size (int4 in)])
      (unless (or (= header-size 40)
                  (= header-size 12))
        (error 'read-bmp "expected a 12- or 40-byte DIB header, got ~a in stream: ~e" header-size in))
      (let-values ([(width height bits-per-pixel compression color-count padded-rgb?)
                    (case header-size
                      [(40)
                       (let ([width (int4 in)]
                             [height (int4 in)]
                             [planes (int2 in)]
                             [bits-per-pixel (int2 in)]
                             [compression (int4 in)]
                             [image-size (int4 in)]
                             [hres (int4 in)]
                             [vres (int4 in)]
                             [color-count (int4 in)]
                             [colors-used (int4 in)])
                         (unless (or (= compression BI_RGB)
                                     (= compression BI_RLE4)
                                     (= compression BI_RLE8))
                           (error 'read-bmp "unsupported compression type ~a in stream: ~e" compression in))
                         (values width height bits-per-pixel compression color-count #t))]
                      [(12)
                       (let ([width (int2 in)]
                             [height (int2 in)]
                             [planes (int2 in)]
                             [bits-per-pixel (int2 in)])
                       (values width height bits-per-pixel BI_RGB 0 #f))])])
        (let* ([color-count (if (zero? color-count)
                                (arithmetic-shift 1 bits-per-pixel)
                                color-count)]
               [colors
                (if (bits-per-pixel . >= . 16)
                    #f
                    (let ([vec (make-vector color-count #"\0\0\0\xFF")])
                      (for ([i (in-range color-count)])
                        (let ([b (read-byte* in)]
                              [g (read-byte* in)]
                              [r (read-byte* in)])
                          (when padded-rgb? (read-byte* in))
                          (vector-set! vec i (bytes r g b 255))))
                      vec))]
               [current-pos (+ 14
                               header-size
                               (if colors (* color-count (if padded-rgb? 4 3)) 0))])
          ;; Image data:
          (read-bytes (- offset current-pos) in)
          (let ([in (cond
                     [(= compression BI_RLE4) (make-rle4-port in)]
                     [(= compression BI_RLE8) (make-rle8-port in)]
                     [else in])])
            (values
             width
             height
             (list->vector
              (reverse
               (for/list ([j (in-range height)])
                 (let* ([row (make-bytes (* 4 width) 255)]
                        [install-color!
                         (lambda (i c)
                           (if (c . < . color-count)
                               (let ([col (vector-ref colors c)])
                                 (bytes-set! row (* i 4) (bytes-ref col 0))
                                 (bytes-set! row (+ 1 (* i 4)) (bytes-ref col 1))
                                 (bytes-set! row (+ 2 (* i 4)) (bytes-ref col 2)))
                               (error 'read-bmp "bad color table index ~a in stream: ~e" c in)))])
                   (case bits-per-pixel
                     [(32)
                      (for ([i (in-range width)])
                        (let ([b (read-byte* in)]
                              [g (read-byte* in)]
                              [r (read-byte* in)])
                          (read-byte* in) ; discard
                          (bytes-set! row (* i 4) r)
                          (bytes-set! row (+ 1 (* i 4)) g)
                          (bytes-set! row (+ 2 (* i 4)) b)))]
                     [(24)
                      (for ([i (in-range width)])
                        (let ([b (read-byte* in)]
                              [g (read-byte* in)]
                              [r (read-byte* in)])
                          (bytes-set! row (* i 4) r)
                          (bytes-set! row (+ 1 (* i 4)) g)
                          (bytes-set! row (+ 2 (* i 4)) b)))]
                     [(16)
                      (for ([i (in-range width)])
                        (let ([col (bitwise-ior (read-byte* in)
                                                (arithmetic-shift (read-byte* in) 8))])
                          (bytes-set! row (* i 4) (arithmetic-shift (bitwise-and col #x7C00) -7))
                          (bytes-set! row (+ 1 (* i 4)) (arithmetic-shift (bitwise-and col #x3E0) -2))
                          (bytes-set! row (+ 2 (* i 4)) (arithmetic-shift (bitwise-and col #x1F) 3))))]
                     [(8)
                      (for ([i (in-range width)])
                        (install-color! i (read-byte* in)))]
                     [(4)
                      (for/fold ([b 0]) ([i (in-range width)])
                        (let ([b (if (zero? (bitwise-and i 1))
                                     (read-byte* in)
                                     (arithmetic-shift b 4))])
                          (install-color! i (arithmetic-shift (bitwise-and b #xF0) -4))
                          b))]
                     [(1)
                      (for/fold ([b 0]) ([i (in-range width)])
                        (let ([b (if (zero? (bitwise-and i 7))
                                     (read-byte* in)
                                     (arithmetic-shift b 1))])
                          (install-color! i (arithmetic-shift (bitwise-and b #x80) -7))
                          b))]
                     [else
                      (error 'read-bmp "unsupported bits-per-pixel count ~a in stream: ~e"
                             bits-per-pixel in)])
                   ;; skip padding, if any:
                   (when (= compression BI_RGB)
                     (let ([n (modulo (ceiling (/ (* width bits-per-pixel) 8)) 4)])
                       (unless (zero? n)
                         (read-bytes (- 4 n) in))))
                   row)))))))))))
