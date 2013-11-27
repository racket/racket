#lang racket/base
(require "private/lzw.rkt")

(provide gif->rgba-rows
         gif->all-rgba-rows)

(define-syntax-rule (when-debugging expr ...) 
  #;(begin expr ...)
  (void))

(define-struct lsd (width height gt? res sort? gt-size bg-idx ratio))
(define-struct img-desc (left top width height lct? interlace? sort? lct-size))
(define-struct image (desc data ct transparent))

(define current-color-table
  (make-parameter #f))

(define (read-header p)
  (unless (bytes=? (read-bytes 3 p) #"GIF")
    (error "missing the GIF header"))
  (let ([version (read-bytes 3 p)])
    (cond
      [(bytes=? version #"87a") 87]
      [(bytes=? version #"89a") 89]
      [else (error "unknown GIF version: ~a" version)])))

(define (print-lsd l)
  (printf "dimensions: ~a x ~a~n"
          (lsd-width l) (lsd-height l))
  (if (lsd-gt? l)
      (printf "global color table of size ~a~n" (lsd-gt-size l))
      (printf "no global color table~n"))
  (printf "color res:~a~nsort?: ~a~n"
          (lsd-res l) (lsd-sort? l))
  (printf "bg-idx: ~a~naspect ratio:~a~n" 
          (lsd-bg-idx l) (lsd-ratio l)))

(define (read-lsd p)
  (define block (read-bytes 7 p))
  (unless (and (bytes? block)
               (= (bytes-length block) 7))
    (error "malformed Logical Screen Descriptor"))
  (let ([packed-fields (bytes-ref block 4)])
    (make-lsd (integer-bytes->integer block #f #f 0 2)
              (integer-bytes->integer block #f #f 2 4)
              (bitwise-bit-set? packed-fields 7)
              (add1 (bitwise-bit-field packed-fields 4 7))
              (bitwise-bit-set? packed-fields 3)
              (expt 2 (add1 (bitwise-bit-field packed-fields 0 3)))
              (bytes-ref block 5)
              (bytes-ref block 6))))

(define (read-ct size p)
  (define res (make-bytes (* 3 size)))
  (let ([n (read-bytes! res p)])
    (unless (= n (* 3 size))
      (error "Color Table unexpectedly ended")))
  res)

(define (print-argbs cs)
  (define size (/ (bytes-length cs) 4))
  (printf "size: ~a~n" size)
  (for ([n (in-range size)])
       (printf "color ~a: (~a, ~a, ~a, ~a)~n"
               n
               (bytes-ref cs (* 4 n))
               (bytes-ref cs (+ 1 (* 4 n)))
               (bytes-ref cs (+ 2 (* 4 n)))
               (bytes-ref cs (+ 3 (* 4 n))))))

(define (print-img-desc id)
  (printf "pos: (~a, ~a)~n"
          (img-desc-left id) (img-desc-top id))
  (printf "dimensions: ~a x ~a~n"
          (img-desc-width id) (img-desc-height id))
  (if (img-desc-lct? id)
      (printf "local color table of size ~a~n" (img-desc-lct-size id))
      (printf "no local color table~n"))
  (printf "interlaced?: ~a~nsort?: ~a~n"
          (img-desc-interlace? id)
          (img-desc-sort? id)))

(define (read-img-desc p)
  (define block (read-bytes 9 p))
  (let ([packed-fields (bytes-ref block 8)])
    (make-img-desc
     (integer-bytes->integer block #f #f 0 2)
     (integer-bytes->integer block #f #f 2 4)
     (integer-bytes->integer block #f #f 4 6)
     (integer-bytes->integer block #f #f 6 8)
     (bitwise-bit-set? packed-fields 7)
     (bitwise-bit-set? packed-fields 6)
     (bitwise-bit-set? packed-fields 5)
     (expt 2 (add1 (bitwise-bit-field packed-fields 0 3))))))

(define (read-data-subblocks p)
  (apply 
   bytes-append
   (let loop ([blocks null])
     (let ([size (read-byte p)])
       (cond
         [(eof-object? size)
          (error "Unexpected EOF")]
         [(= size 0) (reverse blocks)]
         [else
          (loop (cons (read-bytes size p) blocks))])))))

(define (read-image-table image-data p)
  (define coding-bits (read-byte p))
  (define lzw-data (read-data-subblocks p))
  (lzw-decompress image-data coding-bits lzw-data))

(define (read-image p transparent-color)
  (define id (read-img-desc p))
  (define data (make-bytes (* (img-desc-width id) (img-desc-height id))))
  (when-debugging
   (print-img-desc id))
  (parameterize ([current-color-table 
                  (if (img-desc-lct? id)
                      (read-ct (img-desc-lct-size id) p)
                      (current-color-table))])
    (when-debugging
     (when (img-desc-lct? id)
       (print-argbs (current-color-table))))
    (read-image-table data p)
    (make-image id data (current-color-table) transparent-color)))

(define (read-gif p just-one?)
  (define version (read-header p))
  (define lsd (read-lsd p))
  (define global-table
    (and (lsd-gt? lsd)
         (read-ct (lsd-gt-size lsd) p)))
  (when-debugging
   (printf "version: ~a~n" version)
   (print-lsd lsd)
   (when global-table
     (print-argbs global-table)))
  (parameterize ([current-color-table global-table])
    (define parsed-blocks
      (let loop ([parsed-blocks null] [transparent-color #f])
        (let ([id (read-byte p)])
          (cond 
            [(eof-object? id)
             (error "Unexpected end of file")]
            [(= id #x3b)
             (reverse parsed-blocks)]
            [(= id #x2c)
             (let ([i (read-image p transparent-color)])
               (if just-one?
                   i
                   (loop (cons i parsed-blocks) transparent-color)))]
            [(= id #x21)
             ;; Extension
             (let ([ext (read-byte p)])
               (cond
                [(= ext #xF9)
                 ;; Graphic control
                 (read-byte p) ; size = 4
                 (read-byte p) ; disposal
                 (read-byte p) (read-byte p) ; delay time
                 (let ([t (read-byte p)]) ; transparent color
                   (read-byte p) ; 0-sized block
                   (loop parsed-blocks t))]
                [(or (= ext #xFE)
                     (= ext #xFF))
                 ;; comment block or app data
                 (let loop ()
                   (let ([size (read-byte p)])
                     (unless (zero? size)
                       (read-bytes size p)
                       (loop))))
                 (loop parsed-blocks transparent-color)]
                [else
                 (log-warning (format "gif: unhandled extension block type 0x~x in ~e" ext p))
                 (loop parsed-blocks transparent-color)]))]
            [else
             (log-warning (format "gif: unhandled block type 0x~x in ~e" id p))
             (loop parsed-blocks transparent-color)]))))
    parsed-blocks))

(define (gif->rgba-rows in)
  (let ([i (read-gif in #t)])
    (image->rgbs-rows i)))

(define (gif->all-rgba-rows in)
  (for/list ([i (in-list (read-gif in #f))])
    (define-values (w h vec) (image->rgbs-rows i))
    (vector w h vec)))

(define (image->rgbs-rows i)
    (let* ([data (image-data i)]
           [len (bytes-length data)]
           [ct (image-ct i)]
           [w (img-desc-width (image-desc i))]
           [h (img-desc-height (image-desc i))]
           [t (let ([v (image-transparent i)])
                (and v (* v 3)))]
           [vec
            ;; build vector of row byte strings
            (list->vector
             (for/list ([j (in-range h)])
               (let ([bstr (make-bytes (* 4 w) 255)])
                 (let ([yp (* w j)])
                   (for ([i (in-range w)])
                     (let ([pos (* 3 (bytes-ref data (+ yp i)))])
                       (when (eq? pos t)
                         ;; transparent
                         (bytes-set! bstr (+ 3 (* i 4)) 0))
                       (bytes-set! bstr (* i 4) (bytes-ref ct pos))
                       (bytes-set! bstr (+ 1 (* i 4)) (bytes-ref ct (+ 1 pos)))
                       (bytes-set! bstr (+ 2 (* i 4)) (bytes-ref ct (+ 2 pos))))))
                 bstr)))]
           [vec2
            (if (img-desc-interlace? (image-desc i))
                ;; reorder rows for interlace decoding
                (let ([vec2 (make-vector h #f)])
                  (for ([i (in-range h)])
                    (let ([j (let ([count (quotient (- h 1) 8)])
                               (if (i . <= . count) 
                                   (* i 8)
                                   (let ([i (- i (add1 count))]
                                         [count (quotient (- h 5) 8)])
                                     (if (i . <= . count)
                                         (+ (* i 8) 4)
                                         (let ([i (- i (add1 count))]
                                               [count (quotient (- h 3) 4)])
                                           (if (i . <= . count)
                                               (+ (* i 4) 2)
                                               (let ([i (- i (add1 count))])
                                                 (+ (* i 2) 1))))))))])
                      (vector-set! vec2 j (vector-ref vec i))))
                  vec2)
                ;; rows are already in order
                vec)])
      (values w h vec2)))
