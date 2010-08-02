#lang scheme
(require "lzw.rkt")

;; FIXME: still need to handle transparency

(provide gif->rgba-rows)

(define-syntax-rule (when-debugging expr ...) 
  #;(begin expr ...)
  (void))

(define-struct lsd (width height gt? res sort? gt-size bg-idx ratio))
(define-struct img-desc (left top width height lct? interlace? sort? lct-size))
(define-struct image (desc data ct))

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

(define (read-image p)
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
    (make-image id data (current-color-table))))

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
      (let loop ([parsed-blocks null])
        (let ([id (read-byte p)])
          (cond 
            [(eof-object? id)
             (error "Unexpected end of file")]
            [(= id #x3b)
             (reverse parsed-blocks)]
            [(= id #x2c)
             (let ([i (read-image p)])
               (if just-one?
                   i
                   (loop (cons i parsed-blocks))))]
            [else
             (log-warning (format "gif: unhandled block type 0x~x" id))
             (loop parsed-blocks)]))))
    parsed-blocks))

(define (gif->rgba-rows in)
  (let ([i (read-gif in #t)])
    (let* ([data (image-data i)]
           [len (bytes-length data)]
           [ct (image-ct i)]
           [w (img-desc-width (image-desc i))]
           [h (img-desc-height (image-desc i))])
      (values
       w
       h
       (list->vector
        (for/list ([j (in-range h)])
          (let ([bstr (make-bytes (* 4 w) 255)])
            (let ([yp (* w j)])
              (for ([i (in-range w)])
                (let ([pos (* 3 (bytes-ref data (+ yp i)))])
                  (bytes-set! bstr (* i 4) (bytes-ref ct pos))
                  (bytes-set! bstr (+ 1 (* i 4)) (bytes-ref ct (+ 1 pos)))
                  (bytes-set! bstr (+ 2 (* i 4)) (bytes-ref ct (+ 2 pos))))))
            bstr)))))))
