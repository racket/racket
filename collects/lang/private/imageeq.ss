(module imageeq mzscheme
  (require (lib "mred.ss" "mred")
           (lib "cache-image-snip.ss" "mrlib")
           (lib "class.ss"))
  
  (provide image? image=? 
           coerce-to-cache-image-snip
           snip-size
           bitmaps->cache-image-snip)

  (define (image? a)
    (or (is-a? a image-snip%)
        (is-a? a cache-image-snip%)))
  
  (define (snip-size a)
    (cond
      [(is-a? a cache-image-snip%)
       (send a get-size)]
      [else
       (let ([dc (make-object bitmap-dc% (make-object bitmap% 1 1))]
             [wb (box 0)]
             [hb (box 0)])
         (send a get-extent dc 0 0 wb hb #f #f #f #f)
         (values (unbox wb) 
                 (unbox hb)))]))
         
  (define (image=? a-raw b-raw)
    (unless (image? a-raw) (raise-type-error 'image=? "image" 0 a-raw b-raw))
    (unless (image? b-raw) (raise-type-error 'image=? "image" 1 a-raw b-raw))
    (let ([a (coerce-to-cache-image-snip a-raw)]
          [b (coerce-to-cache-image-snip b-raw)])
      (let-values ([(aw ah) (snip-size a)]
                   [(bw bh) (snip-size b)])
        (and (= aw bw)
             (= ah bh)
             (same/alpha? (argb-vector (send a get-argb))
                          (argb-vector (send b get-argb)))))))
  
  (define (same/alpha? v1 v2)
    (let loop ([i (vector-length v1)])
      (or (zero? i)
          (let ([a1 (vector-ref v1 (- i 4))]
                [a2 (vector-ref v2 (- i 4))])
            (and (or (= a1 a2 255)
                     (and (= a1 a2)
                          (= (vector-ref v1 (- i 3)) (vector-ref v2 (- i 3)))
                          (= (vector-ref v1 (- i 2)) (vector-ref v2 (- i 2)))
                          (= (vector-ref v1 (- i 1)) (vector-ref v2 (- i 1)))))
                 (loop (- i 4)))))))
  
    ;; coerce-to-cache-image-snip : image -> (is-a?/c cache-image-snip%)
  (define (coerce-to-cache-image-snip snp)
    (cond
      [(is-a? snp image-snip%)
       (let ([bmp (send snp get-bitmap)])
         (if bmp
             (let ([bmp-mask (or (send bmp get-loaded-mask)
                                 (send snp get-bitmap-mask)
                                 (bitmap->mask bmp))])
               (bitmaps->cache-image-snip (copy-bitmap bmp)
                                          (copy-bitmap bmp-mask)
                                          (floor (/ (send bmp get-width) 2))
                                          (floor (/ (send bmp get-height) 2))))
             (let-values ([(w h) (snip-size snp)])
               (let* ([bmp (make-object bitmap% 
                             (inexact->exact (floor w))
                             (inexact->exact (floor h)))]
                      [bdc (make-object bitmap-dc% bmp)])
                 (send snp draw bdc 0 0 0 0 w h 0 0 'no-caret)
                 (send bdc set-bitmap #f)
                 (bitmaps->cache-image-snip bmp 
                                            (bitmap->mask bmp)
                                            (floor (/ w 2))
                                            (floor (/ h 2)))))))]
      [else snp]))
    
  ;; copy-bitmap : bitmap -> bitmap
  ;; does not copy the mask.
  (define (copy-bitmap bitmap)
    (let* ([w (send bitmap get-width)]
           [h (send bitmap get-height)]
           [copy (make-object bitmap% w h)]
           [a-dc (make-object bitmap-dc% copy)])
      (send a-dc clear)
      (send a-dc draw-bitmap bitmap 0 0)
      (send a-dc set-bitmap #f)
      copy))
  
  ;; bitmap->mask : bitmap -> bitmap
  (define (bitmap->mask bitmap)
    (let* ([w (send bitmap get-width)]
           [h (send bitmap get-height)]
           [s (make-bytes (* 4 w h))]
           [new-bitmap (make-object bitmap% w h)]
           [dc (make-object bitmap-dc% new-bitmap)])
      (send dc clear)
      (send dc draw-bitmap bitmap 0 0)
      (send dc get-argb-pixels 0 0 w h s)
      (let loop ([i (* 4 w h)])
        (unless (zero? i)
          (let ([r (- i 3)]
                [g (- i 2)]
                [b (- i 1)])
            (unless (and (eq? 255 (bytes-ref s r))
                         (eq? 255 (bytes-ref s g))
                         (eq? 255 (bytes-ref s b)))
              (bytes-set! s r 0)
              (bytes-set! s g 0)
              (bytes-set! s b 0))
            (loop (- i 4)))))
      (send dc set-argb-pixels 0 0 w h s)
      (begin0
        (send dc get-bitmap)
        (send dc set-bitmap #f))))
  
  (define (bitmaps->cache-image-snip color mask px py)
    (let ([w (send color get-width)]
          [h (send color get-height)])
      (new cache-image-snip%
           [width w]
           [height h]
           [dc-proc
            (lambda (dc dx dy)
              (send dc draw-bitmap color dx dy 'solid 
                    (send the-color-database find-color "black")
                    mask))]
           [argb-proc
            (lambda (argb-vector dx dy)
              (overlay-bitmap argb-vector dx dy color mask))]
           [px px]
           [py py]))))
