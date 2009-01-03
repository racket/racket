(module imageeq mzscheme
  (require mred
           mrlib/cache-image-snip
           mzlib/class)
  
  (provide image? image=? 
           coerce-to-cache-image-snip
           snip-size
           bitmaps->cache-image-snip)

  (define (image? a)
    (or (is-a? a image-snip%)
        (is-a? a cache-image-snip%)))

  (define (image=? a-raw b-raw)
    (unless (image? a-raw) (raise-type-error 'image=? "image" 0 a-raw b-raw))
    (unless (image? b-raw) (raise-type-error 'image=? "image" 1 a-raw b-raw))
    ;; Rely on image-snip% implementing equal<%>:
    (equal? a-raw b-raw)))
