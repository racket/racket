#lang scheme/base
(require scheme/gui/base
         mrlib/cache-image-snip
         mzlib/class)

(provide scene? image? image=? 
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
  (equal? a-raw b-raw))

(define (scene? i)
  (and (image? i)
       (let-values ([(x y) (send (coerce-to-cache-image-snip i) get-pinhole)])
         (and (= 0 x)
              (= 0 y)))))
