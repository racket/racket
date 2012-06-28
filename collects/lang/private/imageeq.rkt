#lang racket/base
(require racket/snip
         mrlib/cache-image-snip
         (prefix-in 2htdp/image: mrlib/image-core)
         racket/class)

(provide scene? image? image=? 
         coerce-to-cache-image-snip
         snip-size
         bitmaps->cache-image-snip)

(define (image? a)
  (or (is-a? a image-snip%)
      (is-a? a cache-image-snip%)))

(define (image=? a-raw b-raw)
  (unless (or (2htdp/image:image? a-raw) (image? a-raw)) (raise-type-error 'image=? "image" 0 a-raw b-raw))
  (unless (or (2htdp/image:image? b-raw) (image? b-raw)) (raise-type-error 'image=? "image" 1 a-raw b-raw))
  ;; Rely on image-snip% implementing equal<%>:
  (equal? a-raw b-raw))

(define (scene? i)
  (and (image? i)
       (let-values ([(x y) (send (coerce-to-cache-image-snip i) get-pinhole)])
         (and (= 0 x)
              (= 0 y)))))
