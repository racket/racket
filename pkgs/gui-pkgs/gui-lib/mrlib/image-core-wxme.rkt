#lang racket/base
(require racket/class
         wxme
         "private/image-core-snipclass.rkt"
         "image-core.rkt")
(provide reader image<%>)

(define reader
  (new 
   (class* object% (snip-reader<%>)
     (define/public (read-header vers stream) (void))
     (define/public (read-snip text? cvers stream)
       (define bytes (send stream read-raw-bytes '2htdp/image))
       (if text?
           #"."
           (snipclass-bytes->image bytes)))
     (super-new))))
