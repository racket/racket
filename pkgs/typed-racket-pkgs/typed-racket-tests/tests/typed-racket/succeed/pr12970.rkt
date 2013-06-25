#lang typed/racket/base

(struct: (D) Boxer ((f : (Boxer2 D))))
(struct: (D) Boxer2 ((f : (D -> Void))))

(: f-Boxer (All (D) ((Boxer D) D -> Void)))
(define (f-Boxer boxer v)
  ((Boxer2-f (Boxer-f boxer)) v)) 

