#lang racket/base
(require 2htdp/image
         (for-syntax "private/planetcute-image-list.rkt")
         (for-syntax racket/base))

(define-syntax (definitions stx)
  #`(begin
      #,@(for/list ([img (in-list (apply append (map cdr images)))])
           (define req (string->symbol (format "2htdp/planetcute/~a" (name->filename img))))
           #`(begin
               (provide #,img)
               (define #,img (bitmap #,req))))))

(definitions)
