#lang racket/base
(require web-server/servlet-dispatch
         racket/runtime-path
         web-server/dispatchers/dispatch-files)

(define-runtime-path static-path "static.txt")

(serve/launch/wait
 (λ (sema)
   (make #:url->path (λ (url) (values static-path null))))
 #:launch-path #f
 #:banner? #f
 #:port 8001)
