#lang s-exp syntax/module-reader
lang/htdp-beginner
#:read (wrap-reader read options)
#:read-syntax (wrap-reader read-syntax options)
#:info (make-info options)
#:module-info (make-module-info options)

(require htdp/bsl/reader)
(define options '())
