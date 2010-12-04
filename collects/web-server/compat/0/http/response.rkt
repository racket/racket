#lang racket/base
(require (prefix-in new: web-server/http/response)
         "../http/response-structs.rkt")

(define (output-response conn r)
  (new:output-response conn (normalize-response r)))
(define (output-response/method conn r meth)
  (new:output-response/method conn (normalize-response r) meth))

(provide 
 (rename-out
  [new:print-headers print-headers]
  [new:output-file output-file])
 (all-defined-out))