#lang racket/base
(require racket/prefab)

(provide immutable-prefab-struct-key
         all-fields-immutable?)

(define (all-fields-immutable? k)
  (prefab-key-all-fields-immutable? k))
