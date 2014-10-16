#lang racket/base
(require racket/serialize
         file/convertible)

(provide make-serialized-convertible)

(struct serialized-convertible (ser [deser #:mutable])
  #:property prop:convertible (lambda (v mode default)
                                (unless (serialized-convertible-deser v)
                                  (set-serialized-convertible-deser!
                                   v
                                   (deserialize (serialized-convertible-ser v))))
                                (convert (serialized-convertible-deser v) mode default)))

(define (make-serialized-convertible ser)
  (serialized-convertible ser #f))


