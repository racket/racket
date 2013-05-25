(module serialize-structs racket/base

  ;; Exports just the structre info, which is needed in
  ;;  "class.rkt" (which is, in turn, ultimately required by
  ;;  "serialize.rkt")

  (provide
   (protect-out
    (struct-out serialize-info)
    (struct-out deserialize-info)
    prop:serializable serializable-struct? serializable-info))
  
  (define-struct serialize-info (vectorizer deserialize-id can-cycle? dir))
  
  (define-struct deserialize-info (maker cycle-maker))
  
  (define-values (prop:serializable serializable-struct? serializable-info)
    (make-struct-type-property 'serializable #f)))
