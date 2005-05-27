(module serialize-structs mzscheme

  ;; Exports just the structre info, which is needed in
  ;;  "class.ss" (which is, in turn, ultimately required by
  ;;  "serialize.ss")

  (provide
   (protect
    (struct serialize-info (vectorizer deserialize-id can-cycle? dir))
    (struct deserialize-info (maker cycle-maker))
    prop:serializable serializable-struct? serializable-info))
  
  (define-struct serialize-info (vectorizer deserialize-id can-cycle? dir))
  
  (define-struct deserialize-info (maker cycle-maker))
  
  (define-values (prop:serializable serializable-struct? serializable-info)
    (make-struct-type-property 'serializable #f)))
