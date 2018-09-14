(module serialize-structs '#%kernel
  
  (#%declare #:cross-phase-persistent)

  ;; Exports just the structre info, which is needed in
  ;;  "class.rkt" (which is, in turn, ultimately required by
  ;;  "serialize.rkt")

  (#%provide (protect (all-defined)))
  
  (define-values (struct:serialize-info
                  make-serialize-info
                  serialize-info?
                  serialize-info-vectorizer
                  serialize-info-deserialize-id
                  serialize-info-can-cycle?
                  serialize-info-dir)
    (let-values ([(struct: make- ? -ref -set!)
                  (make-struct-type 'serialize-info #f 4 0 #f '() (current-inspector) #f '(0 1 2 3) #f 'serialize-info)])
      (values struct:
              make-
              ?
              (make-struct-field-accessor -ref 0 'vectorizer)
              (make-struct-field-accessor -ref 1 'deserialize-id)
              (make-struct-field-accessor -ref 2 'can-cycle?)
              (make-struct-field-accessor -ref 3 'dir))))
  
  (define-values (struct:deserialize-info
                  make-deserialize-info
                  deserialize-info?
                  deserialize-info-maker
                  deserialize-info-cycle-maker)
    (let-values ([(struct: make- ? -ref -set!)
                  (make-struct-type 'deserialize-info #f 2 0 #f '() (current-inspector) #f '(0 1) #f 'deserialize-info)])
      (values struct: make- ? (make-struct-field-accessor -ref 0 'maker) (make-struct-field-accessor -ref '1 'cycle-maker))))
  
  (define-values (prop:serializable serializable-struct? serializable-info)
    (make-struct-type-property 'serializable #f)))
