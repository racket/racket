(module file-vector mzscheme
  (require (lib "serialize.ss"))
  (provide deserialize-info:file-vector
           struct:file-vector
           make-file-vector
           file-vector?
           file-vector-ref
           file-vector-set!)
  
  (define deserialize-info:file-vector
    (make-deserialize-info
     
     ;; make-proc: symbol -> file-vector
     (lambda (file-tag)
       (let ([vals
              (vector->list
               (call-with-input-file (symbol->string file-tag)
                 (lambda (i-port)
                   (deserialize (read i-port)))))])
         (apply make-file-vector (cons file-tag vals))))
     
     ;; cycle-make-proc: -> (values file-vector (file-vector -> void))
     (lambda ()
       (let ([new-file-vector
              (make-file-vector #f #f)])
         (values
          new-file-vector
          (lambda (fv)
            (set-file-vector-tag! new-file-vector (file-vector-tag fv))
            (set-file-vector-vec! new-file-vector (file-vector-vec fv))))))))
  
  
  
  (define file-vector:serialize-info
    (make-serialize-info
     
     ;; to-vector: file-vector -> (vectorof symbol)
     (lambda (fv)
       (call-with-output-file (symbol->string (file-vector-tag fv))
         (lambda (o-port)
           (write (serialize (file-vector-vec fv)) o-port))
         'replace)
       (make-vector 1 (file-vector-tag fv)))
     
     ;; The serializer id: --------------------
     (syntax deserialize-info:file-vector)
     
     ;; can-cycle?
     #t
     
     ;; Directory for last-ditch resolution --------------------
     (or (current-load-relative-directory) (current-directory))))
  
  (define-values (struct:file-vector make-file-vector file-vector? file-vector-ref file-vector-set!
                                     file-vector-tag set-file-vector-tag!
                                     file-vector-vec set-file-vector-vec!)
    (let-values ([(struct:file-vector make-fv-struct file-vector? fv-struct-ref fv-struct-set!)
                  (make-struct-type 'struct:file-vector ;; the tag goes here
                                    #f  ; no super type
                                    2
                                    0   ; number of auto-fields
                                    #f  ; auto-v
                                    
                                    ; prop-vals:
                                    (list (cons prop:serializable file-vector:serialize-info))
                                    
                                    #f  ; inspector
                                    
                                    ;; the struct apply proc:
                                    #f)])
      (values struct:file-vector
              (lambda (tag . vals)
                (make-fv-struct tag (list->vector vals)))
              file-vector?
              (lambda (fv n)
                (vector-ref (fv-struct-ref fv 1) n))
              (lambda (fv n val)
                (vector-set! (fv-struct-ref fv 1) n val))
              (lambda (fv)
                (fv-struct-ref fv 0))
              (lambda (fv new-tag)
                (fv-struct-set! fv 0 new-tag))
              (lambda (fv)
                (fv-struct-ref fv 1))
              (lambda (fv new-vec)
                (fv-struct-set! fv 1 new-vec))))))