(module file-box mzscheme
  (require (lib "serialize.ss")
           (lib "contract.ss"))  
  
  (define-serializable-struct internal-file-box (path))
  (define file-box? internal-file-box?)
  
  (define (file-box path default)
    (define fb (make-internal-file-box path))
    (unless (file-box-set? fb)
      (file-box-set! fb default))
    fb)
  
  (define (file-box-set? fb)
    (with-handlers ([exn? (lambda _ #f)])
      (file-unbox fb)
      #t))
  
  (define (file-unbox fb)
    (deserialize (call-with-input-file (internal-file-box-path fb) read)))
  (define (file-box-set! fb v)
    (with-output-to-file (internal-file-box-path fb) (lambda () (write (serialize v)))))
  
  (provide/contract 
   [file-box? (any/c . -> . boolean?)]
   [file-box (path? any/c . -> . file-box?)]
   [file-unbox (file-box? . -> . any/c)]
   [file-box-set? (file-box? . -> . boolean?)]
   [file-box-set! (file-box? any/c . -> . void)]))