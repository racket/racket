(module cache-image mzscheme
  (require mzlib/class
           "wxme.rkt")

  (provide reader
           cache-image%)

  (define cache-image%
    (class object%
      (init-field argb width height pin-x pin-y)
      
      (define (get-argb) argb)
      (define (get-width) width)
      (define (get-height) height)
      (define (get-pin-x) pin-x)
      (define (get-pin-y) pin-y)
      
      (super-new)))

  (define reader
    (new
     (class* object% (snip-reader<%>)
       (define/public (read-header vers stream)
         (void))
       (define/public (read-snip text? cvers stream)
         (let ([content (send stream read-bytes "content")])
           (if text?
               #"."
               (let ([l (read (open-input-bytes content))])
                 (make-object cache-image% 
                              (car l)
                              (cadr l)
                              (/ (vector-length (car l)) (cadr l) 4)
                              (caddr l)
                              (cadddr l))))))
       (super-new)))))
