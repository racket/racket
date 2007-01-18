
(module cache-image mzscheme
  (require (lib "class.ss"))

  (provide reader
           (struct cache-image (argb width height pin-x pin-y)))

  (define-struct cache-image (argb width height pin-x pin-y))

  (define reader
    (new
     (class object%
       (define/public (read-header vers stream)
         (void))
       (define/public (read-snip text? cvers stream)
         (let ([content (send stream read-bytes "content")])
           (if text?
               #"."
               (let ([l (read (open-input-bytes content))])
                 (make-cache-image (car l)
                                   (cadr l)
                                   (/ (vector-length (car l)) (cadr l) 4)
                                   (caddr l)
                                   (cadddr l))))))
       (super-new)))))

                   
  
  
  