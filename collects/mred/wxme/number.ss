
(module number mzscheme
  (require (lib "class.ss"))

  (provide reader)

  (define reader
    (new
     (class object%
       (define/public (read-header vers stream)
         (void))
       (define/public (read-snip text? cvers stream)
         (let ([number (send stream read-bytes "number")]
               [decimal-prefix (send stream read-bytes "decimal prefix")]
               [fraction-bytes (send stream read-bytes "fraction")]
               [expansions (send stream read-bytes "expansions")])
           (if text?
               number
               (lambda (src line col pos) (string->number (bytes->string/latin-1 number))))))
       (super-new)))))

                   
  
  
  