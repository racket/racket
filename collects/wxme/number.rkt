(module number mzscheme
  (require mzlib/class
           "wxme.rkt")

  (provide reader)

  (define reader
    (new
     (class* object% (snip-reader<%>)
       (define/public (read-header vers stream)
         (void))
       (define/public (read-snip text? cvers stream)
         (let ([number (send stream read-bytes "number")]
               [decimal-prefix (send stream read-bytes "decimal prefix")]
               [fraction-bytes (send stream read-bytes "fraction")]
               [expansions (send stream read-bytes "expansions")])
           (if text?
               number
               (string->number (bytes->string/latin-1 number)))))
       (super-new)))))
