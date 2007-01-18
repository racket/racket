
(module nested mzscheme
  (require (lib "class.ss")
           (lib "struct.ss")
           "../wxmefile.ss")

  (provide nested-reader%
           (struct readable-nested (reader data)))

  (define-struct/properties (readable-nested nested) (reader data)
    ([prop:readable (lambda (this src line col pos)
                      (send (readable-nested-reader this) generate-special this src line col pos))]))

  (define nested-reader%
    (class object%
      (define/public (read-header vers stream)
        (void))
      (define/public (read-nested-snip text? vers stream data)
        (let ([s (send stream read-nested-editor)])
          (if text?
              s
              (make-readable-nested s this data))))
      (define/public (read-snip text? vers stream)
        (read-nested-snip text? vers stream #f))

      (define/public (generate-special nested src line col pos)
        (make-special-comment nested))
      
      (super-new))))
