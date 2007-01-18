
(module readable-editor mzscheme
  (require (lib "class.ss")
           (lib "struct.ss")
           "../wxme.ss"
           "../editor.ss")

  (provide editor-reader%
           (struct readable-editor (reader data)))

  (define-struct/properties (readable-editor editor) (reader data)
    ([prop:readable (lambda (this src line col pos)
                      (send (readable-editor-reader this) generate-special this src line col pos))]))

  (define editor-reader%
    (class object%
      (define/public (read-header vers stream)
        (void))
      (define/public (read-editor-snip text? vers stream data)
        (let ([s (send stream read-editor-snip)])
          (if text?
              s
              (make-readable-editor s this data))))
      (define/public (read-snip text? vers stream)
        (read-editor-snip text? vers stream #f))

      (define/public (generate-special editor src line col pos)
        (make-special-comment editor))
      
      (super-new))))
