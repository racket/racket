(module readable-editor mzscheme
  (require mzlib/class
           mzlib/struct
           "../wxme.rkt"
           "../editor.rkt")

  (provide readable-editor%
           editor-reader%)

  (define readable-editor%
    (class* editor% (readable<%>)
      (init content reader data)
      (define the-reader reader)
      (define the-data data)

      (define/public (read-special src line col pos)
        (send the-reader generate-special this src line col pos))

      (define/public (get-data)
        the-data)

      (super-make-object content)))

  (define editor-reader%
    (class* object% (snip-reader<%>)
      (define/public (read-header vers stream)
        (void))
      (define/public (read-editor-snip text? vers stream data %)
        (let ([s (send stream read-editor-snip "box content")])
          (if text?
              s
              (make-object % s this data))))
      (define/public (read-snip text? vers stream)
        (read-editor-snip text? vers stream #f readable-editor%))

      (define/public (generate-special editor src line col pos)
        (make-special-comment editor))
      
      (super-new))))
