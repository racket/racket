(module comment mzscheme
  (require mzlib/class
           mzlib/string
           "wxme.rkt"
           "private/readable-editor.rkt")

  (provide reader
           comment-editor%)

  (define comment-editor% (class readable-editor% (super-new)))

  (define reader
    (new (class* editor-reader% (snip-reader<%>)
           (inherit read-editor-snip)
           (define/override (read-snip text? vers stream)
             (let ([s (read-editor-snip text? vers stream #f comment-editor%)])
               (if text?
                   (apply bytes-append
                          (map (lambda (s)
                                 (bytes-append #"; " s #"\n"))
                               (regexp-split #rx#"\n" s)))
                   s)))
           (super-new)))))
