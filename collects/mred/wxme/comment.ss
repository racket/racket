
(module comment mzscheme
  (require (lib "class.ss")
           (lib "string.ss")
           "wxme.ss"
           "private/readable-editor.ss")

  (provide reader)

  (define reader
    (new (class editor-reader%
           (define/override (read-snip text? vers stream)
             (let ([s (super read-snip text? vers stream)])
               (if text?
                   (apply bytes-append
                          (map (lambda (s)
                                 (bytes-append #"; " s #"\n"))
                               (regexp-split #rx#"\n" s)))
                   s)))
           (super-new)))))
