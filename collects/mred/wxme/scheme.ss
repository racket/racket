
(module scheme mzscheme
  (require (lib "class.ss")
           "wxme.ss"
           "editor.ss"
           "private/readable-editor.ss")

  (provide reader)

  (define reader
    (new
     (class editor-reader%
       (inherit read-editor-snip)
       (define/override (read-snip text? vers stream)
         (let ([splice? (zero? (send stream read-integer "splice?"))])
           (read-editor-snip text? vers stream splice?)))

       (define/override (generate-special editor src line col pos)
         (list (if (readable-editor-data editor)
                   'unquote-splicing
                   'unquote)
               (read (editor-content-port editor))))

       (super-new)))))
