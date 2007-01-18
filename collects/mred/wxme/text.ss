
(module text mzscheme
  (require (lib "class.ss")
           "wxme.ss"
           "editor.ss"
           "private/readable-editor.ss")

  (provide reader)

  (define reader
    (new (class editor-reader%
           (define/override (generate-special editor src line col pos)
             (let ([port (editor-content-port editor)]) 
               (let loop ([accum null])
                 (let ([s (read-bytes 4096 port)])
                   (if (eof-object? s)
                       (bytes->string/utf-8 (apply bytes-append (reverse accum)))
                       (loop (cons s accum)))))))
           (super-new)))))
