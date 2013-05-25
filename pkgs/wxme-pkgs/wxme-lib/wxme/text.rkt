(module text mzscheme
  (require mzlib/class
           "wxme.rkt"
           "editor.rkt"
           "private/readable-editor.rkt")

  (provide reader
           text-editor%)

  (define text-editor% (class readable-editor% (super-new)))

  (define reader
    (new (class editor-reader%
           (inherit read-editor-snip)
           (define/override (read-snip text? vers stream)
             (read-editor-snip text? vers stream #f text-editor%))
           (define/override (generate-special editor src line col pos)
             (let ([port (send editor get-content-port)]) 
               (let loop ([accum null])
                 (let ([s (read-bytes 4096 port)])
                   (if (eof-object? s)
                       (bytes->string/utf-8 (apply bytes-append (reverse accum)))
                       (loop (cons s accum)))))))
           (super-new)))))
