
(module text mzscheme
  (require (lib "class.ss")
           "../wxmefile.ss"
           "nested.ss")

  (provide reader)

  (define reader
    (new (class nested-reader%
           (define/override (generate-special nested src line col pos)
             (let ([port (nested-content-port nested)]) 
               (let loop ([accum null])
                 (let ([s (read-bytes 4096 port)])
                   (if (eof-object? s)
                       (bytes->string/utf-8 (apply bytes-append (reverse accum)))
                       (loop (cons s accum)))))))
           (super-new)))))
