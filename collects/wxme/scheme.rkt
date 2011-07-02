(module scheme mzscheme
  (require mzlib/class
           "wxme.rkt"
           "editor.rkt"
           "private/readable-editor.rkt")

  (provide reader 
           scheme-editor%)

  (define scheme-editor% (class readable-editor% (super-new)))

  (define reader
    (new
     (class editor-reader%
       (inherit read-editor-snip)
       (define/override (read-snip text? vers stream)
         (let ([splice? (zero? (send stream read-integer "splice?"))])
           (read-editor-snip text? vers stream splice? scheme-editor%)))

       (define/override (generate-special editor src line col pos)
         (list (if (send editor get-data)
                   'unquote-splicing
                   'unquote)
               (read (send editor get-content-port))))

       (super-new)))))
