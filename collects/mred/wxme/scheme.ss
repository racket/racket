
(module scheme mzscheme
  (require (lib "class.ss")
           "../wxmefile.ss"
           "nested.ss")

  (provide reader)

  (define reader
    (new
     (class nested-reader%
       (inherit read-nested-snip)
       (define/override (read-snip text? vers stream)
         (let ([splice? (zero? (send stream read-integer "splice?"))])
           (read-nested-snip text? vers stream splice?)))

       (define/override (generate-special nested src line col pos)
         (list (if (readable-nested-data nested)
                   'unquote-splicing
                   'unquote)
               (read (nested-content-port nested))))

       (super-new)))))
