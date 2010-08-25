#lang scheme/base

(require setup/xref
         scribble/xref
         scribble/basic
         scheme/promise
         net/url
         net/sendurl)

(provide show-scribbling)

(define (show-scribbling mod-path tag)
  (let ([xref (delay (load-collections-xref))])
    (lambda ()
      (let-values ([(path anchor)
                    (xref-tag->path+anchor 
                     (force xref)
                     (list 'part (list (module-path-prefix->string mod-path) tag)))])
        (if path
            (let ([u (path->url path)])
              (send-url (url->string u)))
            (error 'show-scribbling "cannot find docs for: ~.s ~.s" mod-path tag))))))
