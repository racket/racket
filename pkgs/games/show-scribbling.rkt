#lang racket/base

(require setup/xref
         scribble/xref
         scribble/tag
         net/url
         net/sendurl)

(provide show-scribbling)

(define (show-scribbling mod-path tag)
  (define xref (load-collections-xref))
  (λ ()
    (define-values (path anchor)
      (xref-tag->path+anchor 
       xref
       (make-section-tag tag #:doc mod-path)))
    (if path
        (let ([u (path->url path)])
          (send-url (url->string u)))
        (error 'show-scribbling "cannot find docs for: ~.s ~.s" mod-path tag))))
