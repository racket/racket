(module doc-anchor mzscheme
  (require "private/read-doc.ss"
           (lib "servlet.ss" "web-server"))
  (provide interface-version timeout start)
  (define interface-version 'v1)
  (define timeout +inf.0)
  (define (start initial-request)
    (with-errors-to-browser
     send/finish
     (lambda ()
       (let* ([bindings (request-bindings initial-request)]
              [offset (with-handlers ((void (lambda _ #f)))
                        (string->number
                         (extract-binding/single 'offset bindings)))])
         (read-doc initial-request
                   (extract-binding/single 'file bindings)
                   (extract-binding/single 'caption bindings)
                   (extract-binding/single 'name bindings)
                   offset))))))
