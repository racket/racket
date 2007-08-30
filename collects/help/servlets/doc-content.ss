(module doc-content mzscheme
  (require "private/headelts.ss"
           "private/read-lines.ss"
           (lib "servlet.ss" "web-server"))
  (provide interface-version timeout start)
  (define interface-version 'v1)
  (define timeout +inf.0)
  (define (start initial-request)
    (with-errors-to-browser
     send/finish
     (lambda ()
       (let* ([bindings (request-bindings initial-request)]
              [file (extract-binding/single 'file bindings)]
              [caption (extract-binding/single 'caption bindings)]
              [offset (with-handlers ((void (lambda _ #f)))
                        (string->number
                         (extract-binding/single 'offset bindings)))])
         `(html (head (title "PLT Help Desk") ,hd-css ,@hd-links)
            ,(read-lines initial-request file caption offset)))))))
