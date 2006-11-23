(module doc-message mzscheme
  (require "private/headelts.ss"
           "private/util.ss"
           (lib "servlet.ss" "web-server"))
  (provide interface-version timeout start)
  (define interface-version 'v1)
  (define timeout +inf.0)
  (define (start initial-request)
    (report-errors-to-browser send/finish)
    (let ([bindings (request-bindings initial-request)])
      `(html (head ,hd-css ,@hd-links (title "PLT collection message"))
             (body ,(format-collection-message
                     (extract-binding/single 'msg bindings))
                   (hr))))))
