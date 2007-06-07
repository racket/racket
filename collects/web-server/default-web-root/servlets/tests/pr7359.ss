(module pr7359 mzscheme
  (require (lib "serialize.ss")
           (lib "servlet.ss" "web-server"))
  (provide interface-version timeout start)

  (define interface-version 'v1)
  (define timeout +inf.0)

  (define-serializable-struct foo ())
  (define (start req)
    (deserialize (serialize (make-foo)))
    `(html (body "Made it"))))
