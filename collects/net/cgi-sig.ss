
(module cgi-sig mzscheme
  (require (lib "unitsig.ss"))

  (provide net:cgi^)

  (define-signature net:cgi^
    (
     ;; -- exceptions raised --
     (struct cgi-error ())
     (struct incomplete-%-suffix (chars))
     (struct invalid-%-suffix (char))

     ;; -- cgi methods --
     get-bindings
     get-bindings/post
     get-bindings/get
     output-http-headers
     generate-html-output
     generate-error-output
     bindings-as-html
     extract-bindings
     extract-binding/single
     get-cgi-method
     
     ;; -- general HTML utilities --
     string->html
     generate-link-text
     )))

