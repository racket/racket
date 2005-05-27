
(module base64-sig mzscheme
  (require (lib "unitsig.ss"))

  (provide net:base64^)

  (define-signature net:base64^
    (base64-encode-stream
     base64-decode-stream
     base64-encode
     base64-decode)))

