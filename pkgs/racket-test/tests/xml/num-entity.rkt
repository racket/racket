#lang racket/base
(require xml)

(module+ test
  (require rackunit)

  (define ex #<<EOF
<?xml version="1.0"?>
<blah foo="1 &amp;&#13;&#10;2" />
EOF
    )

  (check-equal?
   (xml->xexpr (document-element (read-xml (open-input-string ex))))
   '(blah ((foo "1 &\r\n2")))))
