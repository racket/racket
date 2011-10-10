#lang racket
(require tests/eli-tester
         xml)

(test
 (with-output-to-bytes
  (lambda ()
    (write-xexpr
     `(html (head (title "Form with CDATA"))
            (body (p "Hello, this is a form")
                  (p ,(cdata 'cdata-start 'cdata-end "<![CDATA[foo]]>"))
                  (p ,(p-i 'pis 'pie 'target "instruction"))
                  (p ,(comment "comment"))
                  "Something")))))
 =>
 #"<html><head><title>Form with CDATA</title></head><body><p>Hello, this is a form</p><p><![CDATA[foo]]></p><p><?target instruction?></p><p><!--comment--></p>Something</body></html>")
