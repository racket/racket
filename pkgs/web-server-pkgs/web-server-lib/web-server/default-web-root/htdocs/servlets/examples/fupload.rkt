#lang racket/base
(require racket/match
         web-server/servlet)
(provide (all-defined-out))
(define interface-version 'v1)
(define timeout +inf.0)

(define (start initial-request)
  (define req
    (send/suspend
     (lambda (k-url)
       (response/xexpr
        `(html (body (form ([action ,k-url] 
                            [method "post"]
                            [enctype "multipart/form-data"])
                           (input ([type "file"]
                                   [name "somename"]))
                           (input ([type "submit"])))))))))
  (define (header->xexpr h)
    (match h
      [(struct header (field value))
       `(li (ul (li "Field: " ,(bytes->string/utf-8 field))
                (li "Value: " ,(bytes->string/utf-8 value))))]))
  (define (binding->xexpr b)
    (match b
      [(struct binding:form (field value))
       `(li (ul (li "Field: " ,(bytes->string/utf-8 field))
                (li "Value: " ,(bytes->string/utf-8 value))))]
      [(struct binding:file (field filename headers content))
       `(li (ul (li "Field: " ,(bytes->string/utf-8 field))
                (li "Name: " ,(bytes->string/utf-8 filename))
                (li "Headers: " (ul ,@(map header->xexpr headers)))
                (li "Contents: " (pre ,(bytes->string/utf-8 content)))))]))
  (response/xexpr
   `(html (body ([bgcolor "white"])
                (p "Uploaded:"
                   (ul ,@(map binding->xexpr (request-bindings/raw req))))))))
