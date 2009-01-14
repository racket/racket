#lang scheme
(require xml
         scribble/text
         scheme/port)

(define-syntax include-template
  (syntax-rules ()
    [(_ p)
     (with-output-to-string
      (lambda ()
        (output (include/text p))))]))

(define-syntax include-template/xexpr
  (syntax-rules ()
    [(_ p)
     (string->xexpr (include-template p))]))

(define (string->xexpr s)
  (with-input-from-string 
   s
   (lambda ()
     (xml->xexpr (document-element (read-xml))))))

(define-syntax in
  (syntax-rules ()
    [(_ x xs e ...)
     (for/list ([x xs])
       (begin/text e ...))]))

(provide include-template
         in)
