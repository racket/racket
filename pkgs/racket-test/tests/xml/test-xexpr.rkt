#lang racket/base

(module+ test

  (require rackunit
           xml
           racket/port)

  (define xe
    '(doc
      (short)
      (alsoShort)))

  (parameterize ([empty-tag-shorthand '(short alsoShort)])
    (check-equal?
     (call-with-output-string
      (lambda (out)
        (write-xexpr xe out)))
     "<doc><short/><alsoShort/></doc>")))
    
