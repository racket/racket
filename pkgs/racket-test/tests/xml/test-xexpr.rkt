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
     "<doc><short/><alsoShort/></doc>"))

  (parameterize ([empty-tag-shorthand '(short)])
    (check-equal?
     (call-with-output-string
      (lambda (out)
        (write-xexpr xe out)))
     "<doc><short/><alsoShort></alsoShort></doc>"))

  (parameterize ([empty-tag-shorthand '(alsoShort)])
    (check-equal?
     (call-with-output-string
      (lambda (out)
        (write-xexpr xe out)))
     "<doc><short></short><alsoShort/></doc>"))

  (parameterize ([empty-tag-shorthand '()])
    (check-equal?
     (call-with-output-string
      (lambda (out)
        (write-xexpr xe out)))
     "<doc><short></short><alsoShort></alsoShort></doc>"))

  (parameterize ([empty-tag-shorthand 'always])
    (check-equal?
     (call-with-output-string
      (lambda (out)
        (write-xexpr xe out)))
     "<doc><short/><alsoShort/></doc>"))

  (parameterize ([empty-tag-shorthand 'never])
    (check-equal?
     (call-with-output-string
      (lambda (out)
        (write-xexpr xe out)))
     "<doc><short></short><alsoShort></alsoShort></doc>")))

    
