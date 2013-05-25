#lang racket
(require xml
         rackunit)

(define (read->offset s)
  (location-offset
   (source-stop
    (first
     (element-content
      (document-element
       (read-xml (open-input-string s))))))))

(define (test s char-ans byte-ans)
  (check-equal? (read->offset s)
                char-ans)
  (parameterize ([xml-count-bytes #t])
    (check-equal? (read->offset s)
                  byte-ans)))

(module+ test
  (test "<html>foo</html>" 10 10)
  (test "<html>foλ</html>" 10 11))
