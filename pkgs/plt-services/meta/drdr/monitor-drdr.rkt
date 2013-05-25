#lang racket
(require xml
         net/url
         tests/web-server/util
         "scm.rkt")

(define drdr-url 
  (string->url "http://drdr.racket-lang.org"))

(define drdr-xml
  (call/input-url drdr-url get-pure-port read-xml/element))
(define drdr-xexpr
  (xml->xexpr drdr-xml))

(define-values
  (building done)
  (for/fold ([building empty]
             [done empty])
    ([tr (in-list (reverse (simple-xpath*/list '(tbody) drdr-xexpr)))])
    (define rev (string->number (simple-xpath* '(a) tr)))
    (define building? (simple-xpath* '(td #:class) tr))
    (if building?
        (values (list* rev building) done)
        (values building (list* rev done)))))

(if (empty? building)
    (if (= (first done) (newest-push))
        (void)
        (error 'monitor-drdr "DrDr is not building, but is not at the most recent push"))
    (void))
