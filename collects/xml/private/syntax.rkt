#lang racket/base
(require racket/contract
         racket/list
         "structures.rkt"
         "reader.rkt"
         "xexpr.rkt")

(provide/contract
 ; XXX these should both actually return syntax? that is also xexpr/c
 [syntax:read-xml (() (input-port?) . ->* . syntax?)]
 [syntax:read-xml/element (() (input-port?) . ->* . syntax?)])

(define (syntax:read-xml [in (current-input-port)])
  (define the-xml (read-xml in))
  (define the-xml-element (document-element the-xml))
  (element->xexpr-syntax the-xml-element))

(define (syntax:read-xml/element [in (current-input-port)])
  (define the-xml-element (read-xml/element in))
  (element->xexpr-syntax the-xml-element))

(define (position from to)
  (let ([start-offset (location-offset from)])
    (list #f (location-line from) (location-char from) start-offset
          (- (location-offset to) start-offset))))

(define (wrap s e)
  (datum->syntax #f e (position (source-start s) (source-stop s))))  

(define (attribute->syntax a)
  (wrap a (list (attribute-name a) (attribute-value a))))

(define (non-dropping-combine atts body)
  (list* (map attribute->syntax atts) body))

(define (combine atts body)
  (if (xexpr-drop-empty-attributes)
      (if (empty? atts)
          body
          (non-dropping-combine atts body))
      (non-dropping-combine atts body)))

(define (element->xexpr-syntax e)
  (wrap e
        (list* (element-name e)
               (combine (element-attributes e)
                        (map content->xexpr-syntax (element-content e))))))

(define (content->xexpr-syntax x)
  (cond
    [(element? x) (element->xexpr-syntax x)]
    [(pcdata? x) (wrap x (pcdata-string x))]
    [(entity? x) (wrap x (entity-text x))]
    [else (wrap x x)]))
