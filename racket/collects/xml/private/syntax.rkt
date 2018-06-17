#lang racket/base
(require racket/contract
         racket/list
         "structures.rkt"
         "reader.rkt"
         "xexpr.rkt")

(provide/contract
 ; XXX these should both actually return syntax? that is also xexpr/c
 [syntax:read-xml (() (input-port? #:src any/c) . ->* . syntax?)]
 [syntax:read-xml/element (() (input-port? #:src any/c) . ->* . syntax?)])

;; the `src` argument is like the 1st argument to `read-syntax`:
;; it goes in the `syntax-source` field of the result
(define (syntax:read-xml [in (current-input-port)]
                         #:src [src (object-name in)])
  (define the-xml (read-xml in))
  (define the-xml-element (document-element the-xml))
  (element->xexpr-syntax src the-xml-element))

;; the `src` argument is like the 1st argument to `read-syntax`:
;; it goes in the `syntax-source` field of the result
(define (syntax:read-xml/element [in (current-input-port)]
                                 #:src [src (object-name in)])
  (define the-xml-element (read-xml/element in))
  (element->xexpr-syntax src the-xml-element))

(define (position src from to)
  (let ([start-offset (location-offset from)])
    (list src
          (location-line from) (location-char from) start-offset
          (- (location-offset to) start-offset))))

(define (wrap src s e)
  (datum->syntax #f e (position src (source-start s) (source-stop s))))  

(define ((attribute->syntax src) a)
  (wrap src a (list (attribute-name a) (attribute-value a))))

(define (non-dropping-combine src atts body)
  (list* (map (attribute->syntax src) atts) body))

(define (combine src atts body)
  (if (xexpr-drop-empty-attributes)
      (if (empty? atts)
          body
          (non-dropping-combine src atts body))
      (non-dropping-combine src atts body)))

(define (element->xexpr-syntax src e)
  (wrap src
        e
        (list* (element-name e)
               (combine src
                        (element-attributes e)
                        (map (content->xexpr-syntax src) (element-content e))))))

(define ((content->xexpr-syntax src) x)
  (cond
    [(element? x) (element->xexpr-syntax src x)]
    [(pcdata? x) (wrap src x (pcdata-string x))]
    [(entity? x) (wrap src x (entity-text x))]
    [else (wrap src x x)]))
