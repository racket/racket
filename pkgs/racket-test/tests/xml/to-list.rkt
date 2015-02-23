#lang racket
(require xml)
(provide (all-defined-out))

(define (document->list xml)
  (list 'make-document
        (prolog->list (document-prolog xml))
        (element->list (document-element xml))
        (list* 'list (map misc->list (document-misc xml)))))
(define (prolog->list p)
  (list 'make-prolog
         (list* 'list (map misc->list (prolog-misc p)))
         (dtd->list (prolog-dtd p))
         (list* 'list (map misc->list (prolog-misc2 p)))))
(define (dtd->list d)
  (if d
      (list 'make-document-type
            (document-type-name d)
            (external-dtd->list (document-type-external d))
            (document-type-inlined d))
      #f))
(define (external-dtd->list d)
  (cond
    [(external-dtd/system? d)
     (list 'make-external-dtd/system (external-dtd-system d))]
    [(external-dtd/public? d)
     (list 'make-external-dtd/public (external-dtd-system d) (external-dtd/public-public d))]
    [(external-dtd? d)
     (list 'make-external-dtd (external-dtd-system d))]))
(define (element->list e)
  (list 'make-element
        (source->list e)
        (list 'quote (element-name e))
        (list* 'list (map attribute->list (element-attributes e)))
        (list* 'list (map content->list (element-content e)))))
(define (misc->list e)
  (cond
    [(comment? e)
     (comment->list e)]
    [(p-i? e)
     (p-i->list e)]))
(define (content->list e)
  (cond
    [(pcdata? e) (pcdata->list e)]
    [(element? e) (element->list e)]
    [(entity? e) (entity->list e)]
    [(comment? e) (comment->list e)]
    [(cdata? e) (cdata->list e)]))
(define (attribute->list e)
  (list 'make-attribute
        (source->list e)
        (attribute-name e)
        (attribute-value e)))
(define (entity->list e)
  (list 'make-entity
        (source->list e)
        (list 'quote (entity-text e))))
(define (pcdata->list e)
  (list 'make-pcdata
        (source->list e)
        (pcdata-string e)))
(define (cdata->list e)
  (list 'make-cdata
        (source->list e)
        (cdata-string e)))
(define (p-i->list e)
  (list 'make-p-i
        (source->list e)
        (p-i-target-name e)
        (p-i-instruction e)))
(define (comment->list e)
  (list 'make-comment
        (comment-text e)))
(define (source->list e)
  (list 'make-source
        (location->list (source-start e))
        (location->list (source-stop e))))
(define (location->list e)
  (if (symbol? e)
      e
      (list 'make-location
            (location-line e)
            (location-char e)
            (location-offset e))))
