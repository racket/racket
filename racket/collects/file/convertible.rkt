#lang racket/base

(require racket/contract)

(define opaque-default/c (new-∀/c))

(define default-or-f/c (or/c #f opaque-default/c))

(define (make-result-contract request default)
  (define default/c
    (if (unsupplied-arg? default) default-or-f/c opaque-default/c))
  (case request
    [(text)
     (or/c string? default/c)]
    [(gif-bytes png-bytes png@2x-bytes ps-bytes eps-bytes pdf-bytes svg-bytes)
     (or/c bytes? default/c)]
    [(png-bytes+bounds png@2x-bytes+bounds eps-bytes+bounds pdf-bytes+bounds)
     (or/c (list/c bytes?
                   (and/c real? (not/c negative?))
                   (and/c real? (not/c negative?))
                   (and/c real? (not/c negative?))
                   (and/c real? (not/c negative?)))
           default/c)]
    [(png-bytes+bounds8 png@2x-bytes+bounds8 eps-bytes+bounds8 pdf-bytes+bounds8)
     (or/c (list/c bytes?
                   (and/c real? (not/c negative?))
                   (and/c real? (not/c negative?))
                   (and/c real? (not/c negative?))
                   (and/c real? (not/c negative?))
                   (and/c real? (not/c negative?))
                   (and/c real? (not/c negative?))
                   (and/c real? (not/c negative?))
                   (and/c real? (not/c negative?)))
           default/c)]
    [else (or/c opaque-default/c any/c)]))

(provide
 (contract-out
  
  [convertible? (-> any/c boolean?)]

  [prop:convertible
   (struct-type-property/c
    (->i ([v convertible?] [request symbol?] [default default-or-f/c])
         [result (request default) (make-result-contract request default)]))]

  [convert
   (->i ([v convertible?] [request symbol?])
        ([default default-or-f/c])
        [result (request default) (make-result-contract request default)])]))


(define-values (prop:convertible convertible? convertible-ref)
  (make-struct-type-property 'convertible))

(define (convert v request [default #f])
  ((convertible-ref v) v request default))
