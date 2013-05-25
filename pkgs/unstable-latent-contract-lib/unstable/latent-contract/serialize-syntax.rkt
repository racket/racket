#lang racket/base

;; Serialize and unserialize syntax objects

;; Serializing doesn't store lexical information, so unserializing requires an extra piece of
;; information: the new lexical context. Therefore, 'unserialize' acts a lot like 'replace-context'.

;; Serializing also doesn't store ALL the syntax properties - just the ones with symbol keys.

(require racket/match)

(provide serialize-syntax unserialize-syntax)

;; serialize-props : syntax -> (listof (cons symbol value))
;; Serializes the properties of a syntax object.
(define (serialize-props stx)
  (map (λ (key) (cons key (syntax-property stx key)))
       (syntax-property-symbol-keys stx)))

;; unserialize-props : syntax (listof (cons symbol value)) -> syntax
;; Unserializes properties; returns a new syntax object that is like the old but with the properties.
(define (unserialize-props stx props)
  (for/fold ([stx stx]) ([kv  (in-list props)])
    (match-define (cons key v) kv)
    (syntax-property stx key v)))

;; serialize-loc : syntax -> list
;; Serializes the source location of a syntax object, as a list. This is one of the formats that
;; datum->syntax accepts as a source location, so there is no need for unserialize-loc.
(define (serialize-loc stx)
  (list (syntax-source stx)
        (syntax-line stx)
        (syntax-column stx)
        (syntax-position stx)
        (syntax-span stx)))

;; serialize-syntax : syntax -> list
;; Serializes a syntax object.
(define (serialize-syntax e)
  (cond
    [(syntax? e)  (list 'syntax
                        (serialize-syntax (syntax-e e))
                        (serialize-loc e)
                        (serialize-props e))]
    [(pair? e)    (list 'pair (serialize-syntax (car e)) (serialize-syntax (cdr e)))]
    [(vector? e)  (list 'vector (serialize-syntax (vector->list e)))]
    [(box? e)     (list 'box (serialize-syntax (unbox e)))]
    [(prefab-struct-key e) => (λ (k) (list 'struct k (serialize-syntax (struct->vector e))))]
    [else         (list 'datum e)]))

;; unserialize-syntax : syntax list -> syntax
;; Unserializes a syntax object, and associates with each part of it the given context.
(define (unserialize-syntax ctx lst)
  (let loop ([lst lst])
    ;(printf "lst = ~v~n" lst)
    (match lst
      [(list 'syntax lst loc props)  (unserialize-props (datum->syntax ctx (loop lst) loc) props)]
      [(list 'pair lst1 lst2)        (cons (loop lst1) (loop lst2))]
      [(list 'vector lst)            (list->vector (loop lst))]
      [(list 'box lst)               (box (loop lst))]
      [(list 'struct k lst)          (apply make-prefab-struct k (loop lst))]
      [(list 'datum e)               e])))
