#lang scheme/base
(require racket/contract/base
         "../decode.rkt"
         "../struct.rkt"
         "manual-utils.rkt"
         "manual-style.rkt")

(provide/contract
 [deftech (() (#:style? boolean?) #:rest (listof pre-content?) . ->* . element?)]
 [tech (() 
        (#:doc (or/c module-path? false/c) #:tag-prefixes (or/c (listof string?) false/c) #:key (or/c string? #f))
        #:rest (listof pre-content?) 
        . ->* . element?)]
 [techlink (() 
            (#:doc (or/c module-path? false/c) #:tag-prefixes (or/c (listof string?) false/c) #:key (or/c string? #f))
            #:rest (listof pre-content?) 
            . ->* . element?)])

(define (*tech make-elem style doc prefix s key)
  (let* ([c (decode-content s)]
         [s (string-foldcase (or key (content->string c)))]
         [s (regexp-replace #rx"ies$" s "y")]
         [s (regexp-replace #rx"s$" s "")]
         [s (regexp-replace* #px"[-\\s]+" s " ")]
         [s (datum-intern-literal s)])
    (make-elem style c (list 'tech (doc-prefix doc prefix s)))))

(define (deftech #:style? [style? #t] . s)
  (let* ([e (if style?
                (apply defterm s)
                (make-element #f (decode-content s)))]
         [t (*tech make-target-element #f #f #f (list e) #f)])
    (make-index-element #f
                        (list t)
                        (target-element-tag t)
                        (list (datum-intern-literal
                               (clean-up-index-string (element->string e))))
                        (list e)
                        'tech)))

(define (tech #:doc [doc #f] #:tag-prefixes [prefix #f] #:key [key #f] . s)
  (*tech (lambda (style c tag)
           (make-link-element
            style
            (list (make-element "techinside" c))
            tag))
         "techoutside"
         doc prefix s key))

(define (techlink #:doc [doc #f] #:tag-prefixes [prefix #f] #:key [key #f] . s)
  (*tech make-link-element #f doc prefix s key))
