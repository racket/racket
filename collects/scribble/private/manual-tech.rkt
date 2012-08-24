#lang scheme/base
(require racket/contract/base
         "../decode.rkt"
         "../struct.rkt"
         "manual-utils.rkt"
         "manual-style.rkt")

(provide/contract
 [deftech (() (#:normalize? any/c
               #:style? any/c) 
           #:rest (listof pre-content?) . ->* . element?)]
 [tech (() 
        (#:doc (or/c module-path? false/c) 
         #:tag-prefixes (or/c (listof string?) false/c) 
         #:key (or/c string? #f)
         #:normalize? any/c)
        #:rest (listof pre-content?) 
        . ->* . element?)]
 [techlink (() 
            (#:doc (or/c module-path? false/c) 
             #:tag-prefixes (or/c (listof string?) false/c) 
             #:key (or/c string? #f)
             #:normalize? any/c)
            #:rest (listof pre-content?) 
            . ->* . element?)])

(define (*tech make-elem style doc prefix s key normalize?)
  (let* ([c (decode-content s)]
         [s (or key (content->string c))]
         [s (if normalize?
                (let* ([s (string-foldcase s)]
                       [s (regexp-replace #rx"ies$" s "y")]
                       [s (regexp-replace #rx"s$" s "")]
                       [s (regexp-replace* #px"[-\\s]+" s " ")])
                  s)
                s)]
         [s (datum-intern-literal s)])
    (make-elem style c (list 'tech (doc-prefix doc prefix s)))))

(define (deftech #:style? [style? #t] #:normalize? [normalize? #t] . s)
  (let* ([e (if style?
                (apply defterm s)
                (make-element #f (decode-content s)))]
         [t (*tech make-target-element #f #f #f (list e) #f normalize?)])
    (make-index-element #f
                        (list t)
                        (target-element-tag t)
                        (list (datum-intern-literal
                               (clean-up-index-string (element->string e))))
                        (list e)
                        'tech)))

(define (tech #:doc [doc #f] 
              #:tag-prefixes [prefix #f] 
              #:key [key #f] 
              #:normalize? [normalize? #t] 
              . s)
  (*tech (lambda (style c tag)
           (make-link-element
            style
            (list (make-element "techinside" c))
            tag))
         "techoutside"
         doc prefix s key
         normalize?))

(define (techlink #:doc [doc #f] 
                  #:tag-prefixes [prefix #f] 
                  #:key [key #f] 
                  #:normalize? [normalize? #t] 
                  . s)
  (*tech make-link-element #f doc prefix s key normalize?))
