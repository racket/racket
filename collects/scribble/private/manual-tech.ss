#lang scheme/base
(require "../decode.ss"
         "../struct.ss"
         "../basic.ss"
         "manual-utils.ss"
         "manual-style.ss")

(provide deftech tech techlink)

(define (*tech make-elem style doc s)
  (let* ([c (decode-content s)]
         [s (string-foldcase (content->string c))]
         [s (regexp-replace #rx"ies$" s "y")]
         [s (regexp-replace #rx"s$" s "")]
         [s (regexp-replace* #px"[-\\s]+" s " ")])
    (make-elem style c (list 'tech (doc-prefix doc s)))))

(define (deftech #:style? [style? #t] . s)
  (let* ([e (if style?
                (apply defterm s)
                (make-element #f (decode-content s)))]
         [t (*tech make-target-element #f #f (list e))])
    (make-index-element #f
                        (list t)
                        (target-element-tag t)
                        (list (clean-up-index-string (element->string e)))
                        (list e)
                        'tech)))

(define (tech #:doc [doc #f] . s)
  (*tech (lambda (style c tag)
           (make-link-element
            style
            (list (make-element "techinside" c))
            tag))
         "techoutside"
         doc s))

(define (techlink #:doc [doc #f] . s)
  (*tech make-link-element #f doc s))
