#lang racket/base
(require net/url)

(provide choose-doc-search
         choose-catalogs)

(define (choose-doc-search config default-doc-search)
  (or (hash-ref config '#:doc-search #f)
      (let ([v (hash-ref config '#:dist-base-url #f)])
        (and v
             (url->string
              (combine-url/relative (string->url v) "doc/local-redirect/index.html"))))
      default-doc-search))

(define (choose-catalogs config default-catalogs)
  (or (hash-ref config '#:dist-catalogs #f)
      (let ([v (hash-ref config '#:dist-base-url #f)])
        (and v
             (list (url->string
                    (combine-url/relative (string->url v) "catalog/"))
                   "")))
      default-catalogs))
