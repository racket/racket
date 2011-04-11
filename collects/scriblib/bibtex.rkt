#lang racket/base
(require racket/function)

(define-syntax-rule
  (define-bibtex this-generate-bib this-cite bib-pth)
  (begin
    (define bibtex-db (path->bibdb bib-pth))
    (define this-generate-bib
      (curry generate-bib bibtex-db))
    (define this-cite
      (curry cite bibtex-db))))

(define (path->bibdb pth)
  #f)

(define (generate-bib db style)
  "XXX")

(define (cite db . keys)
  "XXX")

(provide define-bibtex)