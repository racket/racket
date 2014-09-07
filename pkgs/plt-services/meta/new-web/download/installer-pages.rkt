#lang plt-web

(require "resources.rkt" "data.rkt" "mirror-link.rkt" plt-web/style)

(define (render-installer-page installer)
  (define path      (installer-path installer))
  (define file      (installer-file installer))
  (define html-file (string-append (regexp-replace* #rx"\\." file "-") ".html"))
  (define release   (installer-release installer))
  (define version   (release-version release))
  (define date      (release-date-string release))
  (define package   (package->name (installer-package installer)))
  (define size      (installer-size installer))
  (define type      "") ;; (if (installer-binary? installer) "" " source")
  (define platform  (platform->name (installer-platform installer) package))
  (define title     @text{Download @package v@|version type| for @platform})
  (define suffix-desc (suffix->name (installer-suffix installer)))
  (define human-size
    (let ([mb (/ size (* 1024 1024))])
      (if (< mb 10)
        (let-values ([(q r) (quotient/remainder (round (* mb 10)) 10)])
          (format "~a.~aM" q r))
        (format "~aM" (round mb)))))
  (define (row label text)
    @tr[valign: 'top]{
      @td[align: 'right]{@b{@label}:}
      @td{@nbsp}
      @td[align: 'left]{@text}})
  (define (this url [mode #f])
    (case mode
      [(only-platform) (a href: url platform type)]
      [(render-option) (option value: url platform type)]
      [(render-direct-option) (option value: (let ([m (first mirrors)])
                                               (string-append (mirror-url* m) path))
                                      x-mirror: @url
                                      x-installer-size: @human-size
                                      platform type)]
      [(render-package-option) (option value: url package)]
      [(#f) @a[href: url]{@title}]
      [else (error 'installer-page "unknown mode: ~e" mode)]))
  @page[#:site download-site 
        #:file html-file #:title title #:referrer this #:part-of 'download]{
    @columns[10 #:center? #t #:row? #t #:center-text? #t]{
      @table[align: 'center style: "border: none;"]{
        @tr[valign: 'top]{
          @td[width: "50%" style: "border: none;"]{
            @table[class: "striped rounded"]{
                   @(row "Package"  package)
                   @(row "Version"  @list{@version (@date)})
                   @(row "Platform" (list platform type))
                   @(row "Type"     suffix-desc)
                   @(row "File"     file)
                   @(row "Size"     @span[title: @list{Exact size: @size bytes}]{
                                    @human-size})}}
        @td[width: "50%" style: "border: none;"]{
          Download links:
          @div[style: "font-size: 75%; text-align: right; float: right;"]{
            (Choose the nearest site)}
          @ul{@(let ([mirrors
                      (filter-map
                       (λ (m)
                         (define url
                           (mirror-link
                            (string-append (mirror-url* m) path)
                            size
                            (λ () (format "~a <~a>"
                                          (mirror-person m)
                                          (mirror-email m)))))
                         (and url @li{@a[href: url]{@(mirror-location m)}}))
                       mirrors)])
                 (case (length mirrors)
                   [(0) (error 'installer-page "no available mirror for: ~e"
                               path)]
                   [(1) (list mirrors
                              @li{@small{(no additional mirrors, yet)}})]
                   [else mirrors]))}}}}
    @;TODO: decide whether this is really needed
    @; (looks redundant now that all of the installers are pretty standard)
    @;section{Installation instructions}
    @;(bundle-installation-instructions bundle)
    }})

(define (mirror-url* m)
  (define u (mirror-url m))
  (if (eq? u 'main)
      ((resource "download/installers/" #f))
      u))

(provide installer->page)
(define installer->page
  (let ([t (make-hasheq)])
    (λ (inst . more)
      (let ([page (hash-ref! t inst (λ () (render-installer-page inst)))])
        (if (null? more) page (apply page more))))))
