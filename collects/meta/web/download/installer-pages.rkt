#lang meta/web

(require "shared.rkt" "data.rkt")

(define (render-installer-page installer)
  (define path (installer-path installer))
  (define file (installer-file installer))
  (define html-file (string-append (regexp-replace* #rx"\\." file "-") ".html"))
  (define version (installer-version installer))
  (define date (version->date version))
  (define package (package->name (installer-package installer)))
  (define size (installer-size installer))
  (define type (if (installer-binary? installer) "" " source"))
  (define platform (platform->name (installer-platform installer)))
  (define title @text{Download @package v@|version type| for @platform})
  (define suffix-desc (suffix->name (installer-suffix installer)))
  (define (row label text)
    @tr[valign: 'top]{
      @td[align: 'right]{@b{@label}:}
      @td{@nbsp}
      @td[align: 'left]{@text}})
  (define (this url [mode #f])
    (case mode
      [(only-platform) (a href: url platform type)]
      [(render-option) (option value: url platform type)]
      [(#f) @a[href: url]{@title}]
      [else (error 'installer-page "unknown mode: ~e" mode)]))
  @page[#:file html-file #:title title #:referrer this #:part-of 'download]{
    @table[width: "90%" align: 'center]{
      @tr[valign: 'top]{
        @td[width: "50%"]{
          @table{@(row "Package" package)
                 @(row "Version" @list{@version (@date)})
                 @(row "Platform" (list platform type))
                 @(row "Type"     suffix-desc)
                 @(row "File"     file)
                 @(row "Size"     size)}}
        @td[width: "50%"]{
          Download links:
          @div[style: "font-size: 75%; text-align: right; float: right;"]{
            (Choose the nearest site)}
          @ul{@(map (lambda (m)
                      @li{@a[href: (list (mirror-url m) path)]{
                            @(mirror-location m)}})
                    mirrors)}}}}
    @;TODO: decide whether this is really needed
    @; (looks redundant now that all of the installers are pretty standard)
    @;section{Installation instructions}
    @;(bundle-installation-instructions bundle)
    @;br{}
    @;div[align: 'right]{(@(link-to 'license))}
    })

(provide installer->page)
(define installer->page
  (let ([t (make-hasheq)])
    (lambda (inst . more)
      (let ([page (hash-ref! t inst (lambda ()
                                      (render-installer-page inst)))])
        (if (null? more) page (apply page more))))))
