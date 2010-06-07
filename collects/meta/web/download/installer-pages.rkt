#lang at-exp s-exp "shared.rkt"

(require "data.rkt")

(define (render-installer-page installer)
  (define path (installer-path installer))
  (define file (installer-file installer))
  (define html-file (string-append (regexp-replace* #rx"\\." file "-") ".html"))
  (define version (installer-version installer))
  (define date (version->date version))
  (define package
    (string-titlecase
     (regexp-replace #rx"-" (->string (installer-package installer)) " ")))
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
  @page[#:file html-file #:title title]{
    @table[width: "90%" align: 'center]{
      @tr[valign: 'top]{
        @td[width: "50%"]{
          @table{@(row "Package" package)
                 @(row "Version" @list{@version (@date)})
                 @(row "Platform" platform)
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

(provide installer-pages)
(define installer-pages (map render-installer-page all-installers))
