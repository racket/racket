#lang meta/web

(require "resources.rkt" "data.rkt" "../www/download.rkt")

(define (in-ftp . paths)
  (string-join (cons "/var/ftp/pub/racket" paths) "/"))

(define docs       (symlink (in-ftp "docs")))
(define installers (symlink (in-ftp "installers")))
(define stubs      (symlink "/www/stubs"))

(provide index)
(define index
  @page[#:link-title "Downloads" #:part-of 'download]{
    @div[style: "float: right;"]{@download-button}
    Use these links to browse the download directories directly:
    @ul{@li{Current @a[href: `(,installers "/recent")]{installers}
            (or @a[href: installers]{all versions}).}
        @li{Current documentation in
            @a[href: `(,docs "/recent/html")]{HTML} and in
            @a[href: `(,docs "/recent/pdf")]{PDF}
            (or @a[href: docs]{all versions}).}}})
