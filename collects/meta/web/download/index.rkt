#lang meta/web

(require "resources.rkt" "symlinks.rkt" "../www/download.rkt")

(provide index)
(define index
  @page[#:link-title "Downloads" #:part-of 'download
        #:description
        @'{Download Racket, a modern dialect of Lisp/Scheme.  @;
           Available for Windows, Mac, Linux, and other Unix platforms.  @;
           Includes the DrRacket IDE.}]{
    @div[style: "float: right;"]{@download-button}
    Use these links to browse the download directories directly:
    @ul{@li{Current @a[href: `(,installers "/recent")]{installers}
            (or @a[href: installers]{all versions}).}
        @li{Current documentation in
            @a[href: `(,docs "/recent/html")]{HTML} and in
            @a[href: `(,docs "/recent/pdf")]{PDF}
            (or @a[href: docs]{all versions}).}
        @li{Binary @a[href: libs]{libraries} mainly for GRacket
            (installed during the build process).}}})
