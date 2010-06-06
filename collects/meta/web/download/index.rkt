#lang at-exp s-exp "shared.rkt"

(require racket/string "data.rkt")

(define (in-ftp . paths)
  (string-join (cons "/var/ftp/pub/racket" paths) "/"))

(define docs       (symlink  (in-ftp "docs")))
(define installers (symlink  (in-ftp "installers")))

(provide index)
(define index
  (page #:link-title "Download" #:window-title "Download Racket"
    @ul{@li{Current @a[href: `(,installers "/recent")]{installers}
            (or @a[href: installers]{all versions}).}
        @li{Current documentation in
            @a[href: `(,docs "/recent/html")]{HTML} and in
            @a[href: `(,docs "/recent/pdf")]{PDF}
            (or @a[href: docs]{all versions}).}}))
