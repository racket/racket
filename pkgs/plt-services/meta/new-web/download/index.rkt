#lang plt-web

(require "resources.rkt"
         "download-pages.rkt"
         "../identity.rkt")

(provide index)
(define index
  @page[#:site download-site
        #:link-title "Download" #:window-title "Download Racket"
        #:part-of 'download #:width 'full]{
    @(render-download-page)})

(register-identity download-site)
