#lang plt-web

(require (only-in "../www/resources.rkt" www-site))

(define docs-site (site "stubs/docs" 
                        #:page-style? #f
                        #:meta? #t
                        #:share-from www-site))

(provide documentation)
(define documentation
  ;; This is a stub page to get the header for tweaked doc pages
  (page #:site docs-site
        #:file "" #:link-title "Documentation" #:window-title "{{{TITLE}}}"
        "\n{{{BODY}}}\n"))

(void
 (plain #:site docs-site
        #:file "doc-site.js"
        @list{
              @(site-navbar-dynamic-js docs-site)
              AddOnLoad(AddNavbarToBody);
              }))
(void
 (plain #:site docs-site
        #:file "doc-site.css"
        @list{
              @"@"import url("@(site-css-path docs-site)")
              .navsettop, .tocset { top: 60px; }
              .versionbox { top: 64px; }
              }))
