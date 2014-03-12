#lang plt-web
(require "../www/resources.rkt"
         "../testing.rkt"
         "../identity.rkt"
         plt-web/style)

(provide installers)

(define pre-site (site "pre"
                       #:url (rewrite-for-testing "http://pre.racket-lang.org/")
                       #:page-headers (identity-headers)
                       #:share-from www-site))

(register-identity pre-site)

(define (main path)
  @page[#:site pre-site
        #:file path
        #:title "Racket Snapshots"
        #:width 'full]{
  @columns[10 #:center? #t #:row? #t #:center-text? #f]{
   @h3{Snapshot Builds}}
  @columns[8 #:center? #t #:row? #t #:center-text? #f]{
      @ul{@li{@a[href: "http://www.cs.utah.edu/plt/snapshots/"]{
                University of Utah}}
          @li{@a[href: "http://plt.eecs.northwestern.edu/snapshots/"]{
                Northwestern University}}}}})

;; Generate at both "installers/" (traditional path)
;; and "index.html" (old entry point, now subsumed)
(define installers (main "installers/index.html"))
(void (main "index.html"))
