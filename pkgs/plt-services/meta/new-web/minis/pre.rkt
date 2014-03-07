#lang plt-web
(require "../www/resources.rkt"
         "../testing.rkt"
         plt-web/style)

(provide installers)

(define pre-site (site "pre"
                       #:url (rewrite-for-testing "http://pre.racket-lang.org/")
                       #:share-from www-site))

(define (main id)
  @page[#:site pre-site
        #:id id
        #:width 'full]{
  @columns[10 #:center? #t #:row? #t #:center-text? #f]{
   @h3{Snapshot Builds}}
  @columns[8 #:center? #t #:row? #t #:center-text? #f]{
      @ul{@li{@a[href: "http://www.cs.utah.edu/plt/snapshots/"]{
                University of Utah}}
          @li{@a[href: "http://plt.eecs.northwestern.edu/snapshots/"]{
                Northwestern University}}}}})

;; Generate at both "installers.html" (traditional path)
;; and "index.html" (old entry point, now subsumed)
(define installers (main 'installers))
(void (main 'index))
