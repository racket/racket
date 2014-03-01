#lang plt-web
(require "../www/resources.rkt"
         plt-web/style)

(provide installers)

(define pre-site (site "pre"
                       #:url "http://pre.racket-lang.org/"
                       #:share-from www-site))

(define installers
  @page[#:site pre-site #:width 'full]{
  @columns[10 #:center? #t #:row? #t #:center-text? #f]{
   @h3{Snapshot Builds}}
  @columns[8 #:center? #t #:row? #t #:center-text? #f]{
      @ul{@li{@a[href: "http://www.cs.utah.edu/plt/snapshots/"]{
                University of Utah}}
          @li{@a[href: "http://plt.eecs.northwestern.edu/snapshots/"]{
                Northwestern University}}}}})
