#lang plt-web

(provide identity-headers
         register-identity)

;; Include the returned header in `#:page-headers` for each site:
(define (identity-headers)
  (list
   @script{
    (function(i,s,o,g,r,a,m){i['GoogleAnalyticsObject']=r;i[r]=i[r]||function(){
    (i[r].q=i[r].q||[]).push(arguments)},i[r].l=1*new Date();a=s.createElement(o),
    m=s.getElementsByTagName(o)[0];a.async=1;a.src=g;m.parentNode.insertBefore(a,m)
    })(window,document,'script','//www.google-analytics.com/analytics.js','ga');

    ga('create', 'UA-48767493-1', 'racket-lang.org');
    ga('send', 'pageview');
   }))

;; Call this function for each site:
(define (register-identity site)
  ;; the following resources are not used directly, so their names are
  ;; irrelevant
  @plain[#:site site #:file "google5b2dc47c0b1b15cb.html"]{
    google-site-verification: google5b2dc47c0b1b15cb.html}
  @plain[#:site site #:file "BingSiteAuth.xml"]{
    <?xml version="1.0"?>
    <users><user>140BE58EEC31CB97382E1016E21C405A</user></users>}
  (void))
