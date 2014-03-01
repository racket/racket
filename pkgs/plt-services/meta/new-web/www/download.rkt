#lang plt-web

(require "resources.rkt"
         (prefix-in download: "../download/index.rkt")
         "../download/download-pages.rkt"
         racket/runtime-path)

(provide download)
(define download download:index)

;; For old references that go to "www/download/", make a copy of
;; the main download page:
(define www-download-site (site "www/download"
                                #:share-from www-site
                                #:meta? #f))
(void
 @page[#:site www-download-site
       #:file "index.html"
       #:title "Download" #:window-title "Download Racket"
       #:part-of 'download #:width 'full]{
    @(render-download-page)})

#|
(define-runtime-path img-dir "img")
(define images (list (copyfile #:site www-site (build-path img-dir "download.png"))
                     (copyfile #:site www-site (build-path img-dir "download-dark.png"))))

(provide download-button)
(define (download-button)
  @text{
    @script/inline[type: "text/javascript"]{
      @; Don't load all images here -- it causes a delay when loading the
      @; page instead, do it only when needed, and also set a timer to do it
      @; after loading the page.  This makes it so that there's almost never
      @; a delay when loading the page, and also no delay when switching the
      @; image.
      var rollovers = false, the_download_button = false;
      function init_rollovers() {
        if (!rollovers) {
          rollovers = [ new Image(), new Image() ];
          rollovers[0].src = "@(car images)";
          rollovers[1].src = "@(cadr images)";
          the_download_button = document.getElementById("download_button");
        }
      }
      function set_download_image(n) {
        if (!rollovers) init_rollovers();
        the_download_button.src = rollovers[n].src;
      }
      setTimeout(init_rollovers, 400);
    }
    @a[href: (url-of download)
       onmouseover: "set_download_image(1);"
       onmouseout: "set_download_image(0);"]{
      @img[id: "download_button" src: (car images) style: "border-width: 0;"
           alt: "Download Racket" title: "Download Racket"]}})
|#
