#lang at-exp s-exp "shared.rkt"

(require "../download/download-pages.rkt")

(provide download-button download)

(define download
  @page[#:link-title "Download" #:window-title "Download Racket"
        #:file "download/" #:part-of 'download]{
    @(render-download-page)})

(define download-button
  (let ([img1 (copyfile (in-here "download.png"))]
        [img2 (copyfile (in-here "download-dark.png"))])
    @list{
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
            rollovers[0].src = "@img1";
            rollovers[1].src = "@img2";
            the_download_button = document.getElementById("download_button");
          }
        }
        function set_download_image(n) {
          if (!rollovers) init_rollovers();
          the_download_button.src = rollovers[n].src;
        }
        setTimeout(init_rollovers, 400);
      }
      @a[href: "download/"
         onmouseover: "set_download_image(1);"
         onmouseout: "set_download_image(0);"]{
        @img[id: "download_button" src: img1 style: "border-width: 0;"
             alt: "Download Racket" title: "Download Racket"]}}))
