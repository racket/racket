#lang at-exp racket/base

(require scribble/html
         racket/runtime-path)

;; These are some resources that are shared across different toplevel
;; sites.  They could be included from a single place, but then when one
;; machine crashes the rest won't work right.  (Note: do not add
;; resources that are specific to only one site here, do so in the
;; site's "resources.rkt" file)

(require "utils.rkt")

(provide make-resource-files)

(define-runtime-path resources-dir "resources")

;; robots is passed as #:robots in define-context, and htaccess as #:htaccess;
;; they can be #t (the default) for the standard ones, or some text that gets
;; added to the standard contents -- which is the user-agent line and the
;; ErrorDocument respectively.
(define (make-resource-files generate? page dir robots htaccess page-style? meta? sharing?)
  ;; the default target argument duplicate the behavior in "utils.rkt"
  (define (copyfile file [target (basename file)])
    (list target 
          (if generate?
              (copyfile-resource (build-path resources-dir file) (web-path dir target))
              (resource (web-path dir target) #f))))
  (define (writefile file . contents)
    (list file
          (resource (web-path dir file)
                    (and generate?
                         (file-writer output (list contents "\n"))))))
  (define (pagefile file . contents)
    (list file
          (if generate?
              (apply page (string->symbol (regexp-replace #rx"[.]html$" file ""))
                     contents)
              (resource (web-path dir file) #f))))
  `(,@(if (not sharing?)
          (list (copyfile "logo-and-text.png" "logo-and-text.png"))
          null)
    ,@(if (and page-style?
               (not sharing?))
          (list
           (copyfile "css/gumby.css" "gumby.css")
           (copyfile "js/libs/jquery-1.9.1.min.js" "jquery-1.9.1.min.js")
           (copyfile "js/plugins.js" "plugins.js")
           (copyfile "js/libs/gumby.min.js" "gumby.min.js")
           (copyfile "js/libs/modernizr-2.6.2.min.js" "modernizr-2.6.2.min.js")
           (copyfile "js/main.js" "main.js")
           (copyfile "fonts/icons/entypo.ttf" "entypo.ttf")
           (copyfile "fonts/icons/entypo.woff" "entypo.woff")
           (copyfile "fonts/icons/entypo.eot" "entypo.eot"))
          null)
    ,@(if (not sharing?)
          ;; Not used in `page-style?` mode, but available for sharing
          ;; for non-`page-style?` sites:
          (list (copyfile "css/gumby-slice.css" "gumby-slice.css"))
          null)
    ,@(if (and page-style?
               (not sharing?))
          (list
           (copyfile "plticon.ico" "plticon.ico")
           (copyfile "logo.png" "logo.png")) ; a kind of backward compatibility, just in case
          null)
    ,@(if meta?
          (list
           ;; #t (the default) => no-op file, good to avoid error-log lines
           (let* ([t (if (eq? #t robots) "Disallow:" robots)]
                  [t (and t (list "User-agent: *\n" t))])
             (if t (writefile "robots.txt" t) '(#f #f)))
           ;; There are still some clients that look for a favicon.ico file
           (copyfile "plticon.ico" "favicon.ico")
           @pagefile["page-not-found.html"]{
             @h3[style: "text-align: center; margin: 3em 0 1em 0;"]{
               Page not found}
             @(λ xs (table align: 'center (tr (td (pre xs))))){
               > (@a[href: "/"]{(uncaught-exception-handler)}
                  (*(+(*)(*(+(*)(*)(*)(*)(*))(+(*)(*)(*)(*)(*))(+(*)(*)(*)(*))))@;
                    (+(*)(*)(*)(*))))
               uncaught exception: 404}}
           ;; set the 404 page in htaccess instead of in the conf file, so we get it
           ;; only in sites that we generate here
           (let* ([t (and htaccess "ErrorDocument 404 /page-not-found.html")]
                  [t (if (boolean? htaccess) t (list htaccess "\n" t))])
             (if t (writefile ".htaccess" t) '(#f #f))))
          null)))
