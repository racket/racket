#lang at-exp racket/base

(require scribble/html)

;; These are some resources that are shared across different toplevel
;; sites.  They could be included from a single place, but then when one
;; machine crashes the rest won't work right.  (Note: do not add
;; resources that are specific to only one site here, do so in the
;; site's "resources.rkt" file)

(require "utils.rkt")

(provide make-resource-files
         navbar-style page-sizes font-family) ; needed for the blog template

;; robots is passed as #:robots in define-context, and htaccess as #:htaccess;
;; they can be #t (the default) for the standard ones, or some text that gets
;; added to the standard contents -- which is the user-agent line and the
;; ErrorDocument respectively.
(define (make-resource-files page dir robots htaccess)
  (define (copyfile file [target file])
    (copyfile-resource (in-here file) (web-path dir target)))
  (define (writefile file . contents)
    (resource (web-path dir file) (file-writer output (list contents "\n"))))
  `([logo  ,(copyfile  "logo.png")]
    [icon  ,(copyfile  "plticon.ico")]
    [style ,(writefile "plt.css" racket-style)]
    ;; the following resources are not used directly, so their names are
    ;; irrelevant
    [verification:google
     @,writefile["google5b2dc47c0b1b15cb.html"]{
       google-site-verification: google5b2dc47c0b1b15cb.html}]
    [verification:bing
     @,writefile["BingSiteAuth.xml"]{
       <?xml version="1.0"?>
       <users><user>140BE58EEC31CB97382E1016E21C405A</user></users>}]
    [robots
     ;; #t (the default) => no-op file, good to avoid error-log lines
     ,(let* ([t (if (eq? #t robots) "Disallow:" robots)]
             [t (and t (list "User-agent: *\n" t))])
        (and t (writefile "robots.txt" t)))]
    ;; Seems like there are still some clients that look for a favicon.ico file
    [favicon ,(copyfile "plticon.ico" "favicon.ico")]
    [404
     @,page['page-not-found]{
       @h1[style: '("text-align: center; margin: 3em 0 1em 0;")]{
         Page not found}
       @(λ xs (table align: 'center (tr (td (pre xs))))){
         > (@a[href: "/"]{(uncaught-exception-handler)}
            (*(+(*)(*(+(*)(*)(*)(*)(*))(+(*)(*)(*)(*)(*))(+(*)(*)(*)(*))))@;
              (+(*)(*)(*)(*))))
         uncaught exception: 404}}]
    ;; set the 404 page in htaccess instead of in the conf file, so we get it
    ;; only in sites that we generate here
    [.htaccess
     ,(let* ([t (and htaccess "ErrorDocument 404 /page-not-found.html")]
             [t (if (boolean? htaccess) t (list htaccess "\n" t))])
        (and t (writefile ".htaccess" t)))]))

(define page-sizes
  @list{
    margin-left: auto;
    margin-right: auto;
    width: 45em;
  })
(define font-family
  @list{
    font-family: Optima, Arial, Verdana, Helvetica, sans-serif;
  })

(define navbar-style
  ;; All of these are made to apply only inside `racketnav', so the styles can
  ;; be used in places with their own CSS (eg, blog.racket-lang.org)
  @list{
    .racketnav {
      background-color: #000000;
      color: #ffffff;
      margin-bottom: 1em;
      padding: 0.5em 0em;
      white-space: nowrap;
    }
    .racketnav a {
      color: #ffffff;
      text-decoration: none;
    }
    .racketnav .navcontent {
      @page-sizes
      @font-family
    }
    .racketnav .navtitle {
      font-size: xx-large;
      font-weight: bold;
    }
    .racketnav .navitem {
      text-decoration: none;
      font-size: 88%;
    }
    .racketnav .navlink a {
      padding: 0em 1em;
    }
    .racketnav .navcurlink a {
      padding: 0em 1em;
      background-color: #555555;
    }
    .racketnav .navlink    a:hover,
    .racketnav .navcurlink a:hover {
      background-color: #888888;
    }
    .racketnav .navlinkcell {
      text-align: center;
    }
    .racketnav .helpiconcell {
      text-align: right;
      vertical-align: top;
    }
    .racketnav .helpicon {
      font-weight: bold;
      font-size: 88%;
    }
  })

(define racket-style
  @list{
    @; ---- generic styles ----
    html {
      overflow-y: scroll;
    }
    body {
      color: black;
      background-color: white;
      @font-family
      margin: 0px;
      padding: 0px;
    }
    a {
      text-decoration: none;
    }
    a:hover {
      text-decoration: underline;
    }
    @; ---- content styles ----
    .bodycontent {
      @page-sizes
    }
    @; ---- styles for the navbar ----
    @navbar-style
    @; ---- styles for extras ----
    .parlisttitle {
      margin-bottom: 0.5em;
    }
    .parlistitem {
      margin-bottom: 0.5em;
      margin-left: 2em;
    }
  })
