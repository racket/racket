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
  ;; the default target argument duplicate the behavior in "utils.rkt"
  (define (copyfile file [target (basename file)])
    (list target (copyfile-resource (in-here file) (web-path dir target))))
  (define (copydir/flat dir* [target (basename dir*)])
    (let loop ([dir* (in-here dir*)])
      (append-map
       (λ(p) (define path (build-path dir* p))
             (cond [(file-exists? path)
                    (define str (path->string p))
                    `([,str ,(copyfile-resource path (web-path dir str))])]
                   [(directory-exists? path) (loop path)]))
       (directory-list dir*))))
  (define (writefile file . contents)
    (list file (resource (web-path dir file)
                         (file-writer output (list contents "\n")))))
  (define (pagefile file . contents)
    (list file
          (apply page (string->symbol (regexp-replace #rx"[.]html$" file ""))
                 contents)))
  `(,(writefile "plt.css" racket-style)
    ;; resource files are everything in this directory, copied to the toplevel
    ;; web directory (if there's ever a name clash, the rendering will throw an
    ;; error, and this can be changed accordingly)
    ,@(copydir/flat "resources")
    ;; the following resources are not used directly, so their names are
    ;; irrelevant
    @,writefile["google5b2dc47c0b1b15cb.html"]{
      google-site-verification: google5b2dc47c0b1b15cb.html}
    @,writefile["BingSiteAuth.xml"]{
      <?xml version="1.0"?>
      <users><user>140BE58EEC31CB97382E1016E21C405A</user></users>}
    ;; #t (the default) => no-op file, good to avoid error-log lines
    ,(let* ([t (if (eq? #t robots) "Disallow:" robots)]
            [t (and t (list "User-agent: *\n" t))])
       (if t (writefile "robots.txt" t) '(#f #f)))
    ;; There are still some clients that look for a favicon.ico file
    ,(copyfile "resources/plticon.ico" "favicon.ico")
    @,pagefile["page-not-found.html"]{
      @h1[style: "text-align: center; margin: 3em 0 1em 0;"]{
        Page not found}
      @(λ xs (table align: 'center (tr (td (pre xs))))){
        > (@a[href: "/"]{(uncaught-exception-handler)}
           (*(+(*)(*(+(*)(*)(*)(*)(*))(+(*)(*)(*)(*)(*))(+(*)(*)(*)(*))))@;
             (+(*)(*)(*)(*))))
        uncaught exception: 404}}
    ;; set the 404 page in htaccess instead of in the conf file, so we get it
    ;; only in sites that we generate here
    ,(let* ([t (and htaccess "ErrorDocument 404 /page-not-found.html")]
            [t (if (boolean? htaccess) t (list htaccess "\n" t))])
       (if t (writefile ".htaccess" t) '(#f #f)))))

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
