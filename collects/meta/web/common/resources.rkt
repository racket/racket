#lang at-exp s-exp meta/web/html

;; These are some resources that are shared across different toplevel
;; sites.  They could be included from a single place, but then when one
;; machine crashes the rest won't work right.  (Note: do not add
;; resources that are specific to only one site here, do so in the
;; site's shared.rkt file)

(require "utils.ss")

(provide make-logo make-icon make-style
         navbar-style page-sizes font-family) ; needed for the blog template

(define ((make-file-copier file) dir)
  (copyfile-resource (in-here file) (web-path dir file)))

(define make-logo (make-file-copier "logo.png"))
(define make-icon (make-file-copier "plticon.ico"))

(define (make-style dir)
  (resource (web-path dir "plt.css")
            (file-writer output (list racket-style "\n"))
            (lambda (url) (link rel: "stylesheet" type: "text/css"
                                href: url title: "default"))))

(define page-sizes
  @text{
    margin-left: auto;
    margin-right: auto;
    width: 45em;
  })
(define font-family
  @text{
    font-family: Optima, Arial, Verdana, Helvetica, sans-serif;
  })

(define navbar-style
  ;; All of these are made to apply only inside `racketnav', so the styles can
  ;; be used in places with their own CSS (eg, blog.racket-lang.org)
  @text{
    .racketnav {
      background-color: #000;
      color: #fff;
      margin-bottom: 1em;
      padding: 0.5em 0em;
      white-space: nowrap;
    }
    .racketnav a {
      color: #fff;
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
      background-color: #555;
    }
    .racketnav .navlink    a:hover,
    .racketnav .navcurlink a:hover {
      background-color: #888;
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
  @text{
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
