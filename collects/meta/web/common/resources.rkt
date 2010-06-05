#lang at-exp s-exp meta/web/html

;; These are some resources that are shared across different toplevel
;; sites.  They could be included from a single place, but then when one
;; machine crashes the rest won't work right.  (Note: do not add
;; resources that are specific to only one site here, do so in the
;; site's shared.rkt file)

(require "utils.ss")

(provide make-logo make-icon make-style)

(define ((make-file-copier file) dir)
  (copyfile-resource (in-here file) (web-path dir file)))

(define make-logo (make-file-copier "logo.png"))
(define make-icon (make-file-copier "plticon.ico"))

(define (make-style dir)
  (resource (web-path dir "plt.css")
            (file-writer output (list the-style "\n"))
            (lambda (url) (link rel: "stylesheet" type: "text/css"
                                href: url title: "default"))))
(define the-style
  @text{
    @; ---- generic styles ----
    html {
      overflow-y: scroll;
    }
    body {
      color: black;
      background-color: white;
      font-family: Optima, Arial, Verdana, Helvetica, sans-serif;
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
    .titlecontent, .bodycontent {
      margin-left: auto;
      margin-right: auto;
      width: 45em;
    }
    @; ---- styles for the navbar ----
    .navbar {
      background-color: #000;
      color: #fff;
      margin-bottom: 1em;
      padding: 0.5em 0em;
      white-space: nowrap;
    }
    .navbar a {
      color: #fff;
      text-decoration: none;
    }
    .navtitle {
      font-size: xx-large;
      font-weight: bold;
    }
    .navitem {
      text-decoration: none;
      font-size: 88%;
    }
    .navlink a {
      padding: 0em 1em;
    }
    .navcurlink a {
      padding: 0em 1em;
      background-color: #555;
    }
    .navlink    a:hover,
    .navcurlink a:hover {
      background-color: #888;
    }
    .navlinkcell {
      text-align: center;
    }
    .helpiconcell {
      text-align: right;
      text-vertical-align: top;
    }
    .helpicon {
      font-weight: bold;
      font-size: 88%;
    }
    @; ---- styles for extras ----
    .parlisttitle {
      margin-bottom: 0.5em;
    }
    .parlistitem {
      margin-bottom: 0.5em;
      margin-left: 2em;
    }
  })
