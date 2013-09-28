#lang scribble/html

@(define (css url) @link[href: url rel: "stylesheet" type: "text/css"]{})
@(define (icon name) @i[class: name]{})
@(define (row . content) (apply div class: "row" content))
@(define (js . args) @script[type: "text/javascript" @(apply literal args) "\n"])
@(define (tagline l) @span[style: "font-style: italic" l])

@(define prev @img[src: "img/prev.png"  style: "width: 50px"])
@(define next @img[src: "img/next.png"  style: "width: 50px"])

@; The new racket web pages

@literal{<!doctype html>}

@html{
 @head{
  @meta[charset: "utf-8"]

  @;-- Use the .htaccess and remove these lines to avoid edge case issues.
  @;     More info: h5bp.com/b/378 --
  @meta[http-equiv: "X-UA-Compatible" content: "IE=edge,chrome=1"]

  @title{The Racket Language}
  @link[rel: "shortcut icon" href: "favicon.ico" type: "image/x-icon"]

  @;<!-- Mobile viewport optimized: j.mp/bplateviewport -->
  @meta[name: "viewport" 
        content: "width=device-width, initial-scale=1.0, maximum-scale=1"]

  @;<!-- Place favicon.ico and apple-touch-icon.png in the root directory: mathiasbynens.be/notes/touch-icons -->

  @;<!-- CSS: implied media=all -->
  @css["css/gumby.css"]
  @css["css/style.css"]
  @css["css/scribble.css"]


  @; <!-- All JavaScript at the bottom, except for Modernizr / Respond.
  @;      Modernizr enables HTML5 elements & feature detects; Respond is a polyfill for min/max-width CSS3 Media Queries
  @;      For optimal performance, use a custom Modernizr build: www.modernizr.com/download/ -->
  @script[src: "js/libs/modernizr-2.6.2.min.js"]
  }


@div[class: "navbar" gumby-fixed: "top" id: "nav1"]{
  @row{
   @a[class: "toggle" gumby-trigger: "#nav1 > .row > ul" href: "#"]{
     @icon{icon-menu}}
   @a[class: "two columns logo" href: ""]{
     @img[class: "logo" src: "img/logo.png"]}
   @ul[class: "five columns push_four"]{
     @li{@a[href: "https://pkg.racket-lang.org"]{Packages}}
     @li{@a[href: "https://docs.racket-lang.org"]{Documentation}}
     @li{@a[href: "https://blog.racket-lang.org"]{Blog}}
     @li{@button[class: "medium metro info btn icon-left entypo icon-install"]{
       @a[href: "#"]{Download}}}}}}

@row{
 @div[class: "ten columns centered"]{
  @h2[style: "font-size: 180%; margin-bottom: 10pt"]{
  @strong{Racket} @|nbsp mdash nbsp|
  @tagline{a programmable programming language}}}}

@row{
 @div[class: "eight columns centered"
      style: "margin-bottom: 10pt; font-size: 120%; text-align:justify;"]{
Racket is a wide-spectrum programming language that inherits from Lisp
and Scheme but also provides dialects that support objects, types,
laziness, and many other paradigms. Racket's module system allows
programmers to write and link together components written in different
dialects.  Racket's libraries range from web servers to distributed
computing and from databases to charts.
}}

@div[id: "topcontent"]{
@row{
 @div[class: "one columns"]{}
 @div[class: "seven columns"]{
   @h2[style: "font-size: 180%; margin-bottom: 10pt"]{Start Quickly}
   @div[style: "position: relative"]{
     @p[class: "metro primary btn"
        style: "position: absolute; top: -10%; right: 0%;"]{
        @a[href: "#" class: "switch" gumby-trigger: "#modal1"]{
         @icon["icon-help"]}}
     @a[href: "#" class: "toggle narrow_only"
        gumby-trigger: ".unique_lines|.web_scraper"
        style: "position: absolute; top: -40%; left: 35%"]{@prev}

     @a[href: "#" class: "toggle narrow_only"
        gumby-trigger: ".unique_lines|.web_scraper"
        style: "position: absolute; top: -40%; right: 35%"]{@next}

     @a[href: "#" class: "toggle wide_only"
        gumby-trigger: ".unique_lines|.web_scraper"
        style: "position: absolute; top: 40%; left: -15%"]{@prev}

     @a[href: "#" class: "toggle wide_only"
        gumby-trigger: ".unique_lines|.web_scraper"
        style: "position: absolute; top: 40%; right: -15%"]{@next}}}

@div[class: "one column"]{}

@div[class: "three columns" style: "text-color: black"]{
  @h2[style: "font-size: 180%; margin-bottom: 10pt"]{News}
  @p{Racket version 5.3.5 has been released.}
  @p{Racket videos are now available.}
  @p{@a[href: "racketcon.html"]{RacketCon 2013} will be in September in Boston.}}}


@row{
@p[class: "twelve columns centered" style: "text-align:justify;font-size: 120%; margin-top: 20pt; "]{
@a[href: "http://docs.racket-lang.org/quick/"]{Draw more pictures} or
@a[href: "http://docs.racket-lang.org/more/"]{build a web server from scratch}.  Racket includes both
@a[href: "http://docs.racket-lang.org/"]{batteries} and a @a[href: "http://docs.racket-lang.org/drracket/"]{programming environment},
so @a[href: "http://docs.racket-lang.org/getting-started/"]{get started}!
}}}


  @;<!-- Grab Google CDN's jQuery, with a protocol relative URL; fall back to local if offline -->
  @script[src: "http://ajax.googleapis.com/ajax/libs/jquery/1.9.1/jquery.min.js"]
  @js{window.jQuery || document.write('<script src="/js/libs/jquery-1.9.1.min.js"><\/script>')}

  @script[src: "js/libs/gumby.min.js"]
  @script[src: "js/plugins.js"]
  @script[src: "js/main.js"]



}
