#lang scribble/html
@(require racket/format)
@(define (css url) @link[href: url rel: "stylesheet" type: "text/css"]{})
@(define (icon name) @i[class: name]{})
@(define (row . content) (apply div class: "row" content))
@(define (js . args) @script[type: "text/javascript" @(apply literal args) "\n"])
@(define (tagline l) @span[style: "font-style: italic" l])

@(define (panetitle l) @div[class: "panetitle" l])

@(define (growbox title . body)
   @columns[4 (panetitle (string-append "Grow your " title)) (apply p body)])
@(define (docelem kw name link . text)
   (apply p @a[href: link]{@strong[kw]: @|name| } text))

@(define (sectitle name) @columns[10 #:center? #t #:row? #t]{@h3[name]})

@(define (print-num n)
  (list-ref 
   '("one" "two" "three" "four" "five" "six" "seven" "eight" "nine" "ten" "eleven" "twelve" "thirteen") 
   (sub1 n)))
@(define (columns n #:row? [row? #f] #:center? [center? #f] #:tag [tag div] #:push [push #f] . body)
  (define d (apply tag class: (~a (print-num n) " columns" 
                                  (if center? " centered" "") 
                                  (if push (~a " push_" (print-num push)) "")) body))
  (if row? (row d) d))

@(define prev @img[src: "img/prev.png"  style: "width: 50px"])
@(define next @img[src: "img/next.png"  style: "width: 50px"])

@; The new racket web pages

@doctype['html]

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
   @a[class: "five columns logo" href: ""]{
     @img[class: "logo" src: "img/logo.png"]}
   @ul[class: "five columns"]{
     @li{@a[href: "https://pkg.racket-lang.org"]{Packages}}
     @li{@a[href: "https://docs.racket-lang.org"]{Documentation}}
     @li{@a[href: "https://blog.racket-lang.org"]{Blog}}
     @li{@button[class: "medium metro info btn icon-left entypo icon-install"]{
       @a[href: "#"]{Download}}}}}}

 @columns[10 #:row? #t #:center? #t]{
  @h2[style: "font-size: 180%; margin-bottom: 10pt"]{
  @strong{Racket} @|nbsp mdash nbsp|
  @tagline{a programmable programming language}}}

@columns[8 #:center? #t #:row? #t
         style: "margin-bottom: 10pt; font-size: 120%; text-align:justify;"]{
Racket is a full-spectrum programming language that inherits from Lisp
and Scheme but also provides dialects that support objects, types,
laziness, and many other paradigms. Racket's module system allows
programmers to write and link together components written in different
dialects.  Racket's libraries range from web servers to distributed
computing and from databases to charts.
}

@div[id: "topcontent"]{
@row{
 @columns[1]
 @columns[7]{
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

@columns[1]

@columns[3 style: "text-color: black"]{
  @h2[style: "font-size: 180%; margin-bottom: 10pt"]{News}
  @p{Racket version 5.3.5 has been released.}
  @p{Racket videos are now available.}
  @p{@a[href: "racketcon.html"]{RacketCon 2013} will be in September in Boston.}}}


@columns[12 #:row? #t #:center? #t style: "text-align:justify;font-size: 120%; margin-top: 20pt; "]{
@a[href: "http://docs.racket-lang.org/quick/"]{Draw more pictures} or
@a[href: "http://docs.racket-lang.org/more/"]{build a web server from scratch}.  Racket includes both
@a[href: "http://docs.racket-lang.org/"]{batteries} and a @a[href: "http://docs.racket-lang.org/drracket/"]{programming environment},
so @a[href: "http://docs.racket-lang.org/getting-started/"]{get started}!
}}

@sectitle{Go Further}

@row[id: 'growboxes]{


@growbox["Program"]{Racket's
@a[href: "http://docs.racket-lang.org/guide/intro.html#(part._.Interacting_with_.Racket)"]{interactive
mode} encourages experimentation, and quick scripts easily compose
into larger systems.  Small scripts and large systems both benefit
from
@a[href: "http://docs.racket-lang.org/guide/performance.html"]{native-code
JIT compilation} When a system gets too big to keep in your head, you
can add
@a[href: "http://docs.racket-lang.org/ts-guide/index.html"]{static
types}.}

@growbox["Language"]{@a[href: "http://docs.racket-lang.org/guide/languages.html"]{Extend
Racket} whenever you need to.  Mold it to better suit your tasks
without sacrificing
@a[href: "http://docs.racket-lang.org/guide/dialects.html"]{interoperability}
with existing libraries and without having to modify the
@a[href: "http://docs.racket-lang.org/guide/intro.html"]{tool chain}.
When less is more, you can remove parts of a language or start over
and build a new one.}

@growbox["Skills"]{Whether you're just
@a[href: "http://htdp.org/"]{starting out}, want to know more about
programming language @a[href: "http://www.plai.org/"]{applications} or
@a[href: "http://redex.racket-lang.org/"]{models}, looking to
@a[href: "http://docs.racket-lang.org/continue/"]{expand your
horizons}, or ready to dive into @a[href: "learning.html"]{research},
Racket can help you become a better programmer and system builder.}}

@sectitle{Documentation}

@row{
@columns[5]{
  @panetitle{For getting started}
  @docelem['Quick "An Introduction to Racket with Pictures" "http://docs.racket-lang.org/quick/"]{
    gives you a taste of Racket.}
  @docelem['More "Systems Programming with Racket" "http://docs.racket-lang.org/more/"]{
    dives much deeper and much faster, showing how to build a complete
    continuation-based web server.}
  @docelem['Guide "Racket" "http://docs.racket-lang.org/guide/"]{
    starts with a tutorial on Racket basics, and then it
    describes the rest of the Racket language.}}


@columns[5 #:push 2]{
  @panetitle{For experienced Racketeers}
  @docelem['Reference "Racket" "http://docs.racket-lang.org/reference/"]{
provides comprehensive coverage of all of Racket.}
  @docelem['Continue "Web Applications in Racket" "http://docs.racket-lang.org/continue/"]{
  describes how to use the
    Racket @a[href: "http://docs.racket-lang.org/web-server/"]{web
    server} to build dynamic web applications.}
  @docelem["Package Management" "Racket" "http://docs.racket-lang.org/pkg/"]{
explains how to install
    @a[href: "https://pkg.racket-lang.org"]{packages}, and how to
    build and distribute your own.}}
}

@sectitle{Community}

@row{
@columns[4]{
@panetitle{News & Events}
@p{@a[href: "racketcon.html"]{RacketCon} — the annual
  Racket meeting, coming up in September.  Previously
  in @a[href: "http://con.racket-lang.org/2012"]{2012}
  and @a[href: "http://con.racket-lang.org/2012"]{2011}.}

@p{@a[href: "http://blog.racket-lang.org/"]{Blog}
  — announcements, helpful hints, and thoughtful rants.}
@p{@a[href: "http://twitter.com/#!/racketlang"]{Twitter}
— short bits of Racket news.}
}

@columns[4]{
@panetitle{Discussion}
@p{@a[href: "http://lists.racket-lang.org/"]{Mailing lists}
  — discussion for using and developing Racket.}
@p{@a[href: "http://racket-lang.org/irc-chat.html"]{IRC}   —
Chat in the @tt[style: "background-color: #d8d8e8;"]{@big{@strong{#racket}}} channel on
@a[href: "http://freenode.net"]{@tt{freenode.net}} — an informal
discussion channel for all things related to Racket.
(@a[href: "https://botbot.me/freenode/racket/"]{Browse the logs}.)}

@p{@a[href: "http://racket-lang.org/people.html"]{People}   —
The people behind Racket.}
}



@columns[4]{
@panetitle{Contributing}
@p{@a[href: "https://github.com/plt/racket/"]{Code}
  — the Racket source code on GitHub.}
@p{@a[href: "https://github.com/plt/racket/wiki"]{Wiki}   —
Useful pages
  include @a[href: "https://github.com/plt/racket/wiki/Intro-Projects"]{Intro
    Projects}
  and @a[href: "https://github.com/plt/racket/wiki/Videos"]{Videos},
  including tutorials, interviews, and more.}
@p{@a[href: "http://www.cs.utah.edu/plt/snapshots"]{Snapshot builds}   —
The freshest versions of Racket.}

@p{@a[href: "http://bugs.racket-lang.org"]{Bug reports}   —
File, query and maybe fix existing reports.}}}

@sectitle{Learning}

@row{
@row{
@div[class: "two columns image rounded" style: "margin-top: 2pt"]{
  @a[href: "http://www.htdp.org"]{@img[src: "img/htdp-cover.gif"]}}
@columns[4]{@panetitle{How to Design Programs}
@p{A principled approach to program design}
@ul{
    @li{Teaching language support built-in to DrRacket}
    @li{Aimed at the programming novice}}}

@columns[4]{@panetitle{Realm of Racket}
  @p{Learn Racket and programming, one game at a time}
  @ul{
    @li{Sample game code comes with the Racket distribution}
    @li{For those just starting out with Racket}
}}
@div[class: "two columns image rounded" style: "margin-top: 2pt"]{
  @a[href: "http://www.realmofracket.com"]{@img[src: "img/racket_cover_web.png"]}}
}}

@row{
@row{
@div[class: "two columns image rounded" style: "margin-top: 2pt"]{
  @a[href: "http://cs.brown.edu/~sk/Publications/Books/ProgLangs/2007-04-26/"]{@img[src: "img/plai-cover.jpg"]}}
@columns[4]{@panetitle{PLAI}
@p{Foundations of programming languages}
@ul{
    @li{Understand the features that make languages tick}
    @li{For undergraduates, graduate students, and experts}}}

@columns[4]{@panetitle{Semantics Engineering with PLT Redex}
  @p{Lightweight automation for semantics}
  @ul{
    @li{Model your own programming language semantics}
    @li{For the working language engineer}
}}
@div[class: "two columns image rounded" style: "margin-top: 2pt"]{
  @a[href: "http://redex.racket-lang.org/"]{@img[src: "img/redex-cover.jpg"]}}
}}


  @;<!-- Grab Google CDN's jQuery, with a protocol relative URL; fall back to local if offline -->
  @script[src: "http://ajax.googleapis.com/ajax/libs/jquery/1.9.1/jquery.min.js"]
  @js{window.jQuery || document.write('<script src="/js/libs/jquery-1.9.1.min.js"><\/script>')}

  @script[src: "js/libs/gumby.min.js"]
  @script[src: "js/plugins.js"]
  @script[src: "js/main.js"]



}
