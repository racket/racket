#lang plt-web
(require plt-web/style
         racket/runtime-path
         "irc.rkt"
         (prefix-in pre: "../minis/pre.rkt")
         (only-in "../stubs/docs.rkt" docs-path)
         (only-in "../rcon/resources.rkt" rcon)
         "../identity.rkt"
         (only-in "../stubs/pkgs.rkt"))

;; TODO
;; -- add links in top paragraph
;; Remaining Eli comments:
;; -- use links
;; -- indentation

(require "resources.rkt" "code.rkt" "download.rkt" "learning.rkt" "people.rkt"
         "community.rkt")

(register-identity www-site)

(define-runtime-path img-dir "img")
(define-runtime-path js-dir "js")
(define-runtime-path css-dir "css")

(struct example (title code desc))

(define ((example-with-help . help) code desc #:title title)
  (example title code (list desc help)))
(define generic-example
  @example-with-help{
    @p{To run the example, install Racket, start DrRacket, paste the example
       program into the top area in DrRacket, and click the Run button.
       Alternatively, save the program to a file and run @tt{racket} on the
       file.}})
(define cmdline-example
  @example-with-help{
    @p{This example is a command-line script.  To run the example, install
       Racket, paste the example program into a file, and run @tt{racket} on
       the file with command-line arguments after the filename.  Alternatively,
       for a Unix installation, you can add @tt{#!/usr/bin/env racket} at the
       top and make the file executable, and then you can run the file
       directly.}})
(define scribble-example
  @example-with-help{
    @p{To run the example, install Racket, start DrRacket, and paste the
       example program into the top area in DrRacket.  When a program in a
       Scribble language is opened in DrRacket, a @b{Scribble HTML} button
       appears for rendering the document to HTML.  Click it.}})
(define graphical-example
  @example-with-help{
    @p{To run the example, install Racket, start DrRacket, paste the example
       program into the top area in DrRacket, and click the Run button.}})

(define desc p)
(define (elemcode . strs) (apply tt strs))

(define examples
  (lazy
  @; --- Each example here should be at most 7 lines long ------------
  (list
   @; Candidates for initial example: --------------------------------
   (list
    (generic-example #:title "Find Racket files" ; -----------------------------------------------
     @code{#lang racket
           ;; Finds Racket sources in all subdirs
           (for ([path (in-directory)]
		 #:when (regexp-match? #rx"[.]rkt$" path))
	     (printf "source file: ~a\n" path))}
     @desc{The @elemcode{in-directory} function constructs a sequence that
       walks a directory tree (starting with the current directory, by default)
       and generates paths in the tree.  The @elemcode{for} form binds
       @elemcode{path} to each path in the sequence, and @elemcode{regexp-match?}
       applies a pattern to the path.})
    (generic-example #:title "Simple Web Server" ; -----------------------------------------------
     @code{#lang web-server/insta
           ;; A "hello world" web server
           (define (start request)
             (response/xexpr
              '(html
                (body "Hello World"))))}
     @desc{This example implements a web server using the
       @elemcode{web-server/insta} language.  Each time a connection is made to
       the server, the @elemcode{start} function is called to get the HTML to
       send back to the client.})
    (generic-example #:title "TCP Echo Server"; -----------------------------------------------
     @code{#lang racket  ; An echo server
           (define listener (tcp-listen 12345))
           (let echo-server ()
             (define-values (in out) (tcp-accept listener))
             (thread (lambda () (copy-port in out)
                                (close-output-port out)))
             (echo-server))}
     @desc{Racket makes it easy to use TCP sockets and spawn threads to handle
       them.  This program starts a server at TCP port 12345 that echos
       anything a client sends back to the client.})
    (generic-example #:title "Unique Lines"; -----------------------------------------------
     @code{#lang racket
           ;; Report each unique line from stdin
	   (define seen (make-hash))
	   (for ([line (in-lines)])
	     (unless (hash-ref seen line #f)
	       (displayln line))
	     (hash-set! seen line #t))}
     @desc{Uses a hash table to record previously seen lines.  You can run this
       program in DrRacket, but it makes more sense from the command line.}))
   @; Additional examples: -------------------------------------------
   (list
    (graphical-example #:title "Sierpinski Triangle"; ---------------------------------------------
     @code{#lang racket  ; A picture
           (require 2htdp/image)
	   (let sierpinski ([n 8])
             (cond
	       [(zero? n) (triangle 2 'solid 'red)]
	       [else (define t (sierpinski (- n 1)))
   		     (freeze (above t (beside t t)))]))}
     @desc{The @elemcode{2htdp/image} library provides easy-to-use functions
       for constructing images, and DrRacket can display an image result as
       easily as it can display a number result.  In this case, a
       @elemcode{sierpinski} function is defined and called (at the same time)
       to generate a Sierpinski triangle of depth 8.})
    (graphical-example #:title "GUI Programming"; ---------------------------------------------
     @code{#lang racket/gui ; A GUI guessing game
           (define f (new frame% [label "Guess"]))
           (define n (random 5))  (send f show #t)
           (define ((check i) btn evt)
             (message-box "." (if (= i n) "Yes" "No")))
           (for ([i (in-range 5)])
             (make-object button% (format "~a" i) f (check i)))}
     @desc{This simple guesing game demonstates Racket's class-based GUI
       toolkit.  The @elemcode{frame%} class implements a top-level window, and
       @elemcode{button%} obviously implements a button. The @elemcode{check}
       function defined here produces an function that is used for the button's
       callback action.})
    (generic-example #:title "Web Scraper"; -----------------------------------------------
     @code{#lang racket ; Simple web scraper
           (require net/url net/uri-codec)
           (define (let-me-google-that-for-you str)
             (define g "http://www.google.com/search?q=")
             (define u (string-append g (uri-encode str)))
             (define rx #rx"(?<=<h3 class=\"r\">).*?(?=</h3>)")
             (regexp-match* rx (get-pure-port (string->url u))))}
     @desc{Add a call to @elemcode{let-me-google-that-for-you} to get a list of
       search results.})
    (cmdline-example #:title "Command Line Dice"; -----------------------------------------------
     @code{#lang racket
           ;; A dice-rolling command-line utility
           (command-line
            #:args (dice sides)
            (for ([i (in-range (string->number dice))])
              (displayln
               (+ 1 (random (string->number sides))))))}
     @desc{Playing a game but no dice on hand?  Let Racket roll for you.  The
       @elemcode{command-line} form makes sure that the right number of
       arguments are provided and automatically implements the @tt{--help}
       switch.})
    (generic-example #:title "Greek Letters"; -----------------------------------------------
     @code{#lang racket
           ;; Print the Greek alphabet
           (for ([i (in-range 25)])
             (display
              (integer->char
               (+ i (char->integer #\α)))))}
     (list
     @span{This prints out: }
     @|br|
     @elemcode{αβγδεζηθικλμνξοπρςστυφχψω}
     @|br|
     @desc{You can also spell @elemcode{#\α} as
       @elemcode{#\u3B1} to stay within ASCII.
       Fortunately, Racket and DrRacket are both
       perfectly happy to use Unicode characters, and DrRacket comes
       with shortcuts for inserting them.}))
    (graphical-example #:title "Functional Animations"; ---------------------------------------------
     @code{#lang htdp/bsl ; Any key inflates the balloon
           (require 2htdp/image) (require 2htdp/universe)
           (define (balloon b) (circle b "solid" "red"))
           (define (blow-up b k) (+ b 5))
           (define (deflate b) (max (- b 1) 1))
           (big-bang 50 (on-key blow-up) (on-tick deflate)
                     (to-draw balloon 200 200))}
     @desc{Racket's mission includes education at all levels.  This program
       uses the @elemcode{htdp/bsl} teaching language, the
       @elemcode{2htdp/image} library for creating pictures in the teaching
       languages, and the @elemcode{2htdp/universe} library for interactive
       animations.})
    (generic-example #:title "Lazy Programming"; -----------------------------------------------
     @code{#lang lazy
           ;; An infinite list:
           (define fibs
             (list* 1 1 (map + fibs (cdr fibs))))
           @||
           ;; Print the 1000th Fibonacci number:
           (print (list-ref fibs 1000))}
     @desc{And now for something completely different.  The @elemcode{lazy}
       language is more like Haskell than Lisp, so feel free to build an
       infinite list and look at only part of it.})
    (generic-example #:title "Typed Racket"; -----------------------------------------------
     @code{#lang typed/racket
           ;; Using higher-order occurrence typing
           (define-type SrN (U String Number))
           (: tog ((Listof SrN) -> String))
           (define (tog l)
             (apply string-append (filter string? l)))
           (tog (list 5 "hello " 1/2 "world" (sqrt -1)))}
     @desc{Racket's type system is designed to let you add types after you've
       worked for a while in untyped mode — even if your untyped program
       wouldn't fit nicely in a conventional type system.})
    (scribble-example #:title "Scribble for Documentation"; ----------------------------------------------
     @code|{#lang scribble/base
            @; Generate a PDF or HTML document
            @title{Bottles --- @italic{Abridged}}
            @(apply itemlist
              (for/list ([n (in-range 100 0 -1)])
                @item{@(format "~a" n) bottles.}))}|
     @desc{This program uses the @elemcode{scribble/base} language for
       generating documents using a prose-friendly syntax.})
    (graphical-example #:title "Plot and graphs"; ---------------------------------------------
     @code{#lang racket   ; draw a graph of cos
           (require plot) ; and deriv^3(cos)
           (define ((deriv f) x)
             (/ (- (f x) (f (- x 0.001))) 0.001))
           (define (thrice f) (lambda (x) (f (f (f x)))))
           (plot (list (function ((thrice deriv) sin) -5 5)
                       (function cos -5 5 #:color 'blue)))}
     @desc{This program uses the @elemcode{plot} library to draw plots of
       functions.  Note that the plots are actual value, which DrRacket shows
       in graphical form.})
    (generic-example #:title "Sending email"; -----------------------------------------------
     @code{#lang racket ; Sending email from racket
           (require net/sendmail)
           (sleep (* (- (* 60 4) 15) 60)) ; 4h - 15m
           (send-mail-message
            (getenv "EMAIL") "Parking meter alert!"
            (list (getenv "EMAIL")) null null
            '("Time to go out and move your car."))}
     @desc{Racket comes with plenty of libraries.})
    (generic-example #:title "FFI"; -----------------------------------------------
     @code{#lang racket ; Simple use of the FFI
           (require ffi/unsafe)
           (define mci-send-string
             (get-ffi-obj "mciSendStringA" "Winmm"
               (_fun _string [_pointer = #f] [_int = 0]
                     [_pointer = #f] -> [ret : _int])))
           (mci-send-string "play sound.wav wait")}
     @desc{Using the FFI means that you're not limited to using Racket
       libraries: pulling out a foreign function is easy, and can even be done
       dynamically on the REPL.})
    ;; Is this effective without any highlights?
    (generic-example #:title "Datalog"; -----------------------------------------------
     @code{#lang datalog
           ancestor(A, B) :- parent(A, B).
           ancestor(A, B) :-
             parent(A, C), D = C, ancestor(D, B).
           parent(john, douglas).
           parent(bob, john).
           ancestor(A, B)?}
     @desc{Racket is useful for building other languages.  This example uses
      the pre-packaged Racket implementation of Datalog, a logic programming
      language.  If you use this from DrRacket, you'll see that it provides
      proper highlighting, Check Syntax, and a Datalog-specific REPL.})
    #; ; Not easy to present something like this.
    (generic-example #:title "Customizing your language"; -----------------------------------------------
     @code{#lang racket
           (provide (except-out (all-from-out racket)
                                #%top #%app)
                    (rename-out [top #%top] [app #%app]))
           (define-syntax-rule (top . x) 'x)
           (define-syntax-rule (app f . xs)
             (if (hash? f) (hash-ref f . xs) (f . xs)))
           }
     @desc{TODO, and use this example:
           @pre{#lang s-exp "foo"
                (define x (make-hasheq))
                (hash-set! x A B)
                (x A)}})
    ))))

(define blurb "Racket is a programming language")

(define (sectitle name) @columns[10 #:center? #t #:row? #t]{@h3[name]})

(define (book-image . l)
  @div[class: "two columns image rounded" style: "margin-top: 2pt" l])

(define (growbox title . body)
  @columns[4 (panetitle (string-append "Grow your " title)) (p body)])
(define (docelem kw name link . text)
  (p @a[href: link]{@strong[kw]: @|name| } text))



(define prev @img[src: (copyfile #:site www-site (build-path img-dir "prev.png"))  style: "width: 50px"])
(define next @img[src: (copyfile #:site www-site (build-path img-dir "next.png"))  style: "width: 50px"])

(define explain
      @a[href: "#" class: "switch" id: "question_button" style: "white-space: nowrap;"]{
         @img[width: "14" height: "15"
              src: (copyfile #:site www-site (build-path img-dir "arrow.png"))]@;
         explain})

(provide index)

(define index
  @page[#:site www-site
        #:window-title "The Racket Language"
        #:width 'full
        #:description
        @'{Racket is a modern programming language in the Lisp/Scheme family, @;
           suitable for a wide range of applications.  @;
           Racket provides a rich language extension API, the DrRacket @;
           integrated development environment, and many batteries-included @;
           libraries.}
        @; Ask google to not use the ODP description
        #:extra-headers @list{@css[(copyfile #:site www-site (build-path css-dir "frontpage-style.css"))]
                              @css[(copyfile #:site www-site (build-path css-dir "scribble.css"))]
                              @script[src: (copyfile #:site www-site (build-path js-dir "slideshow.js"))]
                              @meta[name: "robots" content: "NOODP"]}]{
 @columns[10 #:row? #t #:center? #t]{
  @h2[style: "font-size: 180%; margin-bottom: 10pt"]{
  A programmable programming language}}

@columns[8 #:center? #t #:row? #t
         style: "margin-bottom: 10pt; font-size: 120%; text-align:justify;"]{
@strong{Racket} is a full-spectrum programming language. It goes
beyond Lisp and Scheme with dialects that support @a[class: "introlink" href: (docs-path "guide/classes.html")]{objects}, 
@a[class: "introlink" href: (docs-path "ts-guide/")]{types},
@a[class: "introlink" href: (docs-path "lazy/")]{laziness},
 and more. Racket enables programmers to link components
written in @a[class: "introlink" href: (docs-path "guide/dialects.html")]{different dialects},
 and it empowers programmers to create
new, @a[class: "introlink" href: (docs-path "guide/languages.html")]{project-specific dialects}. 
Racket's libraries support
applications from @a[class: "introlink" href: (docs-path "web-server/")]{web servers} and 
@a[class: "introlink" href: (docs-path "db/")]{databases} to 
@a[class: "introlink" href: (docs-path "gui/")]{GUIs} and 
@a[class: "introlink" href: (docs-path "plot/")]{charts}.}
                                                                            
@(apply slideshow-explain (force examples))

@div[id: "topcontent"]{
@row{
 @columns[1]
 @columns[7]{
   @h2[style: "font-size: 180%; margin-bottom: 10pt"]{Start Quickly}
   @div[style: "position: relative"]{
     @p[class: "wide_only"
        style: "position: absolute; top: 0%; left: 100%;"]{@explain}
     @p[class: "narrow_only"
        style: "position: absolute; top: 0%; right: 0%;"]{@explain}
     @a[href: "#" class: "toggle narrow_only prev_toggle"
        gumby-trigger: ".unique_lines|.web_scraper"
        style: "position: absolute; top: -40%; left: 35%"]{@prev}

     @a[href: "#" class: "toggle narrow_only next_toggle"
        gumby-trigger: ".unique_lines|.web_scraper"
        style: "position: absolute; top: -40%; right: 35%"]{@next}

     @a[href: "#" class: "toggle wide_only prev_toggle"
        gumby-trigger: ".unique_lines|.web_scraper"
        style: "position: absolute; top: 40%; left: -15%"]{@prev}

     @a[href: "#" class: "toggle wide_only next_toggle"
        gumby-trigger: ".unique_lines|.web_scraper"
        style: "position: absolute; top: 40%; right: -15%"]{@next}
   @(apply slideshow-panel (force examples))}}

@columns[2]

@columns[2 style: "text-color: black"]{
  @h2[style: "font-size: 144%; margin-bottom: 6pt;"]{News}
  @p[style: "font-size: 80%;"]{Racket @a[href: "http://blog.racket-lang.org/2014/05/racket-v601.html"]{version 6.0.1} is out!}

  @p[style: "font-size: 80%;"]{@rcon[2014]{RacketCon 2014} will take place
  in St.@|nbsp|Louis on September 20. 
  @a[href: "https://www.eventbrite.com/e/racketcon-2014-tickets-11408046775"]{Registration} is now open.}}}


@columns[12 #:row? #t #:center? #t style: "text-align:justify;font-size: 120%; margin-top: 20pt; "]{
@a[href: (docs-path "quick/")]{Learn Racket with pictures} or
@a[href: (docs-path "more/")]{build a web server from scratch}.  Racket comes with
@a[href: (docs-path)]{batteries included} and a full-featured 
@a[href: (docs-path "drracket/")]{programming environment}, 
plus lots of @a[href: "http://pkgs.racket-lang.org"]{packages},
so @a[href: (docs-path "getting-started/")]{get started}!
}}

@sectitle{Go Further}

@row[id: 'growboxes]{


@growbox["Program"]{Racket's
@a[href: (docs-path "guide/intro.html#(part._.Interacting_with_.Racket)")]{interactive
mode} encourages experimentation, and quick scripts easily compose
into larger systems.  Small scripts and large systems both benefit
from
@a[href: (docs-path "guide/performance.html")]{native-code
JIT compilation}. When a system gets too big to keep in your head, you
can add
@a[href: (docs-path "ts-guide/index.html")]{static
types}.}

@growbox["Language"]{@a[href: (docs-path "guide/languages.html")]{Extend
Racket} whenever you need to.  Mold it to better suit your tasks
without sacrificing
@a[href: (docs-path "guide/dialects.html")]{interoperability}
with existing libraries and without having to modify the
@a[href: (docs-path "guide/intro.html")]{tool chain}.
When less is more, you can remove parts of a language or start over
and build a new one.}

@growbox["Skills"]{Whether you're just
@a[href: "http://htdp.org/"]{starting out}, want to know more about
programming language @a[href: "http://www.plai.org/"]{applications} or
@a[href: "http://redex.racket-lang.org/"]{models}, looking to
@a[href: (docs-path "continue/")]{expand your
horizons}, or ready to dive into @a[href: "learning.html"]{research},
Racket can help you become a better programmer and system builder.}}

@sectitle{Documentation}

@row{
@columns[5]{
  @panetitle{For getting started}
  @docelem['Quick "An Introduction to Racket with Pictures" (docs-path "quick/")]{
    gives you a taste of Racket.}
  @docelem['More "Systems Programming with Racket" (docs-path "more/")]{
    dives much deeper and much faster, showing how to build a complete
    continuation-based web server.}
  @docelem['Guide "Racket" (docs-path "guide/")]{
    starts with a tutorial on Racket basics, and then it
    describes the rest of the Racket language.}}


@columns[5 #:push 2]{
  @panetitle{For experienced Racketeers}
  @docelem['Reference "Racket" (docs-path "reference/")]{
provides comprehensive coverage of all of Racket.}
  @docelem['Continue "Web Applications in Racket" (docs-path "continue/")]{
  describes how to use the
    Racket @a[href: (docs-path "web-server/")]{web
    server} to build dynamic web applications.}
  @docelem["Package Management" "Racket" (docs-path "pkg/")]{
explains how to install
    @a[href: "http://pkgs.racket-lang.org"]{packages}, and how to
    build and distribute your own.}}
}

@sectitle{Community}

@row{
@columns[4]{
@panetitle{News & Events}
@p{@rcon[#f]{RacketCon} — The annual
  Racket meeting, coming up in September.  Previously
  @rcon[2013], @rcon[2012], and @rcon[2011].}

@p{@a[href: "http://blog.racket-lang.org/"]{Blog}
  — Announcements, helpful hints, and thoughtful rants.}
@p{@a[href: "http://twitter.com/#!/racketlang"]{Twitter}
— Short bits of Racket news.}
}

@columns[4]{
@panetitle{Discussion}
@p{@a[href: "http://lists.racket-lang.org/"]{Mailing lists}
  — Discussion lists for using and developing Racket.}
@p{@a[href: "http://racket-lang.org/irc-chat.html"]{IRC} —
@irc-content}

@p{@people   —
The people behind Racket.}

@p{@learning   —
Publications and educational resources.}
}



@columns[4]{
@panetitle{Contributing}
@p{@a[href: "https://github.com/plt/racket/"]{Code}
  — The Racket source code on GitHub.}
@p{@a[href: "https://github.com/plt/racket/wiki"]{Wiki}   —
Useful pages
  include @a[href: "https://github.com/plt/racket/wiki/Intro-Projects"]{Intro
    Projects}
  and @a[href: "https://github.com/plt/racket/wiki/Videos"]{Videos},
  including tutorials, interviews, and more.}
@p{@pre:installers{Snapshot builds}   —
The freshest versions of Racket.}

@p{@a[href: "http://bugs.racket-lang.org"]{Bug reports}   —
File, query, and maybe fix existing reports.}}}

@sectitle{Learning}

@row{
@row{
@book-image{
  @a[href: "http://www.htdp.org"]{@img[src: (copyfile #:site www-site (build-path img-dir "htdp-cover.gif"))]}}
@columns[4]{@panetitle{How to Design Programs}
@p{A principled approach to program design.}
@ul{
    @li{Teaching language support is included with DrRacket.}
    @li{Aimed at the programming novice.}}}

@columns[4]{@panetitle{Realm of Racket}
  @p{Learn Racket and programming, one game at a time.}
  @ul{
    @li{Sample game code comes with the Racket distribution.}
    @li{For those just starting out with Racket.}
}}
@book-image{
  @a[href: "http://www.realmofracket.com"]{@img[src: (copyfile #:site www-site (build-path img-dir "racket_cover_web.png"))]}}
}}

@row{
@row{
@book-image{
  @a[href: "http://cs.brown.edu/~sk/Publications/Books/ProgLangs/2007-04-26/"]{
     @img[src: (copyfile #:site www-site (build-path img-dir "plai-cover.jpg"))]}}
@columns[4]{@panetitle{PLAI}
@p{Foundations of programming languages.}
@ul{
    @li{Understand the features that make languages tick.}
    @li{For undergraduates, graduate students, and experts.}}}

@columns[4]{@panetitle{Semantics Engineering with PLT Redex}
  @p{Lightweight automation for semantics.}
  @ul{
    @li{Model your own programming language semantics.}
    @li{For the working language engineer.}
}}
@book-image{
  @a[href: "http://redex.racket-lang.org/"]{@img[src: (copyfile #:site www-site (build-path img-dir "redex-cover.jpg"))]}}
}}

@columns[12 #:row? #t #:center? #t]{@div[class: "thanks"]{@thanks}}
  })

(define (slideshow-explain l1 l2)
  (define l (append l1 l2))
  (for/list ([elem (in-list l)] [pos (in-naturals)])
    @div[class: "modal" id: @list{code-modal@pos}]{
      @div[class: "content"]{
        @a[class: "close switch" gumby-trigger: @list{|#code-modal@pos}]{@i[class: "icon-cancel"]}
        @row{@columns[10 #:center? #t]{
          @h4{@(example-title elem)}
          @pre[style: "font-size: 140%; margin-top: 2%; margin-bottom: 3%;"]{@(example-code elem)}}}
        @row{@columns[10 #:center? #t]{
               @(example-desc elem)
               @p[style: "font-size: 80%;"]{
            Form and function names in the code are hyperlinked to
            documentation, so click on them for more information.}
             }}}}))

(define (slideshow-panel l1 l2)
  (define l (append l1 l2))
  (define button-ids+labels '())
  ;; this separator is shown in non-CSS browsers (e.g., textual ones)
  (define invisible-separator @div[style: "display: none;"]{@br{}@hr{}})
  (define (button txt tip id onclick)
    (set! button-ids+labels (cons (cons id txt) button-ids+labels))
    (a href: "#" id: id onclick: (list onclick "; return false;") title: tip
       nbsp)) ; empty, filled by JS code, so JS-less browsers won't see it
  ;(define next (img src: (copyfile #:site www-site (build-path img-dir "next.png")) width: 10))
  ;(define prev (img src: (copyfile #:site www-site (build-path img-dir "prev.png")) width: 10))
  
  
  (for/list ([elem (in-list l)] [pos (in-naturals)])
    @list{
     @invisible-separator
     @pre[style: "font-size: 140%;"
          class: (append (list "codesnip") (if (zero? pos) (list " active") null)) 
          id: @list{codesnip@pos}]{@(example-code elem)}})
  #;
  (div class: 'slideshow
    (div class: 'buttonpanel
      @button[prev "Previous example"  'rewindbutton  "rewind_show()"]
      @button[next "Next example"      'advancebutton "advance_show()"]
      @button["?"  "Explain this code" 'helpbutton "set_help(!help_showing)"]
      (div class: 'hiddenhelp id: "helppanel" style: "display: none;"
        (div class: 'helpcontent
          @button["close" "Close help" 'closebutton "set_help(false)"]
          (div id: 'helpdesc "") ; placeholder for the descriptions (see below)
          @div[class: 'helptext]{
            Form and function names in the code are hyperlinked to
            documentation, so click on them for more information.})))
    (for/list ([elem (in-list l)] [pos (in-naturals)])
      @list{
        @invisible-separator
        @pre[class: 'slideshowframe id: @list{frame@pos}
             style: @list{display: @(if (zero? pos) "block" "none")@";"}]{
          @(example-code elem)}@;
        @; have the descriptions appear in a logical place and then ...
        @div[id: @list{helpframe@pos} style: "display: none;"]{
          @(example-desc elem)}})
    @invisible-separator
    @script/inline[type: "text/javascript"]{
      @; ... move them to a convenient-for-display place
      var helpdesc = document.getElementById("helpdesc");
      for (var i=0@";" i<@(length l)@";" i++) {
        var help_item = document.getElementById("helpframe"+i);
        help_item.parentNode.removeChild(help_item);
        helpdesc.appendChild(help_item);
      }
      var showing = 0, help_showing = false;
      var frame_s=new Array(), helpframe_s=new Array();
      for (var i=0@";" i<@(length l)@";" i++) {
        frame_s[i] = document.getElementById("frame" + i).style;
        helpframe_s[i] = document.getElementById("helpframe" + i).style;
      }
      var advbutton_s = document.getElementById("advancebutton").style;
      var rewbutton_s = document.getElementById("rewindbutton").style;
      function set_display(disp) {
        frame_s[showing].display = disp;
        helpframe_s[showing].display = disp;
      }
      function change_show_to(new_showing) {
        set_display("none");
        showing = new_showing;
        set_display("block");
        rewbutton_s.color = (showing==0) ? "#aaaaaa" : "#444444";
        advbutton_s.color =
           (showing==@(sub1 (length l))) ? "#aaaaaa" : "#444444";
      }
      function advance_show() {
        if (showing < @(sub1 (length l))) change_show_to(showing+1);
      }
      function rewind_show() {
        if (showing > 0) change_show_to(showing-1);
      }
      var help_panel_s = document.getElementById("helppanel").style;
      function set_help(show) {
        help_panel_s.display = show ? "block" : "none";
        help_showing = show;
      }
      change_show_to(Math.floor(Math.random() * @(length l1)));
      @; display button texts now, instead of making it part of the html,
      @; so it's not shown on JS-less browsers
      @(add-newlines
        (for/list ([id+label (in-list button-ids+labels)])
          (let ([id (car id+label)] [label (cdr id+label)])
            @list{document.getElementById("@id").innerHTML = '@label'@";"})))
    }))

;; TODO
;; (define screenshots
;;   (let ([image (copyfile #:site www-site (in-here "screenshot.jpg"))])
;;     @a[href: screenshots]{
;;       @img[src: image alt: "[screenshots]" border: 0
;;            style: "margin-bottom: 2px;"]@;
;;       @|br|@small{Screenshots}}))

;; (define tour-video
;;   (page #:site www-site #:title "DrRacket Tour" #:file "tour.html"
;;     (define (center . body)
;;       (table align: 'center style: "margin: 3em 0em;"
;;         (tr (td align: 'center body))))
;;     ;; someone posted a comment saying that adding "&fmt=18" to the url
;;     ;; shows a higher resolution video, but it looks exactly the same.
;;     (define url "http://www.youtube.com/v/vgQO_kHl39g&hl=en")
;;     @center{
;;       @object[type: "application/x-shockwave-flash" data: url
;;               width: (round (* 3/2 425)) height: (round (* 3/2 344))]{
;;         @param[name: "movie" value: url]}}))


;; resources that are specific to the front page

@;(define loud (copyfile #:site www-site (build-path img-dir "loud.png")))


(define more.css
  @plain[#:site www-site
         #:referrer (λ (url) (link rel: "stylesheet" type: "text/css"
                                   href: url title: "default"))]{
    @; TODO: from here to END it's css that should probably be removed;
    @;   after that it's all scribble stuff.
    @;{
    .bodycontent {
      background-image: url('@loud');
      background-repeat: no-repeat;
      background-position: center top;
    }
    }
    .leftpane {
      font-size: medium;
      float: left;
      width: 20%;
    }
    .aboutpane {
      width: 56%;
      margin-right: auto;
      margin-left: auto;
    }
    .panetitle {
      width: 100%;
      font-size: large;
      font-weight: bold;
      color: #dd0000;
    }
    .threepanes {
      width: 100%;
    }
    .threepanes td {
      vertical-align: top;
      margin: auto;
      width: 30%;
      padding: 0.5em;
    }
    .slideshow {
      position: relative;
    }
    .slideshowframe {
      height: 17ex;
    }
    .buttonpanel {
      display: block;
      position: absolute;
      left: 100%;
      top: -3ex;
      width: 3em;
      margin: 0em 0em 0em -5em;
    }
    #advancebutton, #rewindbutton, #helpbutton, #closebutton {
      text-decoration: none;
      border: 1px solid #ddddd;
      font-weight: bold;
      color: #44444;
      background-color: #eeeee;
      padding: 0px 1px 0px 1px;
    }
    #advancebutton, #rewindbutton {
      margin: 0px 1px 0px 1px;
    }
    #helpbutton {
      margin: 0px 1px 0px 10px;
    }
    .hiddenhelp {
      width: 0em;
      margin-left: 2em;
    }
    .helpcontent {
      width: 20em;
      background-color: #ffffee;
      padding: 10px;
      margin-top: 3px;
      border: 1px solid black;
    }
    #closebutton {
      font-size: small;
      margin-bottom: 1em;
    }
    .helptext, #helpdesc {
      margin-top: 0.5em;
    }
    .helptext {
      font-size: small;
    }
    .downloadbutton {
      position: relative;
      float: right;
    }
    @; END
    @;
    .codecomment {
      color: #c2741f;
    }
    .codeparenthesis {
      color: #843c24;
    }
    .codeconstant, .codestring {
      color: #228b22;
    }
    .codeid, .codemodpath {
      color: #262680;
    }
    .codeimportid {
      color: blue;
    }
    .codeimportform {
      font-weight: bold;
    }
    .codelinkimportid {
      color: blue;
      text-decoration: none;
    }
    .codelinkimportform {
      font-weight: bold;
      color: black;
      text-decoration: none;
    }
    .codelinkimportid:hover {
      text-decoration: none;
    }
    .codelinkimportform:hover {
      text-decoration: none;
    }
    .codemodpath:hover {
      text-decoration: none;
    }
    .codesnip {
      display: none;
    }
    .codesnip.active {
      display: block;
    }
  })
