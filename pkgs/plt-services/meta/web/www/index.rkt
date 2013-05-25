#lang meta/web

(require "resources.rkt" "code.rkt" "download.rkt" "learning.rkt")

(define (doc path . text)
  (apply a href: (list "http://docs.racket-lang.org/" path) text))

(struct example (code desc))

(define ((example-with-help . help) code desc)
  (example code (list desc help)))
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

(define desc div)
(define (elemcode . strs) (apply tt strs))

(define examples
  @; --- Each example here should be at most 7 lines long ------------
  (list
   @; Candidates for initial example: --------------------------------
   (list
    (generic-example ; -----------------------------------------------
     @code{#lang racket
           ;; Finds Racket sources in all subdirs
           (for ([path (in-directory)])
             (when (regexp-match? #rx"[.]rkt$" path)
               (printf "source file: ~a\n" path)))}
     @desc{The @elemcode{in-directory} function constructs a sequence that
       walks a directory tree (starting with the current directory, by default)
       and generates paths in the tree.  The @elemcode{for} form binds
       @elemcode{p} to each path in the sequence, and @elemcode{regexp-match?}
       applies a pattern to the path.})
    (generic-example ; -----------------------------------------------
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
    (generic-example ; -----------------------------------------------
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
    (generic-example ; -----------------------------------------------
     @code{#lang racket
           ;; Report each unique line from stdin
           (let ([saw (make-hash)])
             (for ([line (in-lines)])
               (unless (hash-ref saw line #f)
                 (displayln line))
               (hash-set! saw line #t)))}
     @desc{Uses a hash table to record previously seen lines.  You can run this
       program in DrRacket, but it makes more sense from the command line.}))
   @; Additional examples: -------------------------------------------
   (list
    (graphical-example ; ---------------------------------------------
     @code{#lang racket  ; A picture
           (require 2htdp/image)
           (let sierpinski ([n 8])
             (if (zero? n)
               (triangle 2 'solid 'red)
               (let ([t (sierpinski (- n 1))])
                 (freeze (above t (beside t t))))))}
     @desc{The @elemcode{2htdp/image} library provides easy-to-use functions
       for constructing images, and DrRacket can display an image result as
       easily as it can display a number result.  In this case, a
       @elemcode{sierpinski} function is defined and called (at the same time)
       to generate a Sierpinski triangle of depth 8.})
    (graphical-example ; ---------------------------------------------
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
    (generic-example ; -----------------------------------------------
     @code{#lang racket ; Simple web scraper
           (require net/url net/uri-codec)
           (define (let-me-google-that-for-you str)
             (let* ([g "http://www.google.com/search?q="]
                    [u (string-append g (uri-encode str))]
                    [rx #rx"(?<=<h3 class=\"r\">).*?(?=</h3>)"])
               (regexp-match* rx (get-pure-port (string->url u)))))}
     @desc{Add a call to @elemcode{let-me-google-that-for-you} to get a list of
       search results.})
    (cmdline-example ; -----------------------------------------------
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
    (generic-example ; -----------------------------------------------
     @code{#lang racket
           ;; Print the Greek alphabet
           (for ([i (in-range 25)])
             (displayln
              (integer->char
               (+ i (char->integer #\u3B1)))))}
     @desc{The only reason we use the encoded form of a character
       @elemcode{#\u3B1} instead of the more direct form @elemcode{#\α} is that
       we don't trust your browser to render it correctly.  DrRacket is
       perfectly happy with @elemcode{#\α}.})
    (graphical-example ; ---------------------------------------------
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
    (generic-example ; -----------------------------------------------
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
    (generic-example ; -----------------------------------------------
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
    (scribble-example ; ----------------------------------------------
     @code|{#lang scribble/base
            @; Generate a PDF or HTML document
            @title{Bottles --- @italic{Abridged}}
            @(apply itemlist
              (for/list ([n (in-range 100 0 -1)])
                @item{@(format "~a" n) bottles.}))}|
     @desc{This program uses the @elemcode{scribble/base} language for
       generating documents using a prose-friendly syntax.})
    (graphical-example ; ---------------------------------------------
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
    (generic-example ; -----------------------------------------------
     @code{#lang racket ; Sending email from racket
           (require net/sendmail)
           (sleep (* (- (* 60 4) 15) 60)) ; 4h - 15m
           (send-mail-message
            (getenv "EMAIL") "Parking meter alert!"
            (list (getenv "EMAIL")) null null
            '("Time to go out and move your car."))}
     @desc{Racket comes with plenty of libraries.})
    (generic-example ; -----------------------------------------------
     @code{#lang scheme/base ; Simple use of the FFI
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
    (generic-example ; -----------------------------------------------
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
    (generic-example ; -----------------------------------------------
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
    )))

(define blurb "Racket is a programming language")

(provide set-news-flashes!)
(define news-flashes #f)
(define (set-news-flashes! . text)
  (when news-flashes (error 'set-news-flashes! "text already set"))
  (set! news-flashes text))

(provide index)
(define index
  (page #:link-title "About" #:window-title "The Racket Language"
        #:description
        @'{Racket is a modern programming language in the Lisp/Scheme family, @;
           suitable for a wide range of applications.  @;
           Racket provides a rich language extension API, the DrRacket @;
           integrated development environment, and many batteries-included @;
           libraries.}
        #:extra-headers @list{@meta[name: "robots" content: "NOODP"]
                              @(lazy more.css)}
    @div[class: 'leftpane]{
      @span[style: "font-size: large; font-weight: bold;"]{Racket}
      is a programming language.
      @(and news-flashes (list br (div style: "width: 100%;" news-flashes)))}
    @div[class: 'downloadbutton]{@download-button}
    @div[class: 'aboutpane]{
      @div[class: 'panetitle]{Start Quickly}
      @div{@(apply slideshow-panel examples)}
      @p{@doc["quick/"]{Draw more pictures} or
         @doc["more/"]{build a web server from scratch}.  Racket includes both
         @doc[""]{batteries} and a @doc["drracket/"]{programming environment},
         so @doc["getting-started/"]{get started}!}}
    @((λ xs (table class: 'threepanes
              (tr (map (λ (x) (td (div class: 'panetitle (car x)) (cdr x)))
                       xs))))
      (list "Grow your Program"
        @p{Racket's
           @doc["guide/intro.html#(part._.Interacting_with_.Racket)"]{
             interactive mode}
           encourages experimentation, and quick scripts easily compose into
           larger systems.  Small scripts and large systems both benefit from
           @doc["guide/performance.html"]{native-code JIT compilation}.
           When a system gets too big to keep in your head, you can add
           @doc["ts-guide/index.html"]{static types}.})
      (list "Grow your Language"
        @p{@doc["guide/languages.html"]{Extend Racket} whenever you need to.
           Mold it to better suit your tasks without sacrificing
           @doc["guide/dialects.html"]{interoperability} with existing
           libraries and without having to modify the
           @doc["guide/intro.html"]{tool chain}.  When less is more, you can
           remove parts of a language or start over and build a new one.})
      (list "Grow your Skills"
        @p{Whether you're just @-htdp{starting out}, want to know more about
           programming language @-plai{applications} or @-redex{models},
           looking to @continue{expand your horizons}, or ready to dive into
           @learning{research}, Racket can help you become a better programmer
           and system builder.}))))

(define (slideshow-panel l1 l2)
  (define l (append l1 l2))
  (define button-ids+labels '())
  ;; this separator is shown in non-CSS browsers (e.g., textual ones)
  (define invisible-separator @div[style: "display: none;"]{@br{}@hr{}})
  (define (button txt tip id onclick)
    (set! button-ids+labels (cons (cons id txt) button-ids+labels))
    (a href: "#" id: id onclick: (list onclick "; return false;") title: tip
       nbsp)) ; empty, filled by JS code, so JS-less browsers won't see it
  (div class: 'slideshow
    (div class: 'buttonpanel
      @button["<" "Previous example"  'rewindbutton  "rewind_show()"]
      @button[">" "Next example"      'advancebutton "advance_show()"]
      @button["?" "Explain this code" 'helpbutton "set_help(!help_showing)"]
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
            @list{document.getElementById("@id").textContent = "@label"@";"})))
    }))

;; TODO
;; (define screenshots
;;   (let ([image (copyfile (in-here "screenshot.jpg"))])
;;     @a[href: screenshots]{
;;       @img[src: image alt: "[screenshots]" border: 0
;;            style: "margin-bottom: 2px;"]@;
;;       @|br|@small{Screenshots}}))

;; (define tour-video
;;   (page #:title "DrRacket Tour" #:file "tour.html"
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

(define loud (copyfile (in-here "loud.png")))

(define more.css
  @plain[#:referrer (λ (url) (link rel: "stylesheet" type: "text/css"
                                   href: url title: "default"))]{
    .bodycontent {
      background-image: url('@loud');
      background-repeat: no-repeat;
      background-position: center top;
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
    }})
