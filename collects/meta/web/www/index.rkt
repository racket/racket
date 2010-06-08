#lang at-exp s-exp "shared.rkt"

(require "code.rkt" "download.rkt" "outreach+research.rkt" racket/string)

(define (doc s)
  (string-append "http://docs.racket-lang.org/" s))

(define-struct example (code desc))
(define-struct (cmdline-example example) ())
(define-struct (scribble-example example) ())
(define-struct (graphical-example example) ())

(define (desc . strs) (apply div strs))
(define (elemcode . strs) (apply tt strs))

(provide index)
(define index
  (page #:link-title "About" #:window-title "Racket"
        #:extra-headers (delay more.css)
    (div class: 'whatpane
         @span{@span[class: 'whatb]{Racket} is a programming language.})
    (div class: 'aboutpane
         (div class: 'panetitle "Start Quickly")
         (div class: 'downloadbutton download-button)
         (alts-panel
          @; --- Each example here should be exactly 7 lines long ---
          @; Candidates for initial example: ------------------------
          (list
           (example
            @code{
               #lang racket
               ;; Finds Racket sources in all subdirs
               (for ([path (in-directory)])
                 (when (regexp-match? #rx"[.]rkt$" path)
                   (printf "source file: ~a\n" path)))


            }
            @desc{
             The @elemcode{in-directory} function constructs
             a sequence that walks a directory tree (starting with
             the current directory, by default) and generates
             paths in the tree. The @elemcode{for}
             form binds @elemcode{p} to each path in the sequence,
             and @elemcode{regexp-match?} applies a pattern to
             the path.})
           (example
            @code{
               #lang web-server/insta
               ;; A "hello world" web server
               (define (start request)
                 '(html
                   (body "Hello World")))


            }
            @desc{
             This example implements a web server using the
             @elemcode{web-server/insta} language. Each time a connection
             is made to the server, the @elemcode{start} function is
             called to get the HTML to send back to the client.
            })
           (example
            @code{
               #lang racket  ; An echo server
               (define listener (tcp-listen 12345))
               (let echo-server ()
                 (define-values (in out) (tcp-accept listener))
                 (thread (lambda () (copy-port in out)
                                    (close-output-port out)))
                 (echo-server))
            }
            @desc{
               Racket makes it easy to use TCP sockets and spawn
               threads to handle them. This program starts a server
               at TCP port 12345 that echos anything a client sends
               back to the client.
            })
           (example
            @code{
               #lang racket
               ;; Report each unique line from stdin
               (let ([saw (make-hash)])
                 (for ([line (in-lines)])
                   (unless (hash-ref saw line #f)
                     (displayln line))
                   (hash-set! saw line #t)))
            }
            @desc{
               Uses a hash table to record previously seen lines.
               You can run this program in DrRacket, but it makes more
               sense from the command line.
            })
          )
          @; Additional examples: --------------------------
          (list
           (graphical-example
            @code{
               #lang racket  ; A picture
               (require 2htdp/image)
               (let sierpinski ([n 6])
                 (if (zero? n)
                     (triangle 2 'solid 'red)
                     (let ([next (sierpinski (- n 1))])
                       (above next (beside next next)))))
            }
            @desc{
             The @elemcode{2htdp/image} library provides easy-to-use
             functions for constructing images, and DrRacket can display
             an image result as easily as it can display a number result.
             In this case, a @elemcode{sierpinski} function is defined and
             called (at the same time) to generate a Sierpinski triangle
             of depth 6.
            })
          (graphical-example
            @code{
               #lang racket/gui ; A GUI guessing game
               (define f (new frame% [label "Guess"]))
               (define n (random 5))  (send f show #t)
               (define ((check i) btn evt)
                 (message-box "." (if (= i n) "Yes" "No")))
               (for ([i (in-range 5)])
                 (make-object button% (format "~a" i) f (check i)))
            }
            @desc{
               This simple guesing game demonstates Racket's
               class-based GUI toolkit.  The
               @elemcode{frame%} class implements a
               top-level window, and @elemcode{button%} obviously
               implements a button. The @elemcode{check} function
               defined here produces an function that is used
               for the button's callback action.
            })
           (example
            @code{
               #lang racket ; Simple web scraper
               (require net/url net/uri-codec)
               (define (let-me-google-that-for-you str)
                 (let* ([g "http://www.google.com/search?q="]
                        [u (string-append g (uri-encode str))]
                        [rx #rx"(?<=<h3 class=\"r\">).*?(?=</h3>)"])
                   (regexp-match* rx (get-pure-port (string->url u)))))
            }
            @desc{
               Add a call to @elemcode{let-me-google-that-for-you} to
               get a list of search results.
            })
           (cmdline-example
            @code{
               #lang racket
               ;; A dice-rolling command-line utility
               (command-line
                #:args (dice sides)
                (for ([i (in-range (string->number dice))])
                  (displayln
                   (+ 1 (random (string->number sides))))))
            }
            @desc{
               Playing a game but no dice on hand?
               Let Racket roll for you. The @elemcode{command-line}
               form makes sure that the right number of
               arguments are provided and automatically
               implements the @tt{--help} switch.
            })
           (example
            @code{
               #lang racket
               ;; Print the Greek alphabet
               (for ([i (in-range 25)])
                 (displayln
                  (integer->char
                   (+ i (char->integer #\u3B1)))))

            }
            @desc{
               The only reason we use the encoded form of a
               character @elemcode{#\u3B1}
               instead of the more direct form @elemcode{#\α} is that we
               don't trust your browser to render it correctly. DrRacket
               is perfectly happy with @elemcode{#\α}.
            })
           (graphical-example
            @code{
               #lang htdp/bsl ; Any key inflates the balloon
               (require 2htdp/image) (require 2htdp/universe)
               (define (balloon b) (circle b "solid" "red"))
               (define (blow-up b k) (+ b 5))
               (define (deflate b) (max (- b 1) 1))
               (big-bang 50 (on-key blow-up) (on-tick deflate)
                         (to-draw balloon 200 200))
            }
            @desc{
               Racket's mission includes education at all levels.
               This program uses the @elemcode{htdp/bsl} teaching
               language, the @elemcode{2htdp/image} library for
               creating pictures in the teaching languages, and the
               @elemcode{2htdp/universe} library for interactive
               animations.
            })
           (example
            @code{
               #lang lazy
               ;; An infinite list:
               (define fibs
                 (list* 1 1 (map + fibs (cdr fibs))))

               ;; Print the 1000th Fibonacci number:
               (print (list-ref fibs 1000))
            }
            @desc{
               And now for something completely different.
               The @elemcode{lazy} language is more like Haskell
               than Lisp, so feel free to build an infinite list
               and look at only part of it.
            })
           (example
            @code{
               #lang typed/racket
               ;; Using higher-order occurrence typing
               (define-type SrN (U String Number))
               (: tog ((Listof SrN) -> String))
               (define (tog l)
                 (apply string-append (filter string? l)))
               (tog (list 5 "hello " 1/2 "world" (sqrt -1)))
            }
            @desc{
               Racket's type system is designed to let you
               add types after you've worked for a while
               in untyped mode @|ndash| even if your untyped program
               wouldn't fit nicely in a conventional type system.
            })
           (scribble-example
            @code|{
               #lang scribble/base
               @; Generate a PDF or HTML document
               @title{Bottles --- @italic{Abridged}}

               @(apply itemlist
                 (for/list ([n (in-range 100 0 -1)])
                   @item{@(format "~a" n) bottles.}))
            }|
            @desc{
               This program uses the @elemcode{scribble/base} language for
               generating documents using a prose-friendly syntax.
            })
          ))
         @p{@a[href: (doc "quick/")]{Draw more pictures} or
              @a[href: (doc "more/")]{build a web server from scratch}.
              Racket includes both @a[href: (doc "")]{batteries}
              and a @a[href: (doc "drracket/")]{programming environment},
              so @a[href: (doc "getting-started/index.html")]{get started}!})
    (table class: 'threepanes
           (tr
            (td
             (div class: 'panetitle "Grow your Program")
             @p{Racket's
             @a[href: (doc "guide/intro.html#(part._.Interacting_with_.Racket)")]{interactive mode}
             encourages experimentation,
             and quick scripts easily compose into larger systems. Small scripts
             and large systems both benefit from @a[href:
             (doc "guide/performance.html")]{native-code JIT compilation}. When
             a system gets too big to keep in your head, you can add
             @a[href: (doc "ts-guide/index.html")]{static types}.})
            (td
             (div class: 'panetitle "Grow your Language")
             @p{@a[href: (doc "guide/languages.html")]{Extend Racket} whenever
                you need to.  Mold it to better suit your tasks without
                sacrificing
                @a[href: (doc "guide/dialects.html")]{interoperability} with
                existing libraries and without having to modify the
                @a[href: (doc "guide/intro.html")]{tool chain}.
                When less is more, you can remove parts of a language
                or start over and build a new one.})
            (td
             (div class: 'panetitle "Grow your Skills")
             @p{Whether you're just @-htdp{starting out}, want to know more
                about programming language @-plai{applications} or
                @-redex{models}, looking to @continue{expand your horizons},
                or ready to dive into @outreach+research{research}, Racket can
                help you become a better programmer and system builder.})))))

(define (alts-panel l1 l2)
  (define l (append l1 l2))
  (apply div class: 'slideshow
         @script/inline[type: "text/javascript"]{
           var showing = 0;
           var help_showing = false;
           var kind = [ @(string-join (for/list ([i (in-list l)])
                                        (cond
                                         [(cmdline-example? i) "\"cmd\""]
                                         [(scribble-example? i) "\"scrib\""]
                                         [(graphical-example? i) "\"dr\""]
                                         [else "\"any\""]))
                                       ",") ];
           function change_show_to(new_showing) {
              elem = document.getElementById("frame" + showing);
              elem.style.display = "none";
              elem = document.getElementById("helpframe" + showing);
              elem.style.display = "none";
              elem = document.getElementById("howto" + kind[showing]);
              elem.style.display = "none";
              showing = new_showing;
              elem = document.getElementById("frame" + showing);
              elem.style.display = "block";
              elem = document.getElementById("helpframe" + showing);
              elem.style.display = "block";
              elem = document.getElementById("howto" + kind[showing]);
              elem.style.display = "block";
              elem = document.getElementById("rewindbutton");
              elem.style.color = ((showing == 0) ? "#aaa" : "#444");
              elem = document.getElementById("advancebutton");
              elem.style.color = ((showing == @(sub1 (length l))) ? "#aaa" : "#444");
           }
           function change_show(amt) {
              change_show_to((showing + amt + @(length l)) % @(length l));
           }
           function advance_show() {
              if (showing < @(sub1 (length l))) change_show(1);
              return false;
           }
           function rewind_show() {
              if (showing > 0) change_show(-1);
              return false;
           }
           function show_help() {
              elem = document.getElementById("helppanel");
              elem.style.display = "block";
              help_showing = true;
              return false;
           }
           function hide_help() {
              elem = document.getElementById("helppanel");
              elem.style.display = "none";
              help_showing = false;
              return false;
           }
           function toggle_help() {
               if (help_showing)
                 return hide_help();
               else
                 return show_help();
           }
         }
         (div
          class: 'buttonpanel
          (a href: "#"
             id: "rewindbutton"
             class: 'slideshowbutton
             onclick: "return rewind_show()"
             style: "color: #aaa;"
             "<")
          (a href: "#"
             id: "advancebutton"
             class: 'slideshowbutton
             onclick: "return advance_show()"
             ">")
          (a href: "#"
             class: 'slideshowhbutton
             onclick: "return toggle_help()"
             "?")
          (div class: 'hiddenhelp
               id: "helppanel"
               style: "display: none"
               (div class: 'helpcontent
                    (a href: "#"
                       class: 'closebutton
                       onclick: "return hide_help()"
                       "close")
                    (apply div class: 'helpdesc
                           (for/list ([elem l]
                                      [pos (in-naturals)])
                             (div class: 'helpdeskframe
                                  id: (format "helpframe~a" pos)
                                  style: "display: none"
                                  (example-desc elem))))
                    @div[class: 'helptext]{Form and function names in the code
                          are hyperlinked to documentation, so click on them
                          for more information.}
                    @div[class: 'helptext
                         id: "howtoany"]{To run the example, install Racket,
                          start DrRacket, paste the example program
                          into the top area in DrRacket,
                          and click the Run button. Alternatively, save the
                          program to a file and run @tt{racket} on the file.}
                    @div[class: 'helptext
                         style: "display: none"
                         id: "howtodr"]{To run the example, install Racket,
                          start DrRacket, paste the example program
                          into the top area in DrRacket,
                          and click the Run button.}
                    @div[class: 'helptext
                         id: "howtoscrib"]{To run the example, install Racket,
                          start DrRacket, and paste the example program
                          into the top area in DrRacket.
                          When a program in a Scribble
                          language is opened in DrRacket, a @b{Scribble HTML}
                           button appears for rendering the document to HTML.
                          Click it.}
                    @div[class: 'helptext
                         style: "display: none"
                         id: "howtocmd"]{This example is a command-line script.
                          To run the example, install Racket,
                          paste the example program into a file, and
                          run @tt{racket} on the file with command-line arguments
                          after the filename. Alternatively, for a Unix installation, you can
                          add @tt{#!/usr/bin/env racket} at the top and make the
                          file executable, and then you can run the file directly.})))
         (append
          (for/list ([elem l]
                     [pos (in-naturals)])
            (div class: 'slideshowframe
                 id: (format "frame~a" pos)
                 style: (format "display: ~a" (if (zero? pos) "block" "none"))
                 (example-code elem)))
          (list
           @script/inline[type: "text/javascript"]{
             change_show_to(Math.floor(Math.random()* @(length l1)));
           }))))

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
  @plain[#:referrer (lambda (url) (link rel: "stylesheet" type: "text/css"
                                        href: url title: "default"))]{
    .bodycontent {
      background-image: url('@loud');
      background-repeat: no-repeat;
      background-position: center top;
    }
    .whatpane {
      font-size: medium;
      float: left;
      width: 20%;
    }
    .whatb {
      font-size: large;
      font-weight: bold;
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
      color: #D00;
    }
    .threepanes {
      width: 100%;
    }
    .threepanes td {
      vertical-align: top;
      align: center;
      width: 30%;
      padding: 0.5em;
    }
    .slideshow {
      position: relative;
    }
    .buttonpanel {
      display: block;
      position: absolute;
      left: 100%;
      top: -3ex;
      width: 3em;
      margin: 0em 0em 0em -5em;
    }
    .slideshowbutton, .slideshowhbutton {
      text-decoration: none;
      border: 1px solid #ddd;
      font-weight: bold;
      color: #444;
      padding: 0px 1px 0px 1px;
    }
    .slideshowbutton {
      margin: 0px 1px 0px 1px;
    }
    .slideshowhbutton {
      margin: 0px 1px 0px 10px;
    }
    .hiddenhelp {
      width: 0em;
      margin-left: 2em;
    }
    .helpcontent {
      width: 20em;
      background-color: #FFE;
      padding: 10px;
      margin-top: 3px;
      border: 1px solid black;
    }
    .closebutton {
      font-size: small;
      margin-bottom: 1em;
    }
    .helptext, .helpdesc {
      margin-top: 0.5em;
    }
    .helptext {
      font-size: small;
    }
    .downloadbutton {
      position: relative;
      float: right;
      left: 1em;
      top: -1em;
      height: 0em;
      width: 1em;
      margin: 0em -1em 0em 0em;
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
