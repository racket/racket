#lang plt-web

(require plt-web/style
         racket/dict racket/match
         "resources.rkt"
         "utils.rkt"
         "../identity.rkt"
         (prefix-in 2011: "2011/all.rkt")
         (prefix-in 2012: "2012/all.rkt")
         (prefix-in 2013: "2013/all.rkt"))

(provide index)

(register-identity con-site)

(define group-rate-url
  "http://doubletree.hilton.com/en/dt/groups/personalized/S/STLUSDT-RAC-20140919/index.jhtml?WT.mc_id=POG")

(define hotel @a[href: "http://www.stlunionstationhotel.com/"]{Union Station DoubleTree})

(define fogus @name["http://www.fogus.me/"]{Michael Fogus})
(define stchang @name["http://www.ccs.neu.edu/home/stchang/"]{Stephen Chang})
(define brianm @name["http://brian.mastenbrook.net/"]{Brian Mastenbrook})
(define danprager @name["https://www.youpatch.com/"]{Daniel Prager})
(define davidv @name["https://github.com/david-vanderson/"]{David Vanderson})

;; TODO better design would be to have a struct for talks
(define (speaker->title s)
  (define talks
    `((,fogus . "Extracting a Goose from a Klein Bottle")
      (,mbutterick . "Like a Blind Squirrel in a Ferrari")
      (,stchang . "A Boost-Inspired Graph Library for Racket")
      (,jbc . "Sound: why is it so darn imperative?")
      (,mflatt . "Carry on Making that Racket")
      (,tonyg . "Minimart: Organizing Squabbling Actors")
      (,gregh . "Emacs à la mode DrRacket")
      (,jay . "Get Bonus! Infinite Functional Entertainment at 60 FPS!")
      (,brianm . "Racket in Production")
      (,danprager . "YouPatch: A Racket-powered startup")
      (,ntoronto . "Purely Functional 3D in Typed Racket")
      (,davidv . "Racket for a networked multiplayer game")))
  (dict-ref talks s))
(define (speaker->slides s)
  (define slides
    `((,stchang . "stchang.pdf")
      (,mflatt . "mflatt.pdf")
      (,tonyg . "tonyg.pdf")
      (,jay . "jay.pdf")
      (,brianm . "mastenbrook.pdf")
      (,danprager . "prager.pdf")
      (,davidv . "vanderson.pdf")
      (,ntoronto . "toronto.pdf")
      (,fogus . "fogus.pdf")
      (,jbc . "clements.pdf")))
  (dict-ref slides s #f))
(define (speaker->code s)
  (define code
    `((,mbutterick . "http://pollenpub.com/")
      (,gregh . "https://github.com/greghendershott/racket-mode")
      (,jay . "https://github.com/get-bonus/get-bonus")
      (,davidv . "https://github.com/david-vanderson/warp")
      (,ntoronto . "https://github.com/ntoronto/pict3d")
      (,tonyg . "https://github.com/tonyg/minimart")
      (,jbc . "https://github.com/jbclements/RSound")))
  (dict-ref code s #f))
(define (speaker->video s)
  (define video
    `((,fogus . "https://www.youtube.com/watch?v=2ZrM0aYaqJM&list=PLXr4KViVC0qI9t3lizitiFJ1cFIeN2Gdh&index=1")
      (,mflatt . "https://www.youtube.com/watch?v=Uw8m4QF4k1E&list=PLXr4KViVC0qI9t3lizitiFJ1cFIeN2Gdh&index=2")
      (,danprager . "https://www.youtube.com/watch?v=8psnTEjYIEA&list=PLXr4KViVC0qI9t3lizitiFJ1cFIeN2Gdh&index=3")
      (,davidv . "https://www.youtube.com/watch?v=Fuz0BtltU1g&list=PLXr4KViVC0qI9t3lizitiFJ1cFIeN2Gdh&index=4")
      (,jay . "https://www.youtube.com/watch?v=_x0Ob2HY8C4&list=PLXr4KViVC0qI9t3lizitiFJ1cFIeN2Gdh&index=5")
      (,ntoronto . "https://www.youtube.com/watch?v=t3xdv4UP9-U&list=PLXr4KViVC0qI9t3lizitiFJ1cFIeN2Gdh&index=6")
      (,brianm . "https://www.youtube.com/watch?v=GAmZIgs72wA&list=PLXr4KViVC0qI9t3lizitiFJ1cFIeN2Gdh&index=7")
      (,tonyg . "https://www.youtube.com/watch?v=LIJHb8E4Mhk&list=PLXr4KViVC0qI9t3lizitiFJ1cFIeN2Gdh&index=8")
      (,jbc . "https://www.youtube.com/watch?v=DkIVzHNjNEA&list=PLXr4KViVC0qI9t3lizitiFJ1cFIeN2Gdh&index=9")
      (,mbutterick . "https://www.youtube.com/watch?v=IMz09jYOgoc&list=PLXr4KViVC0qI9t3lizitiFJ1cFIeN2Gdh&index=10")
      (,gregh . "https://www.youtube.com/watch?v=QWiteH8PARQ&list=PLXr4KViVC0qI9t3lizitiFJ1cFIeN2Gdh&index=11")
      (,stchang . "https://www.youtube.com/watch?v=SvYJF5HC19w&list=PLXr4KViVC0qI9t3lizitiFJ1cFIeN2Gdh&index=12")))
  (dict-ref video s))
(define (title->anchor t)
  (string-replace t " " "-"))
(define (speaker-slot s)
  (define title (speaker->title s))
  @slot[#f s #:slides (speaker->slides s) #:code (speaker->code s) #:video (speaker->video s)]{
    @a[href: (string-append "#" (title->anchor title))]{@title}})
(define (session time title chair)
  @slot[time #f]{@p{@title} Chair: @chair})

(define speaker-info
  (list
   (list
    fogus
     @p*{Racket is the most amazing language that no one's ever heard of.  This
         seemingly harsh assessment is prelude to a discussion on programming
         language development, innovation, marketing, open source, research,
         Lisp, and education.  While other programming languages have dominated
         the public discourse, Racket's influence on said languages is
         undeniable.  I'll touch on some of these influences during the course
         of the talk placing them within the context of just what a language
         like Racket means within the current, and future software landscapes.}
     @p*{Fogus is a programming language aesthete with experience in expert
         systems, logic programming, and distributed simulation.  He is a
         contributor to Clojure, ClojureScript, Datomic, and Transit. Fogus is
         also the co-author of @em{The Joy of Clojure} and author of
         @em{Functional JavaScript} and the upcoming release @em{The Art of
         Chupacabra Husbandry}.})
    (list
     mbutterick
     @p*{At RacketCon last year, I talked about Pollen, a web-publishing system
         I wrote in Racket. This year, I'll recap what I've learned since then
         about typesetting in Racket, by redesigning Racket's documentation,
         hacking Scribble, and releasing Pollen. Plus: my two great Racket
         ambitions.}
     @p*{Matthew Butterick is a writer, designer, and lawyer in Los Angeles. He
         is the author of @em{Typography for Lawyers} and the creator of
         @a[href: "http://practicaltypography.com/"]{practicaltypography.com}.})
    (list
     stchang
     @p*{The Boost Graph Library (BGL) introduces many novel abstraction
         patterns for graph processing. I borrowed many of the BGL's ideas in
         implementing a graph library for Racket. This talk will show how the
         library turned out to be a nice playground for many of the unique
         features in Racket.}
     @p*{Stephen is a postdoc and recent PhD graduate at Northeastern
         University. In his early years, he worked as an electrical engineer
         before deciding that his life needed more abstraction. So he went off
         to study programming languages and has been hacking in Racket ever
         since.})
    (list
     jbc
     @p*{HtDP and big-bang provide an explicit-state, fully-testable framework
         for simple student apps and games. Adapting this framework to handle
         dynamically generated music is surprisingly difficult. I describe the
         specific challenges of shoehorning music into a stateless and testable
         milieu, and propose a solution, using a hybrid dataflow approach.}
     @p*{John Clements is an Associate Professor at Cal Poly State University in
         San Luis Obispo. He is the author of DrRacket’s Stepper, and the RSound
         library, and this paragraph.})
    (list
     mflatt
     @p*{This talk will provide a brief introduction to Racket and Racketeers,
         a report on recent and current developments, and predictions for the
         future. Bring your questions, and I'll bring my Magic 8 Ball.}
     @p*{Matthew Flatt is a professor at the University of Utah and one of the
         main developers of Racket. He works primarily on Racket's run-time
         system, compiler, macro system, build system, package system,
         documentation language, and graphics/GUI libraries.})
    (list
     tonyg
     @p*{Actors are a great model for managing concurrency and communication
         within programs. The Minimart #lang adds Actors to Racket; but Actors
         alone are not enough. Programmers using Actors are often left to solve
         issues such as event broadcasting, service naming and discovery, and
         even crash-handling and exit signalling, on their own.

         Minimart makes solutions to these problems part of the language
         itself. I'll show how Minimart uses publish/subscribe programming and
         "routing events" to manage and organize whole groups of Actors at a
         time.}
     @p*{Tony Garnock-Jones is a PhD candidate at Northeastern University's
         Programming Research Laboratory, working on applying lessons from
         distributed systems to programming language design.})
    (list
     gregh
     @p*{DrRacket is wonderful for both newcomers and Racket pros.
         Some people do like to use Emacs, especially when working with a wide
         variety of file types and languages. Racket-mode brings some of the
         DrRacket approach and experience to Emacs, hopefully giving you the
         best of both worlds. This talk includes a demonstration and a
         discussion of the implementation in Racket and Elisp.}
     @p*{Before becoming obsessed with Racket, Greg Hendershott founded
         and ran the music software company Cakewalk, and has served as an
         advisor to technology companies such as Roland and JamHub. Soon after
         RacketCon he is joining the autumn batch at Hacker School.})
    (list
     jay
     @p*{Hard real-time embedded systems with tight operating
         environments, a.k.a. console video games, are an exciting and
         challenging place to program functionally. The Get Bonus project is
         an effort to experiment in this space with Racket. This
         progress-report presentation will discuss some of our goals and
         some of the interesting implementations we've made in Racket.}
     @p*{Jay McCarthy is a visiting assistant professor at Vassar
         College and one of the developers of Racket. He works primarily on
         Racket's Web server, package system, networking libraries, and
         special projects, like DrDr.})
    (list
     brianm
      @p*{When electronic products come off the manufacturing line, they
          go through a multi-step program and test process to become sellable
          products. Wearable has been using Racket to automate this process for
          the portable wireless products that we design and manufacture or
          license to high-volume consumer electronics companies such as SanDisk.
          I'll talk about why we chose Racket for our most business-critical
          application (and why it's so critical!), what we've learned across
          three generations of manufacturing fixtures and why we went from a
          monolithic to a distributed system and back to monolithic again. I'll
          also talk about expressing actor-model semantics in Racket and our
          gradual migration from untyped to typed Racket.}
      @p*{Brian Mastenbrook is CTO and cofounder of Wearable Inc, a small
          Chicago company that invented the wireless flash drive and develops
          the AirStash OS that makes it possible. In a past life he worked at
          Motorola on code generators in Common Lisp for five-nines
          telecommunication systems (among other things).})
    (list
     danprager
     @p*{@a[href: "https://www.youpatch.com/"]{youpatch.com} began as a hack in
         Racket to save my wife PatchAndi 10 or so hours of effort to turn an
         image of Groucho Marx into the design for a patchwork quilt, and
         evolved into a bootstrapped startup aimed at democratising the hitherto
         elite art of pixel quilt making.

         In this talk I recount the YouPatch story so far, discuss Racket's
         advantages for exploratory programming, and look at the options that
         face a creative programmer when (s)he comes up with an original idea.}
     @p*{Daniel has been programming creatively since his teenage years in
         the 1980s, starting with Turbo Pascal and Z80 assembly on a 64K CP/M
         machine, and most recently in Racket. In between he took his PhD in
         mathematics (specifically computational General Relativity) before
         crossing into software development and leadership, where he has worked
         in diverse areas, including: devising algorithms for staff scheduling,
         software for medical devices, educational software to teach critical
         thinking, and teaching and coaching Agile approaches to software
         development and business. Nowadays he divides his professional time
         between Agile/Lean coaching and more entrepreneurial endeavours,
         including YouPatch!})
    (list
     ntoronto
      @p*{Efficient 3D engines use scene databases to quickly answer queries
          such as "What must be drawn if the viewer is here and looking this
          direction?" and "Return all non-opaque triangles in back-to-front
          order." Most 3D engines are written in an imperative style, even
          though most scene databases are structured as trees and operations
          on them can be done without destructive updates.

          In this talk, I give a sneak peak at a standalone 3D engine with a
          purely functional API, comprised mostly of combinators that operate on
          scene databases. I intend it to replace Plot's internal 3D engine,
          which draws on Cairo device contexts, but also be flexible and
          efficient enough to render simple game scenes using OpenGL.}
      @p*{Neil Toronto is a recent PhD graduate from Brigham Young University,
          now researching programming language support for reliable mathematical
          computation at University of Maryland, College Park. He writes
          programs to draw pretty pictures in his nonexistent spare time.})
    (list
     davidv
     @p*{I'll talk about using Racket features like easy serialization,
         threads, and eventspaces to smoothly go from a toy prototype to a
         playable networked game.}
     @p*{David Vanderson has been a professional software developer for 10
         years.  He stumbled onto Racket a few years back and recently was
         inspired by the game Artemis to make a coop game in Racket.})))


(define index
  @page[#:site con-site
        #:link-title "RacketCon" #:title "RacketCon"
        #:extra-headers style-header]{
   @columns[12 #:center? #t #:row? #t]{
     @h2{RacketCon: 20 September 2014}

     @columns[6 #:center? #f #:row? #f]{
       @p{@b{(fourth RacketCon)} was held in St. Louis on
          the day after @a[href: "https://thestrangeloop.com/"]{Strange Loop}.}

       @p{@b{@a[href: "http://www.fogus.me/"]{Michael Fogus}} gave the keynote.}

       @p{Videos of the talks are available, and linked to from the schedule.}

       @p{@nbsp}

       @p{@b{Schedule:}}
       @session-sched[
         @slot["9:00-9:30" #f]{Registration}
         @slot["9:30" #f]{Welcome}
         @slot["9:30-10:30" #f]{Keynote}
         @(speaker-slot fogus)
         @slot["10:30-10:50" #f]{Break}
         @(session "10:50-11:50" "Racketeering Essentials" "Matthias Felleisen")
         @(speaker-slot mflatt)
         @(speaker-slot danprager)
         @slot["11:50-13:40" #f]{Lunch}
         @(session "13:40-14:40" "Games and Graphics" "Robby Findler")
         @(speaker-slot davidv)
         @(speaker-slot jay)
         @(speaker-slot ntoronto)
         @slot["14:40-15:00" #f]{Break}
         @(session "15:00-16:00" "Actors and Musicians" "Sam Tobin-Hochstadt")
         @(speaker-slot brianm)
         @(speaker-slot tonyg)
         @(speaker-slot jbc)
         @slot["16:00-16:20" #f]{Break}
         @(session "16:20-17:20" "Libraries and Tools" "Claire Alvis")
         @(speaker-slot mbutterick)
         @(speaker-slot gregh)
         @(speaker-slot stchang)
         @slot["17:20" #f]{Closing Remarks}
       ]

       @p{@nbsp}

       @p{@b{Talks:}}
       @(apply ul
               (for/list ([speaker (in-list speaker-info)])
                 (match-define (list name abstract bio) speaker)
                 @a[id: (title->anchor (speaker->title name))
                    @; to compensate for the header
                    style: "padding-top: 60px; margin-top: -60px"]{
                   @li{@p*{@name — @(or (speaker->title name) "TBA")}
                          @(or abstract "")
                          @(or bio "")}
                       @p*{@(let ([slides (speaker->slides name)])
                              (if slides
                                  @a[href: slides]{[slides]}
                                  ""))
                           @(let ([code (speaker->code name)])
                              (if code
                                  @a[href: code]{[code]}
                                  ""))
                           @(let ([video (speaker->video name)])
                              (if video
                                  @a[href: video]{[video]}
                                  ""))
                           @hr{}}}))}

    @columns[1 #:center? #f #:row? #f]{ }

    @columns[5 #:center? #f #:row? #f]{

      @p*{@b{RacketCon} is a public meeting for everyone interested in Racket:
          developers, contributors, programmers, educators, and by-standers.  It
          is an opportunity for all members of the community to come together to
          share plans, ideas, and enthusiasm.  RacketCon will enable the entire
          Racket community to mingle: to update each other, to exchange ideas, to
          collaborate, and to help shape the future of Racket.}

      @p{@nbsp}

      @p{@b{Sponsors:}
         @div[align: 'center]{
         @p{@font[size: "+3"]{@a[href: "http://mbtype.com"]{Matthew Butterick}}}
         @p{@sponsor["NU ACM" "acm.ccs.neu.edu" 2011:nuacm]}
        }}

      @p{@nbsp}

      @p{@b{Previous years:}}
      @ul{@li{@2013:index}
          @li{@2012:index}
          @li{@2011:index}}}}})
