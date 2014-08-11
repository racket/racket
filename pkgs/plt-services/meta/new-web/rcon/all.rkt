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

(define speaker-info
  (list
   (list
    "Michael Fogus" "http://www.fogus.me/"
    "Extracting a Goose from a Klein Bottle"
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
    '("Claire Alvis" "https://github.com/calvis" #f #f #f)
    (list
     "Matthew Butterick" "http://practicaltypography.com/"
     "Like a Blind Squirrel in a Ferrari"
     @p*{At RacketCon last year, I talked about Pollen, a web-publishing system
         I wrote in Racket. This year, I'll recap what I've learned since then
         about typesetting in Racket, by redesigning Racket's documentation,
         hacking Scribble, and releasing Pollen. Plus: my two great Racket
         ambitions.}
     @p*{Matthew Butterick is a writer, designer, and lawyer in Los Angeles. He
         is the author of @em{Typography for Lawyers} and the creator of
         @a[href: "practicaltypography.com"]{practicaltypography.com}.})
    (list
     "Stephen Chang" "http://www.ccs.neu.edu/home/stchang/"
     "A Boost-Inspired Graph Library for Racket"
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
     "John Clements" "http://www.brinckerhoff.org/JBCsite/index.html"
     "Stumbling around in the dark: failure partially averted"
     @p*{Cal Poly includes a 10-week team-based domain-specific course for
         incoming freshmen. I teach this course using Racket, in the domain of
         Music. The challenge is to allow students with no programming
         background to create full-featured music applications using only the
         first few sections of HtDP. I report on the successes and failures of
         these teams, illustrating the bizarre but creative code patterns that
         the students exhibit.}
     @p*{John Clements is an Associate Professor at Cal Poly State University in
         San Luis Obispo. He is the author of DrRacket’s Stepper, and the RSound
         library, and this paragraph.})
    (list
     "Matthew Flatt" "https://www.cs.utah.edu/~mflatt/"
     "Carry on Making that Racket"
     @p*{This talk will provide a brief introduction to Racket and Racketeers,
         a report on recent and current developments, and predictions for the
         future. Bring your questions, and I'll bring my Magic 8 Ball.}
     @p*{Matthew Flatt is a professor at the University of Utah and one of the
         main developers of Racket. He works primarily on Racket's run-time
         system, compiler, macro system, build system, package system,
         documentation language, and graphics/GUI libraries.})
    '("Tony Garnock-Jones" "http://homepages.kcbbs.gen.nz/tonyg/" #f #f #f)
    (list
     "Greg Hendershott" "http://www.greghendershott.com/"
     "Emacs à la mode DrRacket"
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
     "Jay McCarthy" "http://jeapostrophe.github.io"
     "Get Bonus! Infinite Functional Entertainment at 60 FPS!"
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
     "Brian Mastenbrook" "http://brian.mastenbrook.net/"
     "Racket in Production"
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
     "Daniel Prager" "https://www.youpatch.com/"
     "YouPatch: A Racket-powered startup"
     @p*{@a[href: "youpatch.com"]{youpatch.com} began as a hack in Racket to
         save my wife PatchAndi 10 or so hours of effort to turn an image of
         Groucho Marx into the design for a patchwork quilt, and evolved into
         a bootstrapped startup aimed at democratising the hitherto elite art
         of pixel quilt making.

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
     "Neil Toronto" "http://students.cs.byu.edu/~ntoronto/"
     "Purely Functional 3D in Typed Racket"
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
     "David Vanderson" "https://github.com/david-vanderson/"
     "Racket for a networked multiplayer game"
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
       @p{@b{(fourth RacketCon)} will be held in St. Louis on
          the day after @a[href: "https://thestrangeloop.com/"]{Strange Loop}.
          Stay turned for more information!}
       @p{@b{@a[href: "https://www.eventbrite.com/e/racketcon-2014-tickets-11408046775"]{Registration}}
          is open. Tickets are $30.}
       @p{@b{@a[href: "http://www.fogus.me/"]{Michael Fogus}} will be giving the keynote.}

       @p{RacketCon will be held at the @|hotel| (which is one of the Strange Loop venues).
          A @a[href: group-rate-url]{group rate} is available for RacketCon attendees.}

       @p{@nbsp}

       @p{@b{Talks:}}
       @(apply ul
               (for/list ([speaker (in-list speaker-info)])
                 (match-define (list name web-page title abstract bio) speaker)
                 @li{@p*{@a[href: web-page]{@name} — @(or title "TBA")}
                     @(or abstract "")
                     @(or bio "")
                     @hr{}}))}

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
