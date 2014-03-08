#lang plt-web

(require racket/runtime-path
         plt-web/style
         "../resources.rkt"
         "../utils.rkt")

(provide neu nuacm)

(define-runtime-path neu-png "neu.png")
(define-runtime-path nuacm-png "nuacm.png")

(define neu (copyfile #:site con-site neu-png))
(define nuacm (copyfile #:site con-site nuacm-png))

(define-runtime-path mozilla-png "mozilla.png")

(provide index)
(define index
  @page[#:site con-site
        #:file "2011/index.html"
        #:link-title "RacketCon 2011" #:title "RacketCon 2011"
        #:extra-headers style-header]{
   @columns[10 #:center? #t #:row? #t]{
    @h2{RacketCon: 23 & 24 July 2011}
    @p*{@b{RacketCon} is a public meeting for everyone interested in Racket:
        developers, contributors, programmers, educators, and by-standers.  It
        is an opportunity for all members of the community to come together to
        share plans, ideas, and enthusiasm.  RacketCon will enable the entire
        Racket community to mingle: to update each other, to exchange ideas, to
        collaborate, and to help shape the future of Racket.
        @~
        RacketCon will hosted by the
        @a[href: "http://www.ccs.neu.edu/"]{
          College of Computer and Information Science}
        of @a[href: "http://www.neu.edu/"]{Northeastern University}, in Boston,
        Massachusetts.  See @a[href: "#getting"]{Getting to RacketCon} below.
        @~
        @b{The location of RacketCon has changed!}
        See @a[href: "#getting"]{Getting to RacketCon} for information.}

    @h3{Schedule}

    @h3{Saturday, July 23}
    @sched[
      @slot["9:30-10:00" #f]{Breakfast}
      @slot["10:00-10:45" mflatt #:video (yt "wEgaVMOYLEU")]{Racket at @tt{(expt 2 4)}}
      @slot["10:45-11:15" #f]{Break}
      @slot["11:15-11:45" ryanc #:video (yt "AB15shYUU4M")]{Designing macros using @a[class: 'doclink href: "http://docs.racket-lang.org/syntax/stxparse.html"]{@tt{syntax-parse}}}
      @slot["11:45-12:00" @name["http://dorophone.blogspot.com/"]{Vincent Toups}
            #:video (yt "sZ-jGHUWIyA")]{Monadic Parallel Turtle Graphics}
      @slot["12:00-12:15" dyoo #:video (yt "wg9aoJDJNgk")]{Whalesong}
      @slot["12:15-12:30" "Doug Williams"
            #:video (yt "ePZAvb9at8Q")]{Content Generation from Templates}
      @slot["12:30-2:00" #f]{Lunch}
      @slot["2:00-2:45" matthias
            #:video (yt "ngUeVD7OUKo")
            #:slides (F "matthias-slides.pdf")]{
        What's wrong with How to Design Programs@";"
        @br What's new in How to Design Programs 2e}
      @slot["2:45-3:00" "William Dunklau"]{Using Picturing Programs in Grades 7 and 8}
      @slot["3:00-3:15" #f]{Break}
      @slot["3:15-3:30" @name["http://cs.berry.edu/~nhamid/"]{Nadeem Abdul Hamid}]{Web UI Teachpack}
      @slot["3:30-3:45" dvh #:slides (F "dvh-slides.pdf")]{An Object-Oriented World}
      @slot["3:45-4:00" ""]{Short Talks and Demos}
    ]

    @h3{Sunday, July 24}
    @sched[
      @slot["9:30-10:00" #f]{Breakfast}
      @slot["10:00-10:30" rbf]{The Future of DrRacket}
      @slot["10:30-10:45" jay]{Tutorial: Building Web Apps in Racket}
      @slot["10:45-11:00" "Richard Cleis"]{Test Suites for Telescope Control}
      @slot["11:00-11:15" #f]{Break}
      @slot["11:15-11:45" "Doug Williams"]{Knowledge-Based Simulation in Racket}
      @slot["11:45-12:15" pr #:slides (F "pr-slides.pdf")]{Fifteen Hundred Students A Year}
      @slot["12:15-12:45" ryanc]{Tutorial: Database Access & Low-level Libs}
      @slot["12:45-2:00" #f]{Lunch}
      @slot["2:00-2:20" morazanm #:slides (F "marco-slides.pdf")]{The Time of Space Invaders Will Come to Pass}
      @slot["2:20-2:40" jbc]{Teaching first-year students with RSound}
      @slot["2:40-3:00" guillaume]{Designing Error Messages for Novices}
      @slot["3:00-3:15" #f]{Break}
      @slot["3:15-3:30" moskol]{Rewards and Challenges using Racket in a College Computer Science Course}
      @slot["3:30-3:45" "Paul Ojanen"]{Student Revelations after Using Racket, Scratch, and Alice}
      @slot["3:45-4:00" sk]{Demos}
    ]

    @a[name: "getting"]@h3{Getting to RacketCon}

    @p{RacketCon will be held in @b{Shillman Hall, Room 135}, at
       Northeastern University.  Shillman Hall is located
       at 115 Forsyth Street, Boston Massachusetts, and is building
       @a[href: "http://www.northeastern.edu/campusmap/map/qad4.html"]{#30} on
       the @a[href: "http://www.northeastern.edu/campusmap/map"]{campus map}.
       See also the building on
       @a[href: '("http://maps.google.com/?ll=42.337381,-71.090276&"
                  "spn=0.006686,0.009645&layer=c&cbll=42.337349,-71.090029&"
                  "cbp=12,23.55,,0,0&z=17&photoid=po-16598983")]{
         Google Maps}.}

    @h3{Getting there...}

    @dl{
      @dt{by Car:}
      @dd{There is very little on-street parking near Northeastern.
          @[a href: "http://neu.edu/test/parking/guestparking/"]{
            Visitor parking}
          is available at the
          @a[href: "http://neu.edu/test/parking/wheretopark/"]{
            Renaissance Garage},
          a few minutes walk from the conference.  Driving directions from many
          points are available
            @a[href: "http://www.neu.edu/campusmap/directions.html"]{here}.}
      @dt{by Subway:}
      @dd{Northeastern is conveniently located near both the Green and Orange
          subway lines.  The @a[href: "http://www.mbta.com/"]{MBTA} has more
          information.  On the Green ‘E’ line, the Northeastern stop is the
          closest, and appears on the campus map below as the ‘T’ symbol on
          Huntington Avenue.  On the Orange line, the Ruggles stop is closest
          and appears on the campus map below as the ‘T’ symbol in the
          middle-left of the illustration.}
      @dt{by Bicycle, Commuter Rail, Amtrak:}
      @dd{Northeastern is located near the major Amtrak lines, and is also
          accessible by commuter rail and bicycle.  Please contact @|org| for
          more information.}}

    @h3{Where to eat}
    @p{While there are no conference dinner plans, there are numerous
       restaurants near Northeastern:}
    @dl{@dt{@a[href: "http://www.symphonysushi.com/"]{Symphony Sushi}}
        @dd{A sushi restaurant, 5 minutes walk}
        @dt{@a[href: "http://www.phoandi.com/"]{Pho and I}}
        @dd{Thai and Vietnamese food, 5 minutes walk.}
        @dt{@a[href: "http://maps.google.com/maps/place?q=pizzeria+uno+boston&cid=11922407303454006467"]{Pizzeria Uno}}
        @dd{Deep dish pizza, 5 minutes walk.}
        @dt{@a[href: "http://www.bettyswokandnoodle.com/"]{Betty's Wok and Noodle}}
        @dd{Asian-Latin fusion.  6 minutes walk.}
        @dt{@a[href: "http://www.yelp.com/biz/punters-pub-boston"]{Punter's Pub}}
        @dd{A bar, adjacent to Northeastern.}
        @dt{@a[href: "http://www.squealingpigboston.com/"]{The Squealing Pig}}
        @dd{A bar with food, 15 minutes walk.}
        @dt{@a[href: "http://www.urbanspoon.com/r/4/54985/restaurant/Back-Bay/Woodys-Grill-Tap-Boston"]{Woody's Grill and Tap}}
        @dd{Brick oven pizza, 15 minutes walk.}
        }
    @p{This is just a small selection;
       @a[href: '("http://maps.google.com/maps?q=restaurant&hl=en&ll=42.342147"
                  ",-71.093645&spn=0.01337,0.01929&sll=42.338636,-71.092004&ss"
                  "pn=0.006685,0.009645&near=440+Huntington+Ave,+Northeastern+"
                  "University,+Boston,+MA+02115&geocode=CUTKaBPhO49uFUwJhgId3D"
                  "jD-ylfch77IXrjiTHFj4qar3TSZw&gl=us&fll=42.338166,-71.092958"
                  "&fspn=0.013371,0.01929&z=16")]{Google}
       and
       @a[href: '("http://www.yelp.com/search?find_desc=&find_loc=440+Huntingt"
                  "on+Ave%2C+Boston%2C+MA+02115#cflt=restaurants")]{Yelp}
       have many more.}

    @h3{Where to stay}
    @p{Numerous hotels are located close to Northeastern.  The three closest
       are:}
    @dl{@dt{@a[href: "http://www.colonnadehotel.com/"]{The Colonnade Hotel}}
        @dd{A ten minute walk from RacketCon.  Ask for the Northeastern
            discounted rate.  Even with the discount, this is likely to be
            expensive.}
        @dt{@a[href: "http://www.midtownhotel.com/"]{The Midtown Hotel}}
        @dd{A ten minute walk from RacketCon.  A less expensive and less fancy
            option.}
        @dt{@a[href: "http://www.innatlongwood.com/"]{
              The Best Western Inn at Longwood}}
        @dd{A fifteen minute walk from RacketCon.}}

    @h3{Frequently Asked Questions}
    @dl{
      @dt{Do I have to be a Racket programmer to attend?}
      @dd{No.  We welcome anyone interested in or curious about Racket to come
          to RacketCon and learn about why we're all so excited about Racket.}
      @dt{Will there be a registration fee?}
      @dd{No.  RacketCon is free to everyone who wants to attend.  We
               ask that you register ahead of time, so that we can
               plan appropriately.}
      @dt{How do I register?}
      @dd{By sending an email to @|org| with your name and
             affiliation.  Please let us know if you have an dietary
             restrictions, and if you'll be bringing others.}
      @dt{Will the conference provide breakfast and lunch?}
      @dd{Yes!  We will provide complimentary breakfast snacks and lunch on
          both days to registered attendees.}
      @dt{Will the talks be recorded?}
      @dd{Thanks to the generous support of Jeff Dlouhy and the
          @a[href: "http://acm.ccs.neu.edu/"]{
            Northeastern University ACM Student Chapter}, we will
          be video-taping all talks at RacketCon, and putting them on the web.}
      @dt{Who is in charge of RacketCon?}
      @dd{RacketCon is organized by
          @a[href: "http://www.ccs.neu.edu/home/samth"]{Sam Tobin-Hochstadt}
          and the rest of the Racket development team.}
      }

    @h3{Sponsors}
    @p{Support for RacketCon is generously provided by:}
    @div[align: 'center]{
      @sponsor["Northeastern University" "neu.edu" neu]
      @br
      @sponsor["NUACM" "acm.ccs.neu.edu" nuacm]
      @br
      @sponsor["Mozilla" "mozilla.org" (copyfile #:site con-site mozilla-png)]
    }

  }})
