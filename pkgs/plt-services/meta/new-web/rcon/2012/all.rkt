#lang plt-web

(require plt-web/style
         "../resources.rkt"
         "../utils.rkt"
         (only-in "../2011/all.rkt" neu))

(define (register . e) (apply a href: "http://bit.ly/racketconsignup" e))

(provide index)
(define index
  @page[#:site con-site
        #:file "2012/index.html"
        #:link-title "RacketCon 2012" #:title "RacketCon 2012"
        #:extra-headers style-header]{
   @columns[10 #:center? #t #:row? #t]{
    @h2{RacketCon: 13 October 2012}
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
        @;@~
        @;To register for RacketCon, please fill out @register{this form}.
        @~
        We will also run a Hackathon on Sunday in the same room as
        RacketCon. For details, see the
        @a[href: "https://github.com/plt/racket/wiki/RacketCon-Hackathon-2012"]{
          wiki page}.
        }

    @h3{Schedule}

    @sched[
      @slot["9:30-10:00" #f]{Coffee and Welcome}
      @slot["10:00-12:00" mflatt
            #:code "http://www.cs.utah.edu/plt/scratchy/"
            #:video (yt "y1rOWZkALto")]{Tutorial: Building Languages in Racket}
      @slot["12:00-1:30" #f]{Lunch}
      @slot["1:30-1:40" ryanc #:video (yt "yv_PcyQmrFs")]{
        Better Syntax Templates for @tt{syntax-parse}}
      @slot["1:40-1:50" morazanm #:video (yt "vZy6w1G7hRI")]{
        Distributed Programming for First-Year Students}
      @slot["1:50-2:00" tewk
            #:slides (F "tewk-slides.pdf")
            #:video (yt "C88L4u306Ro")]{Distributed Places}
      @slot["2:00-2:10" dyoo
            #:slides (F "dyoo-slides-1.pdf")
            #:video (yt "A5v4gA1DY8I")]{DrBlocket}
      @slot["2:10-2:20" stamourv
            #:code (gh "3974136")
            #:video (yt "W9BYTrDUPtU")]{Optimization Coach}
      @slot["2:20-2:30" asumu
            #:video (yt "MYtm9YG4tMM")
            #:code (gh "3885002")
            #:slides (F "asumu-slides.pdf")]{Generics}
      @slot["2:20-3:00" #f]{Break}
      @slot["3:00-3:10" dyoo
            #:slides (F "dyoo-slides-2.pdf")
            #:video (yt "hh3rkRTOQuA")]{
        WeScheme and the Amazing Technicolor Structures}
      @slot["3:10-3:20" "James Swaine"
            #:code (gh "3973451")
            #:video (yt "28uMzVRMWSs")]{
        Profiling Parallel Racket with the Future Visualizer}
      @slot["3:20-3:30" jpolitz
            #:slides (F "politz-slides.pdf")
            #:video (yt "52mUh5Or9Jk")]{
        Crowdsourced Conformance Testing via Remote Sandboxing
        (or Avoiding Grading for Fun and Profit)}
      @slot["3:30-3:40" rafkind
            #:slides (F "rafkind-slides.pdf")
            #:video (yt "xV4e0YYv6zA")]{Honu: Macros for infix syntax}
      @slot["3:40-3:50" cce
            #:slides (F "cce-slides.pdf")
            #:video (yt "8u1wV7WRwuk")]{
        Racket + ACL2 + ML = Dracula}
      @slot["3:50-4:00" @name["http://about.me/cky"]{Chris Jester-Young}
            #:video (yt "jW-XIzMOFw8")
            #:slides (F "cky-slides.pdf")]{Racket + JVM}
      @slot["4:00-4:15" #f]{Break}
      @slot["4:15-6:15" samth
            #:code "http://github.com/samth/tr-tutorial/"
            #:video (yt "w-fVHOxeEpM")]{Tutorial: Typed Racket}
    ]

    @a[name: "getting"]@h3{Getting to RacketCon}

    @p{RacketCon will be held in @b{West Village H, Room 110}, at
       Northeastern University.  West Village H is located
       at 440 Huntington Ave, Boston Massachusetts, and is building
       @a[href: "http://www.northeastern.edu/campusmap/map/qad4.html"]{#23H} on
       the @a[href: "http://www.northeastern.edu/campusmap/map"]{campus map}.
       See also the building on
       @a[href: "http://goo.gl/maps/On05h"]{Google Maps}}

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
        @dt{@a[href: '("http://maps.google.com/maps/place?"
                       "q=pizzeria+uno+boston&cid=11922407303454006467")]{
              Pizzeria Uno}}
        @dd{Deep dish pizza, 5 minutes walk.}
        @dt{@a[href: "http://www.yelp.com/biz/punters-pub-boston"]{
              Punter's Pub}}
        @dd{A bar, adjacent to Northeastern.}
        @dt{@a[href: "http://www.squealingpigboston.com/"]{The Squealing Pig}}
        @dd{A bar with food, 15 minutes walk.}
        @dt{@a[href: '("http://www.urbanspoon.com/r/4/54985/restaurant/"
                       "Back-Bay/Woodys-Grill-Tap-Boston")]{
              Woody's Grill and Tap}}
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
               ask that you @register{register} ahead of time, so that we can
               plan appropriately.}
      @dt{Will the conference provide lunch?}
      @dd{Yes!  We will provide complimentary lunch
          to registered attendees.}
      @dt{Will the talks be recorded?}
      @dd{We hope to record and post the talks given at RacketCon.  If you are
          interested in assisting with this, please let the organizers know.}
      @dt{Who is in charge of RacketCon?}
      @dd{RacketCon is organized by
          @a[href: "http://www.ccs.neu.edu/home/samth"]{Sam Tobin-Hochstadt}
          and the rest of the Racket development team.}
      }

    @h3{Sponsors}
    @p{Support for RacketCon is generously provided by:}
    @div[align: 'center]{
      @sponsor["Northeastern University" "neu.edu" neu]
    }

  }})
