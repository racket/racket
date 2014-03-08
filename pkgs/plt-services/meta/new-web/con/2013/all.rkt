#lang plt-web

(require plt-web/style
         racket/runtime-path
         "../resources.rkt"
         "../utils.rkt"
         (only-in "../2011/all.rkt" neu nuacm))

(define (register . e) (apply a href: "http://bit.ly/racketconsignup" e))

(define-runtime-path nbcu "nbc-universal.png")

(provide index)
(define index
  @page[#:site con-site
        #:file "2013/index.html"
        #:link-title "RacketCon 2013" #:title "RacketCon 2013"
        #:extra-headers style-header]{
   @columns[10 #:center? #t #:row? #t]{
    @h2{RacketCon: 29 September 2013}
    @p*{@b{RacketCon} is a public meeting for everyone interested in Racket:
        developers, contributors, programmers, educators, and by-standers.  It
        is an opportunity for all members of the community to come together to
        share plans, ideas, and enthusiasm.  RacketCon will enable the entire
        Racket community to mingle: to update each other, to exchange ideas, to
        collaborate, and to help shape the future of Racket.
        @~
        RacketCon is hosted by the
        @a[href: "http://www.ccs.neu.edu/"]{
          College of Computer and Information Science}
        of @a[href: "http://www.neu.edu/"]{Northeastern University}, in Boston,
        Massachusetts.  See @a[href: "#getting"]{Getting to RacketCon} below.
        @~
        Register for the event
        @a[href: "http://bit.ly/racketconsignup2013"]{here}.
        @~
        We will also run a Hackathon on Saturday (28th) in the same room as
        RacketCon from 10AM-5PM. For details, see the
        @a[href: "https://github.com/plt/racket/wiki/RacketCon-Hackathon-2013"]{
         wiki page}.
        }

    @h3{Schedule}

    @sched[
      @slot["9:30-9:55" #f]{Coffee}
      @slot["9:55-10:00" #f]{Welcome}
      @slot["10:00-11:00" mebassett
            #:video (yt "37owCjWnkK0")]{
        Racket in the Film Industry}
      @slot["11:00-11:15" #f]{Break}
      @slot["11:15-12:15" danl
            #:video (yt "oSmqbnhHp1c")
            #:slides (F "danl-slides.pdf")]{
        Racket on the Playstation 3? It's Not What you Think!}
      @slot["12:15-13:15" #f]{Lunch}
      @slot["13:15-14:05" mflatt
            #:video (yt "Z9OYc1YYLT4")
            #:slides (F "mflatt-slides.pdf")]{
        A Dinosaur's Thoughts on Programming Language Evolution}
      @slot["14:05-14:20" #f]{Break}
      @slot["14:20-14:50" jay
            #:video (yt "jnPf6S0_6Xw")
            #:slides (F "jay-slides.pdf")]{
        The Racket Package System, or Planet 5.0 and beyond}
      @slot["14:50-15:00" gregh
            #:video (yt "5q-NZNGV0sY")]{
        Frog: a static blog generator using Racket, Pygments, and Bootstrap}
      @slot["15:00-15:20" ntoronto
            #:video (yt "HmtgHVwja4k")
            #:slides (F "ntoronto-slides.pdf")]{
        Debugging Floating-Point Math in Racket}
      @slot["15:20-15:35" #f]{Break}
      @slot["15:35-15:45" mbutterick
            #:video (yt "20GGVNBykaw")]{
        The World's Most Dangerous Racket Programmer}
      @slot["15:45-15:55" tonyg
            #:video (yt "N7W3O4D8VRo")
            #:slides (F "tonyg-slides.pdf")]{
        Marketplace: Layered Pub/Sub Networks in Racket}
      @slot["15:55-16:05" kasai
            #:video (yt "bC8aS3vwO8g")
            #:slides (F "asai-slides.pdf")]{
        Introductory PL Course for non-CS Major Students in Ochanomizu University}
      @slot["16:05-16:15" cce
            #:video (yt "QfR3WhPi93g")]{
        Generic Sets for Racket}
      @slot["16:15-16:25" etanter
            #:video (yt "b0if9pPi2_M")
            #:slides (F "etanter-slides.pdf")]{
        #lang play}
      @slot["16:25-16:35" maxnew
            #:video (yt "bXhCGZyi99w")]{
        Every Program in Your Redex Model, in Order}
      @slot["16:35-16:50" #f]{Break}
      @slot["16:50-17:00" calvis
            #:video (yt "unSvrdi8ozQ")]{
        The Reasoned Racketeer}
      @slot["17:00-17:10" stamourv
            #:video (yt "D7uPm3J-o6g")]{
        Contracts on a Budget}
      @slot["17:10-17:20" jpolitz
            #:video (yt "qXhhTPRrV_A")]{
        #lang pyret and Captain Teach}
      @slot["17:20-17:30" chrdimo
            #:video (yt "jeKd2v1Uu4Q")]{
        Option Contracts}
      @slot["17:30-17:40" jswaine
            #:video (yt "osoMIIrmHzI")]{
        Automatic Complexity Analysis}
      @slot["17:40-17:50" bfetscher
            #:video (yt "0ueJJ1nD4p4")]{
        Random Test Case Generation with Redex}
    ]

    @; During the day, the following can be used to provide links to
    @;   google+ hangouts and embedded youtube casts on the con page.
    @;   Also good to extend the blurb in "../news-flash.rkt"
    @; @h1{@a[href: "https://plus.google.com/hangouts/_/c75f10c2747c4b71a0268c21624186b661cc596f"]{Live}
    @;     @a[href: "https://plus.google.com/hangouts/_/7450ee29c4ce223e188d08833505139346d6fd51"]{broadcast}}
    @; @table{@tr{
    @; @td{@iframe[width: "360" height: "290" frameborder: "0" allowfullscreen: #t
    @;             src: "http://www.youtube.com/embed/Gomcqdi6kKE"]}
    @; @td{@iframe[width: "360" height: "290" frameborder: "0" allowfullscreen: #t
    @;             src: "http://www.youtube.com/embed/p-bDXYNVz08"]}
    @; }}
    @h3{Google Hangout Videos}
    @p{The talks were captured in two Google Hangouts, linked below.
       Higher quality recordings are linked next to each of the
       talk titles in the schedule.
       @ol{@li{Stream 1:
               @a[href: (yt "Gomcqdi6kKE")]{Part 1}
               @a[href: (yt "mHVYG0L0L8g")]{Part 2}}
           @li{Stream 2:
               @a[href: (yt "p-bDXYNVz08")]{Part 1}
               @a[href: (yt "eZeg0qsOFq0")]{Part 2}
               @a[href: (yt "OAieiBrZmh8")]{Part 3}}}}

    @h3{Keynote Speakers}

    @h4{Dan Liebgold (@a[href: "http://www.naughtydog.com/"]{Naughty Dog})}
    @h5{Racket on the Playstation 3? It's Not What you Think!}
    @h5{Abstract}
    @p{In this talk I will give a brief overview of DC, a custom data scripting
       system developed in Racket at Naughty Dog. It has been an essential tool
       for us to develop games for Playstation 3.}
    @p{When developing our codebase for the Playstation 3 we decided we needed a
       comprehensive scripting system to enable the creation of significant
       amounts of customized data typically necessary to develop a Naughty Dog
       game. Using C++ in conjunction with off the shelf tools like Maya,
       Photoshop, and even internal custom game layout tools usually leaves many
       large gaps in our ability to piece together the game design we envision.}
    @p{As a result we selected Racket (then MzScheme) as a platform upon which to
       develop a system that provided the abilities we needed. In our Racket based
       system (DC), we develop a multitude of domain specific languages, a
       powerful gameplay scripting system, and systems for implementing animation,
       effects, and sound.}
    @p{My talk will give a brief overview of our experience at Naughty Dog
       building this system on top of Racket. I'll cover the nuts and bolts of how
       our system works. I'll cover the challenges of C++ programmers utilizing
       Racket while under the deadlines of retail software development. I’ll talk
       about the many opportunities this approach afforded us, some of which we
       were able to capitalize on to great success, others which lay tantalizingly
       just out of reach. I’ll talk about the culture clash of getting C++
       programmers, technical game designers, and non-technical artists to be
       productive in a world of S-expressions and syntax transformations. And I’ll
       give an overview of some metrics of the impressive contributions our system
       was able to make to the development of "Uncharted" series of games and "The
       Last of Us" for the Playstation 3.}
    @h5{About the Speaker}
    @p{Dan Liebgold has programmed games in one form or another for most of his
       life, starting on an Apple II in 4th grade. He has worked as a
       professional game programmer for 17 years, 11 of them at Naughty Dog, Inc.
       Some of the games he's contributed to are: Starcraft, Sacrifice, the Jak &
       Daxter series and the Uncharted series.}

    @h4{Matthew Eric Bassett (@a[href: "http://www.nbcuni.com/international/"]{NBCUniversal International})}
    @h5{Racket in the Film Industry}

    @a[name: "getting"]@h3{Getting to RacketCon}

    @p{RacketCon will be held in @b{West Village H, Room 108}, at
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
               ask that you register ahead of time, so that we can
               plan appropriately.}
      @dt{Will the conference provide lunch?}
      @dd{Yes!  We will provide complimentary lunch
          to registered attendees.}
      @dt{Will the talks be recorded?}
      @dd{We hope to record and post the talks given at RacketCon.  If you are
          interested in assisting with this, please let the organizers know.}
      @dt{Who is in charge of RacketCon?}
      @dd{RacketCon is organized by
          @a[href: "http://www.ccs.neu.edu/home/asumu"]{Asumu Takikawa}
          and the rest of the Racket development team.}
      }

    @h3{Sponsors}
    @p{Support for RacketCon is generously provided by:}
    @div[align: 'center]{
      @sponsor["Northeastern University" "neu.edu" neu]
      @sponsor["NBC Universal" "www.nbcuni.com" (copyfile #:site con-site nbcu)]
      @sponsor["NU ACM" "acm.ccs.neu.edu" nuacm]
    }

  }})
