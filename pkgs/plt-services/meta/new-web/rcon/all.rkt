#lang plt-web

(require plt-web/style
         racket/dict
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

(define speakers+web-pages
  '(("Claire Alvis" . "https://github.com/calvis")
    ("Matthew Butterick" . "http://practicaltypography.com/")
    ("Stephen Chang" . "http://www.ccs.neu.edu/home/stchang/")
    ("John Clements" . "http://www.brinckerhoff.org/JBCsite/index.html")
    ("Matthew Flatt" . "https://www.cs.utah.edu/~mflatt/")
    ("Tony Garnock-Jones" . "http://homepages.kcbbs.gen.nz/tonyg/")
    ("Greg Hendershott" . "http://www.greghendershott.com/")
    ("Jay McCarthy" . "http://faculty.cs.byu.edu/~jay/home/")
    ("Brian Mastenbrook" . "http://brian.mastenbrook.net/")
    ("Daniel Prager" . "https://www.youpatch.com/")
    ("Neil Toronto" . "http://students.cs.byu.edu/~ntoronto/")
    ("David Vanderson" . "https://github.com/david-vanderson/")))


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

       @p{@b{Confirmed speakers:}}
       @(apply ul
               (for/list ([(speaker web-page) (in-dict speakers+web-pages)])
                 @li{@a[href: web-page]{@speaker}}))}

    @columns[1 #:center? #f #:row? #f]{ }

    @columns[5 #:center? #f #:row? #f]{

      @p*{@b{RacketCon} is a public meeting for everyone interested in Racket:
          developers, contributors, programmers, educators, and by-standers.  It
          is an opportunity for all members of the community to come together to
          share plans, ideas, and enthusiasm.  RacketCon will enable the entire
          Racket community to mingle: to update each other, to exchange ideas, to
          collaborate, and to help shape the future of Racket.}

      @p{@nbsp}

      @p{@b{Previous years:}}
      @ul{@li{@2013:index}
          @li{@2012:index}
          @li{@2011:index}}}}})
