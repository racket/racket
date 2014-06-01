#lang plt-web

(require plt-web/style
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
       @p{Confirmed speakers:}
       @ul{@li{Claire Alvis}
           @li{Matthew Butterick}
           @li{Stephen Chang}
           @li{John Clements}
           @li{Matthew Flatt}
           @li{Tony Garnock-Jones}
           @li{Greg Hendershott}
           @li{Jay McCarthy}
           @li{Brian Mastenbrook}
           @li{Daniel Prager}
           @li{Neil Toronto}
           @li{David Vanderson}}}

    @columns[1 #:center? #f #:row? #f]{ }

    @columns[5 #:center? #f #:row? #f]{

      @p*{@b{RacketCon} is a public meeting for everyone interested in Racket:
          developers, contributors, programmers, educators, and by-standers.  It
          is an opportunity for all members of the community to come together to
          share plans, ideas, and enthusiasm.  RacketCon will enable the entire
          Racket community to mingle: to update each other, to exchange ideas, to
          collaborate, and to help shape the future of Racket.}

      @p{@nbsp}

      @p{Previous years:}
      @ul{@li{@2013:index}
          @li{@2012:index}
          @li{@2011:index}}}}})
