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

(define index
  @page[#:site con-site
        #:link-title "RacketCon" #:title "RacketCon"
        #:extra-headers style-header]{
   @columns[8 #:center? #t #:row? #t]{
    @h2{RacketCon}
    @p{@b{(fourth RacketCon)} will be held in St. Louis on September 20, 2014,
       the day after @a[href: "https://thestrangeloop.com/"]{Strange Loop}.
       Stay turned for more information!}
    @p{@b{@a[href: "https://www.eventbrite.com/e/racketcon-2014-tickets-11408046775"]{Registration}}
       is open. Tickets are 30$.}
    @p{@b{@a[href: "http://www.fogus.me/"]{Michael Fogus}} will be giving the keynote.}
    @p*{@b{RacketCon} is a public meeting for everyone interested in Racket:
        developers, contributors, programmers, educators, and by-standers.  It
        is an opportunity for all members of the community to come together to
        share plans, ideas, and enthusiasm.  RacketCon will enable the entire
        Racket community to mingle: to update each other, to exchange ideas, to
        collaborate, and to help shape the future of Racket.}
    @ul{@li{@2013:index}
        @li{@2012:index}
        @li{@2011:index}}}})
