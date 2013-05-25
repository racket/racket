#lang meta/web

(require "resources.rkt" "people.rkt" "techreports.rkt" "old-techreports.rkt")

(provide learning)
(define learning
  @page[#:window-title "Racket Learning" #:part-of 'learning
        #:description
        '@{Racket-related learning resources.  Introductions to Racket, @;
           the Racket Guide, Computer Science textbooks that use Racket, @;
           outreach programs, and graduate studies.}]{
    @parlist[@strong{Resources for Learning}
      (apply parlist @text{Documentation for getting started:} intros)
      @text{@-htdp — a textbook for introductory programming, but also
        worthwhile for experience programmers who are new to “functional
        programming.”}
      @text{@-plai — a textbook on programming languages.}]
    @parlist[@strong{Videos}
      @text{See the @-wiki["Videos"] page at the @|-wiki|.}]
    @parlist[
      @strong{Outreach}
      @text{@-pbd — a workshop to train teachers using @-htdp in the
        classroom.}
      @text{@-bootstrap — a curriculum for middle-school students.}]
    @(apply parlist @strong{PLT Publications}
            (cons techreports
                  (for*/list ([place (in-list all-places)]
                              [pubs (in-value (place-pubs place))]
                              #:when pubs)
                    @a[href: pubs]{@(place-name place)})))
    @parlist[@strong{Graduate Study}
             @text{We welcome applications from students interested in
                   @|graduate-study|.}]})

(define graduate-study
  @page[#:file "common-plt-app.html" #:part-of 'learning]{
    @(define (box-style border-width color)
       @list{border: @|border-width|px solid black; padding: 5px; @;
             background: @|color|@";"})
    @(define place-names
       (add-between
        (sort (map (λ (p)
                     (regexp-replace #rx", [A-Z][A-Z]$" (place-location p) ""))
                   all-places)
              string<?)
        " / "))
    @(define responsible-people
       (add-between
        (for/list ([person (sort (map (compose car place-people) all-places)
                                 string<? #:key person-name)])
          @a[href: (person-url person)]{
            @(regexp-replace #rx" .*$" (person-name person) "")})
        ", "))
    @h1{Graduate Study with PLT}
    @p{An open letter to graduate applicants:}
    @div[style: (box-style 3 "#dddddd")]{
    @p*{
      Dear Prospective Graduate Student,
      @|br br|
      Thank you for your interest in working with PLT.  We get several
      inquiries every year from students interested in working with one or more
      of us.  We're flattered and, of course, interested in your applications.
      Because you are more interested in PLT than in our specific institutions,
      we have created the following common application form.  By filling it in
      once, you can automatically apply to all current PLT institutions.
    @~ @|style: (box-style 1 "#bbbbbb")|@;
      Yes, we know you don't like @place-names (circle those applicable).  But
      we like them, or we wouldn't be living there.  Think about the message
      you're sending by rejecting our choices.  Moreover, we think very highly
      of our colleagues—more highly than we think of your judgment in this
      matter—so for your own good, we're going to forward your application to
      them anyway.
    @~ How many years have you programmed in Scheme?
    @~ How many years have you programmed in Racket?
    @~ If the two numbers above are not identical, justify.
    @~ How many Racket Web applications have you written?
    @~ What problems did you find with the Racket Web server in the process?
       Please list bug report numbers.
    @~ Which wheels did you inadvertently reinvent?
    @~ Do you prefer your calculi Classic or Featherweight?
    @~ Should types be intervals or ideals?
    @~ In your opinion, which Barendregt proof has the highest hours spent
       understanding-to-symbols ratio?
    @~ Which is your favorite combinator?
    @~ Thank you for your interest.  Don't be a cat squandering the popcorn.
    @~ @|align: 'right|
       —Shriram, Outreach Coordinator, PLT}}
    @p{Seriously, we @em{do} enjoy working with graduate students.  If you are
       inspired by the PLT project and want to drive the next generation of
       innovation, you should strongly consider graduate study with us.  We
       look forward to hearing from you.  All of us, no matter where we may
       live.}
    @p[align: 'right]{—@responsible-people}})

;; redirection page for the previous name of this page
(define outreach+research
  @page[#:part-of 'learning
        #:title "Outreach & Research"
        #:link-title @list{Outreach@|nbsp|&@|nbsp|Research}
        #:extra-headers
        @meta[http-equiv: "refresh" content: "0;url=learning.html"]]{
    Moved.})
