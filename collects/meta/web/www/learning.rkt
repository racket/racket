#lang at-exp s-exp "shared.rkt"

(define brown-pubs
  @a[href: "http://www.cs.brown.edu/~sk/Publications/Papers/"]{
    Brown PLT Publications})
(define nwu-pubs
  @a[href: "http://www.eecs.northwestern.edu/~robby/pubs/"]{
    Northwestern PLT Publications})
(define neu-pubs
  @a[href: "http://www.ccs.neu.edu/scheme/pubs/"]{
    Northeastern PLT Publications})
(define utah-pubs
  @a[href: "http://www.cs.utah.edu/plt/publications/"]{Utah PLT Publications})
;; TODO: add calpoly & byu?

(provide learning)
(define learning
  @page{
    @parlist[@strong{Resources for Learning}
      (apply parlist @text{Documentation for getting started:} intros)
      @text{@-cookbook — useful recipes, many of which apply to Racket.}
      @text{@-htdp — a textbook for introductory programming, but also
        worthwhile for experience programmers who are new to “functional
        programming.”}
      @text{@-plai — a textbook on programming languages.}]
    @parlist[
      @strong{Outreach}
      @text{@-teachscheme — a workshop to train teachers using @-htdp in the
        classroom.}
      @text{@-bootstrap — a curriculum for middle-school students.}]
    @parlist[@strong{Publications}
             techreports brown-pubs nwu-pubs neu-pubs utah-pubs]
    @parlist[@strong{Graduate Study}
             @text{We welcome applications from students interested in
                   @|graduate-study|.}]})

(define graduate-study
  @page[#:file "common-plt-app.html" #:part-of learning]{
    @(define (box-style border-width color)
       @list{border: @|border-width|px solid black; padding: 5px; @;
             background: @|color|@";"})
    @h1{Graduate Study with PLT}
    @p{An open letter to graduate applicants:}
    @div[style: (box-style 3 "#ddd")]{
    @p*{
      Dear Prospective Graduate Student,
      @|br br|
      Thank you for your interest in working with PLT.  We get several
      inquiries every year from students interested in working with one or more
      of us.  We're flattered and, of course, interested in your applications.
      Because you are more interested in PLT than in our specific institutions,
      we have created the following common application form.  By filling it in
      once, you can automatically apply to all current PLT institutions.
    @~ @|style: (box-style 1 "#bbb")|@;
      Yes, we know you don't like Boston/Chicago/Providence/Provo/Salt Lake
      City/San Luis Obispo/Worcester (circle those applicable).  But we like
      them, or we wouldn't be living there.  Think about the message you're
      sending by rejecting our choices.  Moreover, we think very highly of our
      colleagues—more highly than we think of your judgment in this matter—so
      for your own good, we're going to forward your application to them
      anyway.
    @~ How many years have you programmed in Scheme?
    @~ How many years have you programmed in PLT Scheme?
    @~ If the two numbers above are not identical, justify.
    @~ How many PLT Scheme Web applications have you written?
    @~ What problems did you find with the PLT Scheme Web server in the
       process?  Please list bug report numbers.
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
    @p[align: 'right]{
      —@;
      @a[href: "http://www.ccs.neu.edu/home/matthias/"]{Matthias},
      @a[href: "http://www.eecs.northwestern.edu/~robby/"]{Robby},
      @a[href: "http://www.cs.wpi.edu/~kfisler/"]{Kathi},
      @a[href: "http://www.cs.utah.edu/~mflatt/"]{Matthew},
      @a[href: "http://www.cs.brown.edu/~sk/"]{Shriram}}})

(require "techreports.rkt")
(define techreports
  @page[#:file "techreports/" #:part-of learning
        #:title "Technical Reports"]{
    @p{PLT publishes technical reports about some of its tools and libraries so
       that scholars who wish to give proper credit to some of our innovations
       have a definite citation.  Each entry below provides the full pdf and a
       bibtex entry; some of the bibtex entries provide additional citations to
       published papers.}

    @make-bib-table{}})

;; redirection page for the previous name of this page
(define outreach+research
  @page[#:part-of learning
        #:title "Outreach & Research"
        #:link-title @list{Outreach@|nbsp|&@|nbsp|Research}
        #:extra-headers
        @meta[http-equiv: "refresh" content: "0;url=learning.html"]]{
    Moved.})
