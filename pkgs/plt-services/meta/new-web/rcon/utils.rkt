#lang at-exp racket/base

(require "resources.rkt" racket/match scribble/html)

(provide (all-defined-out))

(define style-header
  @style/inline{
    a.doclink {
      text-decoration: none;
      color: black;
    }
    dt {
      font-weight: bold;
    }
    dd {
      margin-bottom: 1ex;
    }})

(define (name url . rest)
  (apply a class: 'name href: url rest))

(define mflatt @name["http://www.cs.utah.edu/~mflatt"]{Matthew Flatt})
(define rafkind @name["http://www.cs.utah.edu/~rafkind"]{Jon Rafkind})
(define tewk @name["http://kevintew.com/"]{Kevin Tew})
(define ryanc @name["http://www.cs.utah.edu/~ryan"]{Ryan Culpepper})
(define matthias @name["http://www.ccs.neu.edu/~matthias"]{Matthias Felleisen})
(define dvh @name["http://www.ccs.neu.edu/~dvanhorn"]{David Van Horn})
(define asumu @name["http://www.ccs.neu.edu/~asumu"]{Asumu Takikawa})
(define stamourv @name["http://www.ccs.neu.edu/~stamourv"]{Vincent St-Amour})
(define cce @name["http://www.ccs.neu.edu/~cce"]{Carl Eastlund})
(define samth @name["http://www.ccs.neu.edu/~samth"]{Sam Tobin-Hochstadt})
(define sk @name["http://www.cs.brown.edu/~sk"]{Shriram Krishnamurthi})
(define jpolitz @name["http://jpolitz.github.com/"]{Joe Gibbs Politz})
(define jay @name["http://jeapostrophe.github.io"]{Jay McCarthy})
(define rbf @name["http://www.eecs.northwestern.edu/~robby"]{Robby Findler})
(define jbc @name["http://www.brinckerhoff.org/clements/"]{John Clements})
(define dyoo @name["http://hashcollision.org/"]{Danny Yoo})
(define pr @name["http://www.cs.uwaterloo.ca/~plragde/"]{Prabhakar Ragde})
(define guillaume @name["http://gmarceau.qc.ca/"]{Guillaume Marceau})
(define moskol @name["http://www.ric.edu/faculty/amoskol/"]{Ann Moskol})
(define morazanm @name["http://www.shu.edu/academics/profiles/profile-details.cfm?customel_datapageid_148360=220477"]{Marco Morazan})
(define gregh @name["http://www.greghendershott.com/"]{Greg Hendershott})
(define mbutterick @name["http://practicaltypography.com/"]{Matthew Butterick})
(define tonyg @name["http://homepages.kcbbs.gen.nz/tonyg/"]{Tony Garnock-Jones})
(define calvis @name["https://github.com/calvis"]{Claire Alvis})
(define bfetscher @name["https://github.com/bfetscher"]{Burke Fetscher})
(define etanter @name["http://pleiad.cl/people/etanter"]{Éric Tanter})
(define chrdimo @name["http://people.seas.harvard.edu/~chrdimo/"]{Christos Dimoulas})
(define maxnew @name["https://github.com/maxsnew"]{Max New})
(define kasai @name["http://www.is.ocha.ac.jp/~asai/"]{Kenichi Asai})
(define danl @name["https://twitter.com/danl2620"]{Dan Liebgold})
(define mebassett @name["http://mebassett.gegn.net/"]{Matthew Eric Bassett})
(define ntoronto @name["http://students.cs.byu.edu/~ntoronto/"]{Neil Toronto})
(define jswaine @name["https://github.com/Zoetermeer"]{James Swaine})

(define org @a[href: "mailto:racketcon@racket-lang.org"]{the organizers})

(struct slot* (time speaker slides code video title))
(define (slot time speaker
              #:slides [slides #f]
              #:code [code #f]
              #:video [video #f]
              . title)
  (slot* time speaker slides code video title))

(define (yt id) @list{http://www.youtube.com/watch?v=@id})

(define (gh id) @list{https://gist.github.com/@id})

(define (sponsor name site image)
  @a[title: name href: @list{http://@|site|/}]{
    @img[style: "margin: 20px; border: 0;" src: image alt: name]})

(define (sched . slots)
  (table class: 'sched width: "100%" frame: 'hsides
         cellspacing: 0 cellpadding: "3px"
    (for/list ([sl (in-list slots)])
      (match-define (slot* t speaker slides code video title) sl)
      (define time
        ;; "figure space" for alignment
        (cons (and (regexp-match? #rx"^.:" t) (entity #x2007))
              (add-between (regexp-split #rx"-" t) ndash)))
      (define slides-elem (and slides @a[href: slides]{[slides]}))
      (define code-elem   (and code @a[href: code]{[code]}))
      (define video-elem  (and video @a[href: video]{[video]}))
      @tr[valign: 'top bgcolor: (if speaker "#ffffff" "#e8e8e8")]{
        @td[width: "15%" nowrap: 'nowrap]{@time}
        @td[width: "25%"]{@speaker}
        @td[width: "60%"]{
          @title
          @span[style: "text-align: right"]{
            @slides-elem @code-elem @video-elem}}})))
(define (sched* . slots)
  (table class: 'sched width: "100%" frame: 'hsides
         cellspacing: 0 cellpadding: "3px"
    (for/list ([sl (in-list slots)])
      (match-define (slot* t speaker slides code video title) sl)
      (define time
        ;; "figure space" for alignment
        (cons (and (regexp-match? #rx"^.:" t) (entity #x2007))
              (add-between (regexp-split #rx"-" t) ndash)))
      (define slides-elem
        (and slides @a[href: slides #;(copyfile (in-here slides))]{[slides]}))
      (define code-elem
        (and code @a[href: code]{[code]}))
      (define video-elem
        (and video @a[href: video]{[video]}))

      @tr[valign: 'top bgcolor: (if speaker "#ffffff" "#e8e8e8")]{
        @td[width: "15%" nowrap: 'nowrap]{@time}
        @td[width: "25%"]{@speaker}
        @td[width: "60%"]{
          @title
          @span[style: "text-align: right"]{
            @slides-elem @code-elem @video-elem}}})))

;; like sched, but with talks grouped in sessions, and only session headers
;; have times listed
;; the interface could be prettier
(define (session-sched . slots)
  (table class: 'sched width: "100%" frame: 'hsides
         cellspacing: 0 cellpadding: "3px"
    (for/list ([sl (in-list slots)])
      (match-define (slot* t speaker slides code video title) sl)
      (define time
        ;; "figure space" for alignment
        (and t
             (cons (and (regexp-match? #rx"^.:" t) (entity #x2007))
                   (add-between (regexp-split #rx"-" t) ndash))))
      (define slides-elem (and slides @a[href: slides]{[slides]}))
      (define code-elem   (and code @a[href: code]{[code]}))
      (define video-elem  (and video @a[href: video]{[video]}))
      @tr[valign: 'top bgcolor: (if speaker "#ffffff" "#e8e8e8")]{
        @td[width: "30%"]{@(or time speaker)}
        @td[width: "70%"]{
          @title
          @span[style: "text-align: right"]{
            @slides-elem @code-elem @video-elem}}})))

;; Refer to a file that has been manually uploaded
;; (relative to its referring page):
(define (F p) p)
