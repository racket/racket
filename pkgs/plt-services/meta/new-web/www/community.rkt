#lang plt-web

(require plt-web/style
         "resources.rkt" "people.rkt" "irc.rkt"
         "../minis/lists.rkt" "../minis/bugs.rkt"
         "../stubs/blog.rkt" "../stubs/git.rkt"
         (prefix-in pre: "../minis/pre.rkt")
         (only-in "../rcon/resources.rkt" rcon))

(provide thanks)
(define thanks
      @text{
        Thanks to @a[href: "http://www.nsf.gov/"]{the NSF},
        @a[href: "http://www.darpa.mil/"]{DARPA},
        the
        @a[href: "http://www.ed.gov/FIPSE/"]{
          Fund for the Improvement of Postsecondary Education (FIPSE)}
        at the @a[href: "http://www.ed.gov/"]{US Department of Education},
        the @a[href: '("http://www.exxonmobil.com/Corporate/"
                       "community_foundation.aspx")]{Exxon Foundation},
        CORD, partners of the Academy of Information Technology,
        @a[href: "http://microsoft.com"]{Microsoft},
        @a[href: "http://mozilla.org"]{Mozilla},
        and @a[href: "http://google.com"]{Google}
        for their generous support over the years.})

(provide community)
(define community
  @page[#:site www-site
        #:window-title "Racket Community Resources" #:part-of 'community
        #:description
        @'{Racket-related community resources: mailing lists, IRC channel, @;
           resources for developers, RacketCon, blog, and more.}]{
    @columns[12 #:center? #t #:row? #t]{
    @mailing-lists-quick
    @irc-quick
    @parlist[@strong{RacketCon}
             @text{@rcon[] is our annual conference.}]
    @parlist[@strong{PLT Design Inc.}
      @text{@blog — announcements, helpful hints, and thoughtful rants.}
      @text{@a[href: "http://twitter.com/#!/racketlang"]{Twitter}
            — random Racket bits.}
      @text{@people — the people behind Racket.}]
    @parlist[@strong{Wiki}
      @text{@-wiki[#f]{Wiki pages}, including:}
      @text{@-wiki{Intro Projects} — Find a project to contribute,}
      @text{@-wiki{Videos} — Watch tutorials, interviews, and more.}]
    @parlist[@strong{Development}
      @text{@git (also available on
            @a[href: "http://github.com/plt/racket/"]{GitHub}).}
      @text{@pre:installers{Snapshot} — daily builds.}
      @text{@bug-reports — create and query existing reports.}]
    @parlist[@strong{Support}
      @thanks]}})
