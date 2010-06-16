#lang at-exp s-exp "shared.rkt"

(require "people.rkt" "irc.rkt"
         "../minis/lists.rkt"
         "../stubs/blog.rkt" "../stubs/git.rkt" "../stubs/pre.rkt")

(provide community)
(define community
  @page[#:part-of 'community]{
    @mailing-lists-quick
    @parlist[@strong{Discussion Channels}
      @text{@irc-chat{Chat on IRC} in the @TT{@big{@strong{#racket}}} channel
        on @a[href: "http://freenode.net"]{@tt{freenode.net}}
        @mdash an informal discussion channel for all things related to Racket.
        @irc-logs{Browse the logs}.}]
    @parlist[@strong{Resources for Learning}
      (apply parlist @text{Documentation for getting started:} intros)
      @text{@-cookbook @mdash useful recipes, many of which apply to Racket.}
      @text{@-htdp @mdash a textbook for introductory programming, but also
        worthwhile for experience programmers who are new to @|ldquo|functional
        programming.@|rdquo|}
      @text{@-plai @mdash a textbook on programming languages.}
      @text{@-teachscheme @mdash a workshop to train teachers using @-htdp in
        the classroom.}]
    @parlist[@strong{PLT Scheme Inc.}
      @text{@blog @mdash announcements, helpful hints, and thoughtful rants.}
      @text{@people @mdash the people behind Racket.}]
    @parlist[@strong{Development}
      @text{@git (also available on
            @a[href: "http://github.com/plt/racket/"]{GitHub})}
      @text{@pre-installers and @|pre-root|.}
      ;;TODO: proper reference
      @a[href: "http://download.racket-lang.org/chronology/"]{
        Release Announcements}
      ]})
