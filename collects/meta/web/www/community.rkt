#lang at-exp s-exp "shared.rkt"

(require "../stubs/blog.rkt" "../stubs/git.rkt" "../stubs/pre.rkt"
         "people.rkt" "irc.rkt")

(define (TT . xs)
  @tt[style: "background-color: #dde;"]{@xs})

(define (maillist-email name)
  @TT{@big{@strong{@name}}@"@"racket-lang.org})
(define (maillist-url name)
  (define url "http://lists.racket-lang.org/")
  @text{@a[href: `(,url ,name "/")]{Subscribe}
        or @a[href: `(,url ,name "/archive/")]{browse}})
;; TODO: Need to finish the setup for gmane and google-groups
;; (define (gmane name)
;;   @a[href: `("http://dir.gmane.org/gmane.lisp.scheme." ,name)]{Gmane})
;; (define google-groups
;;   @a[href: "http://groups.google.com/group/plt-scheme"]{Google Groups})

(provide community)
(define community
  (page
    (parlist @strong{Mailing Lists}
      @text{@maillist-email{users} @mdash a discussion list for all things
        related to Racket.  Ask your questions here!
        (@maillist-url{users}.)
        @; These are not set up yet
        @; also via @gmane{racket} and @|google-groups|).
        }
      @text{@maillist-email{announce} @mdash a low-volume, moderated list
        for announcements, only.  (@maillist-url{announce}.)}
      @text{@maillist-email{dev} @mdash a mailing list for Racket development,
        for the people who want to see how the sausages are made and help make
        them.  (@maillist-url{dev}.)
        @; @";" also on @gmane{plt.dev}.)
        })
    (parlist @strong{Discussion Channels}
      @text{@irc-chat{Chat on IRC} in the @TT{@big{@strong{#racket}}} channel
        on @a[href: "http://freenode.net"]{@tt{freenode.net}}
        @mdash an informal discussion channel for all things related to Racket.
        @irc-logs{Browse the logs}.})
    (parlist @strong{Resources for Learning}
      (apply parlist @text{Documentation for getting started:} intros)
      @text{@-cookbook @mdash useful recipes, many of which apply to Racket.}
      @text{@-htdp @mdash a textbook for introductory programming, but also
        worthwhile for experience programmers who are new to @|ldquo|functional
        programming.@|rdquo|}
      @text{@-plai @mdash a textbook on programming languages.}
      @text{@-teachscheme @mdash a workshop to train teachers using @-htdp in
        the classroom.})
    (parlist @strong{PLT Scheme Inc.}
      @text{@blog @mdash announcements, helpful hints, and thoughtful rants.}
      @text{@people @mdash the people behind Racket.}
      )
    (parlist @strong{Development}
      @text{@git (also available on
            @a[href: "http://github.com/plt/racket/"]{GitHub})}
      @text{@pre-installers and @|pre-root|.}
      ;;TODO: proper reference
      @a[href: "http://download.racket-lang.org/chronology/"]{
        Release Announcements}
      )))
