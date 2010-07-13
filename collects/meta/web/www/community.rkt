#lang at-exp s-exp "shared.rkt"

(require "people.rkt" "irc.rkt"
         "../minis/lists.rkt"
         "../stubs/blog.rkt" "../stubs/git.rkt"
         (prefix-in pre: "../stubs/pre.rkt"))

(provide community)
(define community
  @page[#:part-of 'community]{
    @mailing-lists-quick
    @irc-quick
    @parlist[@strong{PLT Scheme Inc.}
      @text{@blog — announcements, helpful hints, and thoughtful rants.}
      @text{@people — the people behind Racket.}]
    @parlist[@strong{Development}
      @text{@git (also available on
            @a[href: "http://github.com/plt/racket/"]{GitHub})}
      @text{@pre:installers and @|pre:index|.}
      ;;TODO: proper reference
      @a[href: "http://download.racket-lang.org/chronology/"]{
        Release Announcements}
      ]})
