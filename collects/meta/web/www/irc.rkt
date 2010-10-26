#lang meta/web

(require "shared.rkt")

(provide irc-quick)

(define webchat-link
  "http://webchat.freenode.net?channels=racket&uio=OT10cnVlJjExPTIzNg6b")

(define irc-chat
  @page[#:title "IRC" #:part-of 'community]{
    @iframe[src: webchat-link width: "100%" height: "400"]})

(define irc-logs-symlink
  (symlink "/home/scheme/irc-logs/racket/" "irc-logs"))
(define (irc-logs text) @a[href: (list irc-logs-symlink "/")]{@text})

(define (irc-quick)
  @parlist[@strong{Discussion Channel}
    @text{@irc-chat{Chat on IRC} in the @TT{@big{@strong{#racket}}} channel on
      @a[href: "http://freenode.net"]{@tt{freenode.net}} — an informal
      discussion channel for all things related to Racket.
      @irc-logs{Browse the logs}.}])
