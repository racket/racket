#lang plt-web

(require "resources.rkt" plt-web/style)

(provide irc-content
         irc-quick)

(define webchat-link
  "http://webchat.freenode.net?channels=racket&uio=OT10cnVlJjExPTIzNg6b")

(define irc-chat
  @page[#:site www-site #:title "IRC" #:part-of 'community]{
    @columns[12 #:row? #t #:center? #t]{
      @iframe[src: webchat-link width: "100%" height: "400"]}})

(define irc-content
  @list{Chat in the @tt[style: "background-color: #d8d8e8;"]{@big{@strong{#racket}}} channel on
@a[href: "http://freenode.net"]{@tt{freenode.net}}, an informal
discussion channel for all things related to Racket, or
@a[href: "https://botbot.me/freenode/racket/"]{browse the logs}.})

(define irc-quick
  @text{@parlist[@strong{IRC}
                 @irc-content]})
