#lang at-exp s-exp "shared.rkt"

(provide irc-chat irc-logs)

(define webchat-link
  "http://webchat.freenode.net?channels=racket&uio=OT10cnVlJjExPTIzNg6b")

(define irc-chat
  @page[#:title "IRC" #:part-of 'community]{
    @iframe[src: webchat-link width: "100%" height: "400"]})

(define irc-logs-symlink (symlink "/home/scheme/irc-logs/racket/"))
(define (irc-logs text) @a[href: irc-logs-symlink]{@text})
