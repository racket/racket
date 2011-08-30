#lang meta/web

(require "resources.rkt")

(define webchat-link
  "http://webchat.freenode.net?channels=racket&uio=OT10cnVlJjExPTIzNg6b")

(define irc-chat
  @page[#:title "IRC" #:part-of 'community]{
    @iframe[src: webchat-link width: "100%" height: "400"]})

(define irc-logs
  (let ()
    @plain[#:file "irc-logs/.htaccess" #:referrer values]{
      RewriteEngine on
      RewriteRule ^(.*)$ http://pre.racket-lang.org@;
         /irc-logs/@||racket/@|"$1"| [P]
    }
    (lambda (text) @a[href: "irc-logs/"]{@text})))

(provide irc-quick)
(define (irc-quick)
  @parlist[@strong{Discussion Channel}
    @text{@irc-chat{Chat on IRC} in the @TT{@big{@strong{#racket}}} channel on
      @a[href: "http://freenode.net"]{@tt{freenode.net}} — an informal
      discussion channel for all things related to Racket.
      (@irc-logs{Browse the logs}.)}])
