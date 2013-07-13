#lang meta/web

(require "resources.rkt")

(define webchat-link
  "http://webchat.freenode.net?channels=racket&uio=OT10cnVlJjExPTIzNg6b")

(define irc-chat
  @page[#:title "IRC" #:part-of 'community]{
    @iframe[src: webchat-link width: "100%" height: "400"]})

(define irc-logs
  (let ()
    @plain[#:file "irc-logs/.htaccess"]{
      RewriteEngine on
      RewriteRule ^(racket(-dev)?/.*)$ http://lambda.racket-lang.org@;
         /irc-logs/@|"$1"| [P,L]
      @; For legacy links (should eventually be removed)
      RewriteRule ^(.*)$ http://lambda.racket-lang.org@;
         /irc-logs/racket/@|"$1"| [P,L]
    }
    (λ (type . text) @a[href: `("irc-logs/" ,type "/")]{@text})))

(provide irc-quick)
(define (irc-quick)
  (define (chan name) @TT{@big{@strong{#@name}}})
  @parlist[@strong{Discussion Channel}
    @text{@irc-chat{Chat on IRC} in the @chan{racket} channel on
      @a[href: "http://freenode.net"]{@tt{freenode.net}} — an informal
      discussion channel for all things related to Racket.
      (@irc-logs['racket]{Browse the logs}.)
      There is also @chan{racket-dev} (@irc-logs['racket-dev]{logs}), a channel
      for notification bots.}])
