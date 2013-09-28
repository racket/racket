#lang meta/web

(require "resources.rkt")

(define webchat-link
  "http://webchat.freenode.net?channels=racket&uio=OT10cnVlJjExPTIzNg6b")

(define irc-chat
  @page[#:title "IRC" #:part-of 'community]{
    @iframe[src: webchat-link width: "100%" height: "400"]})

(define log-header+footer
  (lazy (regexp-split #rx"{{{BODY}}}"
                      (xml->string @page[#:id 'browse-downloads
                                         #:html-only #t
                                         #:part-of 'community
                                         "{{{BODY}}}"]))))
(define header @plain[#:file "irc-logs/dummy/HEADER.html" #:newline #f
                      (car  (force log-header+footer))])
(define footer @plain[#:file "irc-logs/dummy/README.html" #:newline #f
                      (cadr (force log-header+footer))])

(provide irc-logs)
(define irc-logs
  (let ([base "/home/scheme/irc-logs"])
    (define t (make-hash))
    (λ (type . text)
      (hash-ref! t type (λ() (a href: (symlink (format "~a/~a" base type)
                                               (format "irc-logs/~a" type))
                                text))))))
(void (irc-logs ".htaccess"))

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
