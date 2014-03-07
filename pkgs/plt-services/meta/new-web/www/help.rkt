#lang plt-web

(require plt-web/style "resources.rkt" "community.rkt")

(provide help)
(define help
  @page[#:site www-site #:link-title "Need Help?"]{
   @columns[12 #:center? #t #:row? #t]{
   @parlist[@strong{Don't Panic!}
            @text{Racket has a variety of resources designed to help you
                  with any problems you may have.}]
   @parlist[@strong{Help Desk}
            @text{Your first stop should always be with the help system that's
                  built into Racket and available from DrRacket's help menu
                  @strong{or by pressing F1 with the cursor on a search term}.
                  This documentation is customized for your installation, and
                  may include documentation for optional packages you've
                  installed.  As a second line of defense, the documentation
                  for the core of the most recent version of Racket is
                  available @-docs{from this web site}.}
            @text{Not sure what to search for?  The documentation includes a
                  @guide{guide} (also located in your local copy of the
                  documentation) that provides a narrative introduction to many
                  of Racket's features.}]
   @parlist[@strong{Learning how to Program}
            @text{Try going through @|-htdp|.}]
   @parlist[@strong{Searching the Web}
            @text{Your favorite search engine may well provide answers for many
                  of your questions, in particular, try to search through
                  Racket questions on
                  @a[href: "http://stackoverflow.com/questions/tagged/racket"]{
                    stackoverflow}.}]
   @parlist[@strong{The Mailing List}
            @text{The @tt|{users@racket-lang.org}| mailing list is a great
                  source for answers to questions when the above resources
                  don't pan out@";" sign up for it in the @community area of
                  the website.}]
   @br
   @text{Thanks for using Racket!}}})
