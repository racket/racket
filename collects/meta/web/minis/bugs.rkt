#lang at-exp s-exp "../common.rkt"

(define-context "bugs")

(define planet-bugs "http://planet.racket-lang.org/trac/newticket")

(require "../www/main.rkt")

(define query
  @page[#:title "Query Bug Reports" #:file "query/"]{
    @em{TODO}})

(define index
  @page[#:title "Bug Reports"]{
    @p[style: "padding: 5px; color: #a00; border: 1px solid;"]{@em{
         If you can, please use the Bug Report item in DrRacket's Help menu. It
         works better than this page, because it helps you supply precise
         information about your Racket installation and working environment.}}
    @p{@strong{Note:} Bug reports for PLaneT packages are submitted on the
       @a[href: planet-bugs]{PLaneT server}.}
    @p{Before submitting a bug report, you may wish to:
       @ul{@li{Consult the @-docs,}
           @li{@download a newer Racket version if there is one (Racket
               displays its version number on startup),}
           @li{@query{Query existing bug reports}.}}}})
