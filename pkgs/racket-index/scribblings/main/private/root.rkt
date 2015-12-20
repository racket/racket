#lang at-exp racket/base
(require scribble/core
         scribble/decode
         scribble/manual)

(define h bold)

(define (root global?)
  (make-splice
   @list{@h{Are you looking for a Racket tutorial?}

         If you are new to programming, try looking at @other-doc['(lib "scribblings/quick/quick.scrbl")]. Otherwise, you may be interested in @other-doc['(lib "scribblings/more/more.scrbl")].

         @h{Are you looking for a quick cheat sheet on Racket?}

         This is not the page you are looking for. The @other-doc['(lib "racket-cheat/racket-cheat.scrbl")] is the page you are looking for.

         @h{Are you looking for the complete reference for Racket and its standard library?}

         @other-doc['(lib "scribblings/reference/reference.scrbl")] is an exhaustive reference and @other-doc['(lib "scribblings/guide/guide.scrbl")] is a more casual introduction through applications.

         @h{Are you looking for a list of the manuals of the libraries available on this system?}

         The @other-doc[(if global? '(lib "scribblings/main/start.scrbl") '(lib "scribblings/main/user/user-start.scrbl"))] contains categorized links to everything you have installed.

         @h{Can't find what you are looking for locally?}
         
         The online @link["http://pkg-build.racket-lang.org/doc/start/index.html"]{Manual List} contains categorized documentation all packages indexed on @link["http://pkgs.racket-lang.org"]{@tt{http://pkgs.racket-lang.org}}. If you can't find something you need on your system, try looking there.}))

(provide root)
