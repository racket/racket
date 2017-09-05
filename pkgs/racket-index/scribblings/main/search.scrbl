#lang scribble/doc
@(require scribble/base
          scribble/core
          scribble/html-properties
          "private/utils.rkt"
          "private/make-search.rkt"
          "config.rkt")

@main-page['search #t]

@para[#:style (style #f (list (attributes (list (cons 'class "plt_global_only")))))]{You are searching all available Racket packages, including those that you may not have installed locally. Therefore, you may need to install a package to use the results shown below. @hyperlink["http://docs.racket-lang.org/pkg/getting-started.html"]{Getting Started with Packages} guides you through this process. If you want to re-run your search with local results only, press F1 in DrRacket or run @tt{raco docs} on the command line.}

@para[#:style (style #f (list (attributes (list (cons 'class "plt_local_only")))))]{You are searching only your locally installed Racket packages. More results may be available by using the @hyperlink["http://docs.racket-lang.org/search/"]{global search} that inspects all available packages. @elem[#:style (style #f (list (attributes (list (cons 'id "redo_search_global")))))]{You may wish to repeat your search globally.}}

@make-search[#f]

