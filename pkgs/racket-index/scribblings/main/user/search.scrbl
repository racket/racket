#lang scribble/doc
@(require scribble/base
          scribble/core
          scribble/html-properties
          "../private/utils.rkt"
          "../private/make-search.rkt")

@main-page['search #f
                   ;; "racket.css" needs to be installed for search results:
                   #:force-racket-css? #t]

@para[#:style (style #f (list (attributes (list (cons 'class "plt_local_only")))))]{You are searching only your locally installed Racket packages. More results may be available by using the @hyperlink["http://docs.racket-lang.org/search/"]{global search} that inspects all available packages. @elem[#:style (style #f (list (attributes (list (cons 'id "redo_search_global")))))]{You may wish to repeat your search globally.}}

@make-search[#t]
