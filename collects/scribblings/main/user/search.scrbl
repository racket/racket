#lang scribble/doc
@(require "../private/utils.ss"
          "../private/make-search.ss")

@main-page['search #f
                   ;; "racket.css" needs to be installed for search results:
                   #:force-racket-css? #t]

@make-search[#t]
