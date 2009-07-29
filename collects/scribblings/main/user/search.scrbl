#lang scribble/doc
@(require "../private/utils.ss"
          "../private/make-search.ss")

@main-page['search #f
                   ;; "scheme.css" needs to be installed for search results:
                   #:force-scheme-css? #t]

@make-search[#t]
