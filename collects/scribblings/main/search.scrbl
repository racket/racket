#lang scribble/doc
@(require "private/utils.rkt" "private/make-search.rkt")

@main-page['search #t
                   ;; "racket.css" needs to be installed for search results:
                   #:force-racket-css? #t]

@make-search[#f]
