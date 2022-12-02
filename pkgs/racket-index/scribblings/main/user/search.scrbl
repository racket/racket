#lang scribble/doc
@(require scribble/html-properties
          "../private/utils.rkt"
          "../private/make-search.rkt"
          "../private/notice.rkt")

@main-page['search #f
           ;; "racket.css" needs to be installed for search results:
           #:force-racket-css? #t
           #:extra-additions
           (list (make-css-addition
                  (collection-file-path
                   "search.css"
                   "scribblings/main/private")))]

@local-notice

@make-search[#t]
