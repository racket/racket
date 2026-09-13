#lang scribble/doc
@(require scribble/base
          scribble/core
          scribble/html-properties
          "private/utils.rkt"
          "private/make-search.rkt"
          "private/notice.rkt"
          "config.rkt")

@main-page['search #t
           #:extra-additions
           (list (make-css-addition
                  (collection-file-path
                   "search.css"
                   "scribblings/main/private")))]

@global-notice
@local-notice

@make-search[#f]

