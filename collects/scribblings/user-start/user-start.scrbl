#lang scribble/doc
@(require scribble/manual
          scribble/struct
          "../start/manuals.ss")

@title{PLT Scheme Documentation (user)}

@margin-note{This is a user-specific listing, which may include
             @|PLaneT| packages and other collections that are not in
             the main installation. The main installation's listing is
             @other-manual['(lib "scribblings/start/start.scrbl")].}

@(build-contents #t)

@(make-toc-element
  #f
  null
  (list @link["master-index/index.html" #:underline? #f]{master index}))
