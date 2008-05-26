#lang scribble/doc
@(require scribble/manual
          scribble/struct
          "../private/utils.ss"
          "../private/manuals.ss")

@main-page['start #f]

@;{
@; This page should always be the default, so it doesn't need to say
@; something special or link to the main page.
@margin-note{This is a user-specific listing, which may include
             @|PLaneT| packages and other collections that are not in
             the main installation. The main installation's listing is
             @other-manual['(lib "scribblings/start/start.scrbl")].}
;}

@margin-note{This is the PLT Scheme documentation, including
             user-specific packages (@|PLaneT| packages and other
             collections) that are not in the main installation.}

@(make-start-page #t)
