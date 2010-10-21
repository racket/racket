#lang scribble/base

@(require "shared.rkt")

@title{How to Program Racket}
@author{Matthias Felleisen, Matthew Flatt}

@; -----------------------------------------------------------------------------

Since 1995 PLT has grown from a handful of people who worked on/with the
 repository to three dozen and more. In addition, Racekt is an open source
 project, meaning other people study the code in the repository and use it
 as an implicit guide to Racket programming. To manage the growth of the PLT
 developer basis and to showcase good Racket coding, every contribution
 should satisfy certain basic criteria.

This document spells out these criteria, and it is also a call for
 improvements and suggestions for additional criteria.  Like code, this
 document lives and benefits from the "many pairs of eyes" effect of open
 source. Code should be viewed by more than one person; a second person is
 likely to catch mistakes and problems and hidden bugs. This document is
 meta-code, and the same ideas apply. If you have suggestions, contact the
 authors via email.

@; -----------------------------------------------------------------------------

@include-section["correct-maintain-speed.scrbl"]
@include-section["textual.scrbl"]
