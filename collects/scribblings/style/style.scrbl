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

@bold{Note} We understand that some of the files in the code base do not
 live up to these standards. Help us improve these files. If you need to
 edit and understand a file that fails in some ways, take the time to
 reorganize it properly as soon as the comprehension step takes longer than
 a few minutes. Because if understanding takes a lot of time, it is likely
 that the file isn't maintainable. Whoever touches the file next will be
 grateful.

@; -----------------------------------------------------------------------------

@include-section["correct-maintain-speed.scrbl"]
@include-section["size.scrbl"]
@include-section["textual.scrbl"]
