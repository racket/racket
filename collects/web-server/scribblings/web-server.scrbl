#lang scribble/doc
@(require "web-server.ss")

@title[#:tag "web-server-ref"]{@bold{Web Server}: PLT HTTP Server}

@author[(author+email "Jay McCarthy" "jay@plt-scheme.org")]

The @web-server collection provides libraries that can be used to
develop Web applications in Scheme.

@table-of-contents[]

@include-section["running.scrbl"]

@include-section["servlet.scrbl"]
@include-section["lang.scrbl"]

@include-section["formlets.scrbl"]

@include-section["configuration.scrbl"]
@include-section["dispatchers.scrbl"]
@include-section["web-config-unit.scrbl"]
@include-section["web-server-unit.scrbl"]
@include-section["managers.scrbl"]

@include-section["private.scrbl"]

@include-section["faq.scrbl"]

@; ------------------------------------------------------------
@section[#:tag "ack"]{Acknowledgements}

We thank Matthew Flatt for his superlative work on MzScheme.
We thank the previous maintainers of the @web-server : Paul T. Graunke, Mike Burns, and Greg Pettyjohn
Numerous people have
provided invaluable feedback on the server, including Eli Barzilay, Ryan Culpepper, Robby
Findler, Dave Gurnell, Matt Jadud, Dan Licata, Jacob Matthews, Matthias Radestock, Andrey Skylar,
Michael Sperber, Anton van Straaten, Dave Tucker, and Noel Welsh. We also thank the
many other PLT Scheme users who have exercised the server and offered critiques.

@index-section[]
