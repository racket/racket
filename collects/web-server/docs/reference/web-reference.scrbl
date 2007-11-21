#reader(lib "docreader.ss" "scribble")
@require["../web-server.ss"]

@title[#:tag "web-server-ref"]{Web Server Reference Manual}
@author{Jay McCarthy (jay@"@"plt-scheme.org)}

The @web-server collection provides libraries that can be used to
develop Web applications in Scheme.

@table-of-contents[]

@include-section["running.scrbl"]

@include-section["servlet.scrbl"]
@include-section["lang.scrbl"]

@include-section["configuration.scrbl"]
@include-section["dispatchers.scrbl"]
@include-section["web-config-unit.scrbl"]
@include-section["web-server-unit.scrbl"]
@include-section["managers.scrbl"]

@include-section["private.scrbl"]

@; ------------------------------------------------------------
@section[#:tag "ack"]{Acknowledgements}

We thank Matthew Flatt for his superlative work on MzScheme.
We thank the previous maintainers of the @web-server : Paul T. Graunke, Mike Burns, and Greg Pettyjohn
Numerous people have
provided invaluable feedback on the server, including Eli Barzilay, Ryan Culpepper, Robby
Findler, Dan Licata, Matt Jadud, Jacob Matthews, Matthias Radestock, Andrey Skylar,
Michael Sperber, Dave Tucker, Anton van Straaten, and Noel Welsh. We also thank the
many other PLT Scheme users who have exercised the server and offered critiques.

@index-section["web-server-ref-index"]
