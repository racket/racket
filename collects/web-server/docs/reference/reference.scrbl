#reader(lib "docreader.ss" "scribble")
@require["../web-server.ss"]

@title[#:tag "web-server-ref"]{Web Server Reference Manual}

The @file{web-server} collection provides libraries that can be used to
develop Web applications in Scheme.

@table-of-contents[]

@include-section["running.scrbl"]
@include-section["configuration.scrbl"]
@include-section["dispatchers.scrbl"]
@include-section["web-config-unit.scrbl"]
@include-section["web-server-unit.scrbl"]
@include-section["managers.scrbl"]
@include-section["servlet-env.scrbl"]

@include-section["servlet.scrbl"]

@include-section["lang.scrbl"]

@include-section["private.scrbl"]

@index-section["web-server-ref-index"]