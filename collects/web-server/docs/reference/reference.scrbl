#reader(lib "docreader.ss" "scribble")
@require["../web-server.ss"]

@title{Web Server Reference Manual}

The @file{web-server} collection provides libraries that can be used to
develop Web applications in Scheme.

@table-of-contents[]

@include-section["command-line.scrbl"]
@include-section["configuration.scrbl"]
@include-section["dispatchers.scrbl"]
@include-section["lang.scrbl"]
@include-section["managers.scrbl"]
@include-section["private.scrbl"]
@include-section["servlet-env.scrbl"]
@include-section["servlet.scrbl"]
@include-section["web-config-unit.scrbl"]
@include-section["web-server-unit.scrbl"]
@include-section["web-server.scrbl"]

@index-section["web-server-ref-index"]