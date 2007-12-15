#lang scribble/doc
@(require "common.ss")

@title{@bold{Net}: PLT Networking Libraries}

@table-of-contents[]

@include-section["url.scrbl"]
@include-section["sendurl.scrbl"]
@include-section["smtp.scrbl"]
@include-section["cgi.scrbl"]
@include-section["sendmail.scrbl"]

@(bibliography

 (bib-entry #:key "CGI"
            #:title "Common Gateway Interface (CGI/1.1)"
            #:url "http://hoohoo.ncsa.uiuc.edu/cgi/")

 (bib-entry #:key "RFC2396"
            #:title "Uniform Resource Identifiers (URI): Generic Syntax"
            #:author "T. Berners-Lee, R. Fielding, and L. Masinter"
            #:location "RFC"
            #:url "http://www.ietf.org/rfc/rfc2396.txt"
            #:date "1998")

 (bib-entry #:key "RFC3986"
            #:title "Uniform Resource Identifier (URI): Generic Syntax"
            #:author "T. Berners-Lee, R. Fielding, and L. Masinter"
            #:location "RFC"
            #:url "http://www.ietf.org/rfc/rfc3986.txt"
            #:date "2005")

)

@index-section[]
