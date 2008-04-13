#lang scribble/doc
@(require "common.ss")

@title{@bold{Net}: PLT Networking Libraries}

@table-of-contents[]

@include-section["url.scrbl"]
@include-section["sendurl.scrbl"]
@include-section["smtp.scrbl"]
@include-section["cgi.scrbl"]
@include-section["sendmail.scrbl"]
@include-section["nntp.scrbl"]
@include-section["pop3.scrbl"]
@include-section["imap.scrbl"]
@include-section["gifwrite.scrbl"]

@(bibliography

 (bib-entry #:key "CGI"
            #:title "Common Gateway Interface (CGI/1.1)"
            #:url "http://hoohoo.ncsa.uiuc.edu/cgi/")

 (bib-entry #:key "RFC977"
            #:title "Network News Transfer Protocol"
            #:author "Brian Kantor and Phil Lapsley"
            #:location "RFC"
            #:url "http://www.ietf.org/rfc/rfc0977.txt"
            #:date "1986")
            
 (bib-entry #:key "RFC1939"
            #:title "Post Office Protocol - Version 3"
            #:author "J. Myers and M. Rose"
            #:location "RFC"
            #:url "http://www.ietf.org/rfc/rfc1939.txt"
            #:date "1996")

 (bib-entry #:key "RFC2060"
            #:title "Internet Message Access Protocol - Version 4rev1"
            #:author "M. Crispin"
            #:location "RFC"
            #:url "http://www.ietf.org/rfc/rfc2060.txt"
            #:date "1996")

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
