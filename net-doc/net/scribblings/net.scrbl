#lang scribble/doc
@(require "common.rkt")

@title{Net: Networking Libraries}

@table-of-contents[]

@include-section["http-client.scrbl"]
@include-section["url.scrbl"]
@include-section["uri-codec.scrbl"]
@include-section["ftp.scrbl"]
@include-section["sendurl.scrbl"]
@include-section["smtp.scrbl"]
@include-section["sendmail.scrbl"]
@include-section["head.scrbl"]
@include-section["unihead.scrbl"]
@include-section["imap.scrbl"]
@include-section["pop3.scrbl"]
@include-section["mime.scrbl"]
@include-section["base64.scrbl"]
@include-section["qp.scrbl"]
@include-section["dns.scrbl"]
@include-section["nntp.scrbl"]
@include-section["tcp.scrbl"]
@include-section["tcp-redirect.scrbl"]
@include-section["ssl-tcp-unit.scrbl"]
@include-section["cgi.scrbl"]
@include-section["cookie.scrbl"]
@include-section["git-checkout.scrbl"]

@(bibliography

 (bib-entry #:key "CGI"
            #:title "Common Gateway Interface (CGI/1.1)"
            #:url "http://hoohoo.ncsa.uiuc.edu/cgi/")

 (bib-entry #:key "RFC822"
            #:title "Standard for the Format of ARPA Internet Text Messages"
            #:author "David Crocker"
            #:location "RFC"
            #:url "http://www.ietf.org/rfc/rfc0822.txt"
            #:date "1982")

 (bib-entry #:key "RFC977"
            #:title "Network News Transfer Protocol"
            #:author "Brian Kantor and Phil Lapsley"
            #:location "RFC"
            #:url "http://www.ietf.org/rfc/rfc0977.txt"
            #:date "1986")

 (bib-entry #:key "RFC1738"
            #:title "Uniform Resource Locators (URL)"
            #:author "T. Berners-Lee, L. Masinter, and M. McCahill"
            #:location "RFC"
            #:url "http://www.ietf.org/rfc/rfc1738.txt"
            #:date "1994")

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

 (bib-entry #:key "RFC2109"
            #:title "HTTP State Management Mechanism"
            #:author "D. Kristol and L. Montulli"
            #:location "RFC"
            #:url "http://www.ietf.org/rfc/rfc2109.txt"
            #:date "1997")

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

 (bib-entry #:key "RFC6265"
            #:title "HTTP State Management Mechanism"
            #:author "A. Barth"
            #:location "RFC"
            #:url "http://tools.ietf.org/html/rfc6265.html"
            #:date "2011")
)

@index-section[]
