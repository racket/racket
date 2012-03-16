#lang scribble/doc
@(require "web-server.rkt")

@title[#:tag "stateless"]{Stateless Servlets}

@defmodulelang*[(web-server/base web-server)]

@section[#:style 'hidden #:tag "stateless-example"]{Example}

A stateless servlet should @racket[provide] the following exports:

@(require (for-label web-server/http
                     racket/serialize
                     (except-in web-server/stuffers stuffer)
                     web-server/managers/none
                     (except-in web-server/managers/manager manager)
                     "dummy-stateless-servlet.rkt")) @; to give a binding context
@declare-exporting[#:use-sources (web-server/scribblings/dummy-stateless-servlet)]

@defthing[interface-version (one-of/c 'stateless)]{
 This indicates that the servlet is a stateless servlet.
}

@defthing[stuffer (stuffer/c serializable? bytes?)]{
 This is the stuffer that will be used for the servlet.
      
 If it is not provided, it defaults to @racket[default-stuffer].
}

@defthing[manager manager?]{
 This is the manager that will be used for the servlet.
      
 If it is not provided, it defaults to @racket[(create-none-manager #f)].
}

@defproc[(start [initial-request request?])
         response?]{
 This function is called when an instance of this servlet is started.
 The argument is the HTTP request that initiated the instance.
}

An example @racket['stateless] servlet module:
@racketmod[
 web-server
 (require web-server/http)
 (provide interface-version stuffer start)
 (define interface-version 'stateless)
 (define stuffer
   (stuffer-chain
    serialize-stuffer
    (md5-stuffer (build-path (find-system-path 'home-dir) ".urls"))))
 (define (start req)
   (response/xexpr
    `(html (body (h2 "Look ma, no state!")))))
]

The @racketmodname[web-server/base] language exports all of the functions
and syntax from @racketmodname[racket/base] and nothing else.

The @racketmodname[web-server] language exports all of the functions
and syntax from the following libraries: @racketmodname[racket],
@racketmodname[net/url], @racketmodname[web-server/http],
@racketmodname[web-server/http/bindings],
@racketmodname[web-server/lang/abort-resume],
@racketmodname[web-server/lang/web],
@racketmodname[web-server/lang/native],
@racketmodname[web-server/lang/web-param],
@racketmodname[web-server/lang/web-cells],
@racketmodname[web-server/lang/file-box],
@racketmodname[web-server/lang/soft],
@racketmodname[web-server/dispatch], and
@racketmodname[web-server/stuffers].  Some of these are documented in
the subsections that follow.

@include-section["stateless-usage.scrbl"]
@include-section["serial.scrbl"]
@include-section["native.scrbl"]
@include-section["lang.scrbl"]
@include-section["lang-web-cells.scrbl"]
@include-section["file-box.scrbl"]
@include-section["web-param.scrbl"]
@include-section["soft.scrbl"]
@include-section["stuffers.scrbl"]
