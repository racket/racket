#reader(lib "docreader.ss" "scribble")
@require["../web-server.ss"]

@title[#:style 'toc]{Scheme Servlets}

The @file{web-server} allows servlets to be written in Scheme. It
provides the supporting API, described below, for the construction
of these servlets.

@local-table-of-contents[]

@; ------------------------------------------------------------
@section[#:tag "module-servlets"]{Definition}

A @defterm{servlet} is a module that provides the following:

@defthing[interface-version symbol?]{
 A symbol indicating the servlet interface the servlet conforms
 to. This influences the other provided identifiers.
}

If @scheme[interface-version] is @scheme['v1], then the servlet 
provides:

@defthing[timeout integer?]{
 This number is used as the @scheme[continuation-timeout] argument to
 a timeout-based continuation manager used for this servlet. (See
 @secref["timeouts.ss"].)
}

@defproc[(start [initial-request request?])
         response?]{
 This function is called when an instance of this servlet is started.
 The argument is the HTTP request that initiated the instance.
}

If @scheme[interface-version] is @scheme['v2], then the servlet 
provides:

@defthing[manager manager?]{
 The manager for the continuations of this servlet.
}

@defproc[(start [initial-request request?])
         response?]{
 This function is called when an instance of this servlet is started.
 The argument is the HTTP request that initiated the instance.
}

@; ------------------------------------------------------------
@section[#:tag "servlet-structs.ss"]{Contracts}

XXX

@; ------------------------------------------------------------
@section[#:tag "request-structs.ss"]{HTTP Requests}

XXX

@; ------------------------------------------------------------
@section[#:tag "bindings.ss"]{Request Bindings}

XXX

@; ------------------------------------------------------------
@section[#:tag "response-structs.ss"]{HTTP Responses}

XXX

@; ------------------------------------------------------------
@section[#:tag "web.ss"]{Web}

XXX

@; ------------------------------------------------------------
@section[#:tag "helpers.ss"]{Helpers}

XXX

@; XXX Depreciate
@; ------------------------------------------------------------
@section[#:tag "servlet-url.ss"]{Servlet URLs}

XXX

@; ------------------------------------------------------------
@section[#:tag "basic-auth.ss"]{Basic Authentication}

XXX

@; ------------------------------------------------------------
@section[#:tag "web-cells.ss"]{Web Cells}

XXX

