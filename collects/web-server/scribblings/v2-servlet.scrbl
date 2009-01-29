#lang scribble/doc
@(require "web-server.ss")

@title{Version 2 Servlets}
@(require (for-label web-server/http
                     (except-in web-server/managers/manager manager)
                     "dummy-v2-servlet.ss")) @; to give a binding context
@declare-exporting[#:use-sources (web-server/scribblings/dummy-v2-servlet)]

@defthing[interface-version (one-of/c 'v2)]{
 This indicates that the servlet is a version two servlet.
}

@defthing[manager manager?]{
 The manager for the continuations of this servlet. See @secref["managers"] for options.
}

@defproc[(start [initial-request request?])
         response/c]{
 This function is called when an instance of this servlet is started.
 The argument is the HTTP request that initiated the instance.
}

An example version 2 module:
@(require (for-label web-server/managers/none))
@schememod[
 scheme
 (require web-server/managers/none)
 
 (define interface-version 'v2)
 (define manager 
   (create-none-manager
    (lambda (req)
      `(html (head (title "No Continuations Here!"))
             (body (h1 "No Continuations Here!"))))))
 (define (start req)
   `(html (head (title "Hello World!"))
          (body (h1 "Hi Mom!"))))
 ]

These servlets should use the @schememodname[web-server/servlet] API.
