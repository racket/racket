#lang scribble/doc
@(require "web-server.rkt"
          (for-label web-server/http
                     web-server/lang/soft
                     web-server/lang/web))

@title[]{Soft State}

@defmodule[web-server/lang/soft]{

Sometimes you want to reference a large data-structure from a stateless program without the data-structure being serialized 
and increasing the size of the serialization. This module provides support for this scenario.

@defproc[(soft-state? [v any/c])
         boolean?]{
 Determines if @racket[v] is a soft state record.
}
                  
@defproc[(make-soft-state [thnk (-> any/c)])
         soft-state?]{
 Creates a piece of soft state that is computed by @racket[thnk]. This value is serializable.
}

@defproc[(soft-state-ref [ss soft-state?])
         any/c]{
 Extracts the value associated with @racket[ss]. If the value is not
 available (perhaps because of garbage collection, deserialization in an
 uninitialized process, etc), then the thunk associated with @racket[ss]
 is invoked and the value is cached.
}

@defform[(soft-state expr ...)]{
 Equivalent to @racket[(make-soft-state (lambda () expr ...))].
}

Here's an example servlet that uses soft state:
@racketmod[
 web-server
 
 (provide interface-version start) 
 (define interface-version 'stateless)
 
 (define softie
   (soft-state
    (printf "Doing a long computation...\n")
    (sleep 1)))
 
 (define (start req)
   (soft-state-ref softie)
   (printf "Done\n")
   (start
    (send/suspend
     (lambda (k-url)
       (response/xexpr
        `(html (body (a ([href ,k-url]) "Done"))))))))
]

When this is run and the link is clicked a few times, the output is:
@verbatim{
$ plt-web-server -p 8080
Doing a long computation...
Done
Done
Done
Done
}

If the server is restarted or the hostname in the URL is changed to a different host with the same code, and the URL is clicked:
@verbatim{
^Cuser break
$ plt-web-server -p 8080
Doing a long computation...
Done
}

}
