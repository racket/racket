#lang scribble/doc
@(require "web-server.rkt")

@title[#:tag "connection-manager"]{Connection Manager}
@(require (for-label web-server/private/connection-manager
                     web-server/private/timer))

@defmodule[web-server/private/connection-manager]{

This module provides functionality for managing pairs of
input and output ports. We have plans to allow a number of different strategies
for doing this.

@defstruct[connection
           ([timer timer?]
            [i-port input-port?] [o-port output-port?] [custodian custodian?]
            [close? boolean?])]{
 A connection is a pair of ports (@racket[i-port] and @racket[o-port]) that is
 ready to close after the current job if @racket[close?] is @racket[#t]. Resources
 associated with the connection should be allocated under @racket[custodian].
 The connection will last until @racket[timer] triggers.
}

@defproc[(connection-manager? [x any/c]) boolean?]{
 Determines if @racket[x] is a connection manager.
}

@defproc[(start-connection-manager)
         connection-manager?]{
 Runs the connection manager (now just the timer manager).
}

@defproc[(new-connection [cm connection-manager?]
                         [timeout number?]
                         [i-port input-port?]
                         [o-port output-port?]
                         [cust custodian?]
                         [close? boolean?])
         connection?]{
 Constructs a connection with a timer with a trigger of @racket[timeout] that calls
 @racket[kill-connection!].
}

@defproc[(kill-connection! [c connection?])
         void]{
 Closes the ports associated with @racket[c], kills the timer, and shuts down
 the custodian.
}

@defproc[(adjust-connection-timeout! [c connection?]
                                     [t number?])
         void]{
 Calls @racket[increment-timer!] with the timer behind @racket[c] with @racket[t].
}

}
