#lang scribble/doc
@(require "web-server.ss")

@title[#:tag "connection-manager.ss"]{Connection Manager}
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
 A connection is a pair of ports (@scheme[i-port] and @scheme[o-port]) that is
 ready to close after the current job if @scheme[close?] is @scheme[#t]. Resources
 associated with the connection should be allocated under @scheme[custodian].
 The connection will last until @scheme[timer] triggers.
}

@defproc[(start-connection-manager)
         void]{
 Runs the connection manager (now just the timer manager).
}

@defproc[(new-connection [timeout number?]
                         [i-port input-port?]
                         [o-port output-port?]
                         [cust custodian?]
                         [close? boolean?])
         connection?]{
 Constructs a connection with a timer with a trigger of @scheme[timeout] that calls
 @scheme[kill-connection!].
}

@defproc[(kill-connection! [c connection?])
         void]{
 Closes the ports associated with @scheme[c], kills the timer, and shuts down
 the custodian.
}

@defproc[(adjust-connection-timeout! [c connection?]
                                     [t number?])
         void]{
 Calls @scheme[increment-timer!] with the timer behind @scheme[c] with @scheme[t].
}

}
