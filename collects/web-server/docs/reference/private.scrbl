#reader(lib "docreader.ss" "scribble")
@require["../web-server.ss"]

@title[#:tag "private"
       #:style 'toc]{Internal}

The @web-server is a complicated piece of software and as a result,
defines a number of interesting and independently useful sub-components.
Some of these are documented here.

@local-table-of-contents[]


@; ------------------------------------------------------------
@section[#:tag "timer.ss"]{Timers}

@file{private/timer.ss} provides a functionality for running
procedures after a given amount of time, that may be extended.

@defstruct[timer ([evt evt?]
                  [expire-seconds number?]
                  [action (-> void)])]{
 @scheme[evt] is an @scheme[alarm-evt] that is ready at @scheme[expire-seconds].
 @scheme[action] should be called when this @scheme[evt] is ready.
}

@defproc[(start-timer-manager [cust custodian?])
         void]{
 Handles the execution and management of timers. Resources are charged to
 @scheme[cust].
}

@defproc[(start-timer [s number?]
                      [action (-> void)])
         timer?]{
 Registers a timer that runs @scheme[action] after @scheme[s] seconds.
}

@defproc[(reset-timer! [t timer?]
                       [s number?])
         void]{
 Changes @scheme[t] so that it will fire after @scheme[s] seconds.
}

@defproc[(increment-timer! [t timer?]
                           [s number?])
         void]{
 Changes @scheme[t] so that it will fire after @scheme[s] seconds from when
 it does now.
}

@defproc[(cancel-timer! [t timer?])
         void]{
 Cancels the firing of @scheme[t] ever and frees resources used by @scheme[t].
}


@; XXX Generalize
@; ------------------------------------------------------------
@section[#:tag "connection-manager.ss"]{Connection Manager}

@file{private/connection-manager.ss} provides functionality for managing pairs of
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

@; XXX Don't pass in parent-cust
@defproc[(start-connection-manager [parent-cust custodian?])
         void]{
 Runs the connection manager (now just the timer manager) will @scheme[parent-cust]
 as the custodian.
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
 Calls @scheme[reset-timer!] with the timer behind @scheme[c] with @scheme[t].
}

@; ------------------------------------------------------------
@section[#:tag "dispatch-server-unit.ss"]{Dispatching Server}

The @web-server is just a configuration of a dispatching server.
This dispatching server component is useful on its own.

@file{private/dispatch-server-sig.ss} defines the following signatures:

@defthing[dispatch-server^ signature?]{
 The following identifiers:
 @defproc[(serve)
          (-> void)]{
  Runs and returns a shutdown procedure.
 }
 @defproc[(serve-ports [i input-port?]
                       [o output-port?])
          void]{
  Serves a single connection with @scheme[i] and @scheme[o].
 }
}

@defthing[dispatch-server-config^ signature?]{
 The following identifiers:
 @defthing[port port?]{Specifies the port to serve on.}
 @defthing[listen-ip string?]{Passed to @scheme[tcp-accept].}
 @defthing[max-waiting integer?]{Passed to @scheme[tcp-accept].}
 @defthing[initial-connection-timeout integer?]{Specifies the initial timeout given to a connection.}
 @defproc[(read-request [c connection?]
                        [p port?]
                        [port-addresses port-addresses?])
          any/c]{
  Defines the way the server reads requests off connections to be passed
  to @scheme[dispatch].
 }
 @defthing[dispatch dispatcher?]{How to handle requests.}
}

@file{private/dispatch-server-unit.ss} provides the unit
which actually implements a dispatching server.

@; XXX Talk about how threads and custodians are used.

@defthing[dispatch-server\@ (unit/c (tcp^ dispatch-server-config^) (dispatch-server^))]{
 Runs the dispatching server config in a very basic way, except that it uses
 @secref["connection-manager.ss"] to manage connections.
}

@; ------------------------------------------------------------
@section[#:tag "closure.ss"]{Serializable Closures}

The defunctionalization process of the Web Language (see @secref["lang"])
requires an explicit representation of closures that is serializable.
@file{private/closure.ss} is this representation. It provides:

@defproc[(make-closure-definition-syntax [tag syntax?]
                                         [fvars (listof identifier?)]
                                         [proc syntax?])
         syntax?]{
 Outputs a syntax object that defines a serializable structure,
 with @scheme[tag] as the tag, that represents a closure over
 @scheme[fvars], that acts a procedure and when invoked calls
 @scheme[proc], which is assumed to be syntax of @scheme[lambda]
 or @scheme[case-lambda].
}

@defproc[(closure->deserialize-name [c closure?])
         symbol?]{
 Extracts the unique tag of a closure @scheme[c]
}

These are difficult to use directly, so @file{private/define-closure.ss}
defines a helper form:

@defform[(define-closure tag formals (free-vars ...) body)]{
 Defines a closure, constructed with @scheme[make-tag] that accepts
 @scheme[freevars ...], that when invoked with @scheme[formals]
 executes @scheme[body].
}

@; XXX Example

@; ------------------------------------------------------------
@section[#:tag "cache-table.ss"]{Cache Table}

@file{private/cache-table.ss} provides a set of caching hash table
functions.

@defproc[(make-cache-table)
         cache-table?]{
 Constructs a cache-table.
}

@defproc[(cache-table-lookup! [ct cache-table?]
                              [id symbol?]
                              [mk (-> any/c)])
         any/c]{
 Looks up @scheme[id] in @scheme[ct]. If it is not present, then @scheme[mk] is
 called to construct the value and add it to @scheme[ct].
}

@defproc[(cache-table-clear! [ct cache-table?])
         void?]{
 Clears all entries in @scheme[ct].
}

@defproc[(cache-table? [v any/c])
         boolean?]{
 Determines if @scheme[v] is a cache table.
}

@; ------------------------------------------------------------
@section[#:tag "mime-types.ss"]{MIME Types}

@file{private/mime-types.ss} provides function for dealing with @file{mime.types}
files.

@defproc[(read-mime-types [p path?])
         (hash-table/c symbol? bytes?)]{
 Reads the @file{mime.types} file from @scheme[p] and constructs a
 hash table mapping extensions to MIME types.
}

@defproc[(make-path->mime-type [p path?])
         (path? . -> . bytes?)]{
 Uses a @scheme[read-mime-types] with @scheme[p] and constructs a
 function from paths to their MIME type.
}

@; XXX Rename mod-map.ss
@; ------------------------------------------------------------
@section[#:tag "mod-map.ss"]{Serialization Utilities}

@scheme[(lib "serialize.ss")] provides the functionality of serializing
values. @file{private/mod-map.ss} compresses the serialized representation.

@defproc[(compress-serial [sv serialized-value?])
         compressed-serialized-value?]{
 Collapses multiple occurrences of the same module in the module
 map of the serialized representation, @scheme[sv].
}

@defproc[(decompress-serial [csv compressed-serialized-value?])
         serialized-value?]{
 Expands multiple occurrences of the same module in the module
 map of the compressed serialized representation, @scheme[csv].
}

@; ------------------------------------------------------------
@section[#:tag "url-param.ss"]{URL Param}

The @web-server needs to encode information in URLs. If this data
is stored in the query string, than it will be overridden by browsers that
make GET requests to those URLs with more query data. So, it must be encoded
in URL params. @file{private/url-param.ss} provides functions for helping
with this process.

@defproc[(insert-param [u url?]
                       [k string?]
                       [v string?])
         url?]{
 Associates @scheme[k] with @scheme[v] in the final URL param of @scheme[u],
 overwritting any current binding for @scheme[k].
}

@defproc[(extract-param [u url?]
                        [k string?])
         (or/c string? false/c)]{
 Extracts the string associated with @scheme[k] in the final URL param of
 @scheme[u], if there is one, returning @scheme[#f] otherwise.
}

@; ------------------------------------------------------------
@section[#:tag "util.ss"]{Miscellaneous Utilities}

There are a number of other miscellaneous utilities the @web-server
needs. They are provided by @file{private/util.ss}.

@subsection{Contracts}
@defthing[port-number? contract?]{Equivalent to @scheme[(between/c 1 65535)].}
@defthing[path-element? contract?]{Equivalent to @scheme[(or/c string? path? (symbols 'up 'same))].}

@subsection{Lists}
@defproc[(list-prefix? [l list?]
                       [r list?])
         boolean?]{
 True if @scheme[l] is a prefix of @scheme[r].
}

@subsection{URLs}

@defproc[(url-replace-path [proc ((listof path/param?) . -> . (listof path/param?))]
                           [u url?])
         url?]{
 Replaces the URL path of @scheme[u] with @scheme[proc] of the former path.
}

@; XXX Remove use or take url?
@defproc[(url-path->string [url-path (listof path/param?)])
         string?]{
 Formats @scheme[url-path] as a string with @scheme["/"] as a delimiter
 and no params.
}

@subsection{Paths}
@defproc[(explode-path* [p path?])
         (listof path-element?)]{
 Like @scheme[normalize-path], but does not resolve symlinks.
}

@defproc[(path-without-base [base path?]
                            [p path?])
         (listof path-element?)]{
 Returns, as a list, the portion of @scheme[p] after @scheme[base],
 assuming @scheme[base] is a prefix of @scheme[p].
}

@defproc[(directory-part [p path?])
         path?]{
 Returns the directory part of @scheme[p], returning @scheme[(current-directory)]
 if it is relative.
}

@defproc[(build-path-unless-absolute [base path-string?]
                                     [p path-string?])
         path?]{
 Prepends @scheme[base] to @scheme[p], unless @scheme[p] is absolute.
}

@defproc[(strip-prefix-ups [p (listof path-element?)])
         (listof path-element?)]{
 Removes all the prefix @scheme[".."]s from @scheme[p].
}

@subsection{Exceptions}

@defproc[(pretty-print-invalid-xexpr [exn exn:invalid-xexpr?]
                                     [v any/c])
         void]{
 Prints @scheme[v] as if it were almost an X-expression highlighting the error
 according to @scheme[exn].
}

@; XXX Remove
@defproc[(network-error [s symbol?]
                        [fmt string?]
                        [v any/c] ...)
         void]{
 Like @scheme[error], but throws a @scheme[exn:fail:network].
}

@defproc[(exn->string [exn (or/c exn? any/c)])
         string?]{
 Formats @scheme[exn] with @scheme[(error-display-handler)] as a string.
}

@subsection{Strings}

@defproc[(lowercase-symbol! [sb (or/c string? bytes?)])
         symbol?]{
 Returns @scheme[sb] as a lowercase symbol.
}

@defproc[(read/string [s string?])
         serializable?]{
 @scheme[read]s a value from @scheme[s] and returns it.
}

@defproc[(write/string [v serializable?])
         string?]{
 @scheme[write]s @scheme[v] to a string and returns it.
}
