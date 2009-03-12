#lang scribble/doc
@(require "web-server.ss")

@title[#:tag "private" #:style 'toc]{Internal}
@(require (for-label scheme/tcp
                     web-server/dispatchers/dispatch
                     net/url
                     scheme/serialize
                     xml
                     net/tcp-sig))

The @web-server is a complicated piece of software and as a result,
defines a number of interesting and independently useful sub-components.
Some of these are documented here.

@local-table-of-contents[]


@; ------------------------------------------------------------
@section[#:tag "timer.ss"]{Timers}
@(require (for-label web-server/private/timer))

@defmodule[web-server/private/timer]{

@filepath{private/timer.ss} provides a functionality for running
procedures after a given amount of time, that may be extended.

@defstruct[timer ([evt evt?]
                  [expire-seconds number?]
                  [action (-> void)])]{
 @scheme[evt] is an @scheme[alarm-evt] that is ready at @scheme[expire-seconds].
 @scheme[action] should be called when this @scheme[evt] is ready.
}

@defproc[(start-timer-manager)
         void]{
 Handles the execution and management of timers.
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

}

@; ------------------------------------------------------------
@section[#:tag "connection-manager.ss"]{Connection Manager}
@(require (for-label web-server/private/connection-manager))

@defmodule[web-server/private/connection-manager]{

@filepath{private/connection-manager.ss} provides functionality for managing pairs of
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
 Calls @scheme[reset-timer!] with the timer behind @scheme[c] with @scheme[t].
}
              
}

@; ------------------------------------------------------------
@section[#:tag "dispatch-server-unit.ss"]{Dispatching Server}
@(require (for-label web-server/private/dispatch-server-unit)
          (for-label web-server/private/dispatch-server-sig)
          (for-label web-server/web-server-sig))

The @web-server is just a configuration of a dispatching server.
This dispatching server component is useful on its own.

@subsection{Dispatching Server Signatures}

@defmodule[web-server/private/dispatch-server-sig]{

The @schememodname[web-server/private/dispatch-server-sig] library
provides two signatures.

@defsignature[dispatch-server^ ()]{

The @scheme[dispatch-server^] signature is an alias for
@scheme[web-server^].

 @defproc[(serve) (-> void)]{
  Runs the server and returns a procedure that shuts down the server.
 }

 @defproc[(serve-ports [ip input-port?]
                       [op output-port?])
          void]{
 Serves a single connection represented by the ports @scheme[ip] and
 @scheme[op].
 }
}

@defsignature[dispatch-server-config^ ()]{

 @defthing[port port-number?]{Specifies the port to serve on.}
 @defthing[listen-ip (or/c string? false/c)]{Passed to @scheme[tcp-listen].}
 @defthing[max-waiting integer?]{Passed to @scheme[tcp-accept].}
 @defthing[initial-connection-timeout integer?]{Specifies the initial timeout given to a connection.}
 @defproc[(read-request [c connection?]
                        [p port-number?]
                        [port-addresses 
                         (input-port? . -> . (values string? string?))])
          (values any/c boolean?)]{
  Defines the way the server reads requests off connections to be passed
  to @scheme[dispatch].
 }
 @defthing[dispatch (-> connection? any/c void)]{How to handle requests.}
}

}

@subsection{Dispatching Server Unit}

@defmodule[web-server/private/dispatch-server-unit]{

The @schememodname[web-server/private/dispatch-server-unit] module
provides the unit that actually implements a dispatching server.

@defthing[dispatch-server@ (unit/c (import tcp^ dispatch-server-config^) 
                                   (export dispatch-server^))]{
 Runs the dispatching server config in a very basic way, except that it uses
 @secref["connection-manager.ss"] to manage connections.
}

}

@subsection{Threads and Custodians}

The dispatching server runs in a dedicated thread. Every time a connection is initiated, a new thread is started to handle it.
Connection threads are created inside a dedicated custodian that is a child of the server's custodian. When the server is used to
provide servlets, each servlet also receives a new custodian that is a child of the server's custodian @bold{not} the connection
custodian.

@; ------------------------------------------------------------
@section[#:tag "closure.ss"]{Serializable Closures}
@(require (for-label web-server/private/closure)
          (for-label web-server/private/define-closure))

@defmodule[web-server/private/closure]{

The defunctionalization process of the Web Language (see @secref["stateless-servlets"])
requires an explicit representation of closures that is serializable.
@filepath{private/closure.ss} is this representation. It provides:

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
 Extracts the unique tag of a closure @scheme[c].
}
                 
}
                 
These are difficult to use directly, so @filepath{private/define-closure.ss}
defines a helper form:

@subsection[#:style 'hidden]{Define Closure}
@defmodule[web-server/private/define-closure]{

@defform[(define-closure tag formals (free-vars ...) body)]{
 Defines a closure, constructed with @scheme[make-tag] that accepts closure that returns
 @scheme[freevars ...], that when invoked with @scheme[formals]
 executes @scheme[body].
}

Here is an example:
@schememod[
 scheme
(require scheme/serialize)

(define-closure foo (a b) (x y)
  (+ (- a b)
     (* x y)))

(define f12 (make-foo (lambda () (values 1 2))))
(serialize f12)
#,(schemeresult '((1) 1 (('page . foo:deserialize-info)) 0 () () (0 1 2)))
(f12 6 7)
#,(schemeresult 1)
(f12 9 1)
#,(schemeresult 10)

(define f45 (make-foo (lambda () (values 4 5))))
(serialize f45)
#,(schemeresult '((1) 1 (('page . foo:deserialize-info)) 0 () () (0 4 5)))
(f45 1 2)
#,(schemeresult 19)
(f45 8 8)
#,(schemeresult 20)
]

}

@; ------------------------------------------------------------
@section[#:tag "cache-table.ss"]{Cache Table}
@(require (for-label web-server/private/cache-table))

@defmodule[web-server/private/cache-table]{

@filepath{private/cache-table.ss} provides a set of caching hash table
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

}

@; ------------------------------------------------------------
@section[#:tag "mime-types.ss"]{MIME Types}
@(require (for-label web-server/private/mime-types))

@defmodule[web-server/private/mime-types]{

@filepath{private/mime-types.ss} provides function for dealing with @filepath{mime.types}
files.

@defproc[(read-mime-types [p path-string?])
         (hash-table/c symbol? bytes?)]{
 Reads the @filepath{mime.types} file from @scheme[p] and constructs a
 hash table mapping extensions to MIME types.
}

@defproc[(make-path->mime-type [p path-string?])
         (path? . -> . bytes?)]{
 Uses a @scheme[read-mime-types] with @scheme[p] and constructs a
 function from paths to their MIME type.
}
                               
}

@; ------------------------------------------------------------
@section[#:tag "mod-map.ss"]{Serialization Utilities}
@(require (for-label web-server/private/mod-map))

@defmodule[web-server/private/mod-map]{

The @schememodname[scheme/serialize] library provides the
functionality of serializing values. @filepath{private/mod-map.ss}
compresses the serialized representation.

@defproc[(compress-serial [sv list?])
         list?]{
 Collapses multiple occurrences of the same module in the module
 map of the serialized representation, @scheme[sv].
}

@defproc[(decompress-serial [csv list?])
         list?]{
 Expands multiple occurrences of the same module in the module
 map of the compressed serialized representation, @scheme[csv].
}
               
}

@; ------------------------------------------------------------
@section[#:tag "url-param.ss"]{URL Param}
@(require (for-label web-server/private/url-param))

@defmodule[web-server/private/url-param]{

The @web-server needs to encode information in URLs. If this data
is stored in the query string, than it will be overridden by browsers that
make GET requests to those URLs with more query data. So, it must be encoded
in URL params. @filepath{private/url-param.ss} provides functions for helping
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

}

@; ------------------------------------------------------------
@section[#:tag "gzip.ss"]{GZip}
@(require (for-label web-server/private/gzip
                     file/gzip
                     file/gunzip))

@defmodule[web-server/private/gzip]{

The @web-server provides a thin wrapper around @schememodname[file/gzip] and @schememodname[file/gunzip].

@defproc[(gzip/bytes [ib bytes?])
         bytes?]{
 GZips @scheme[ib] and returns the result.
}
                
@defproc[(gunzip/bytes [ib bytes?])
         bytes?]{
 GUnzips @scheme[ib] and returns the result.
}

}

@; ------------------------------------------------------------
@section[#:tag "util.ss"]{Miscellaneous Utilities}
@(require (for-label web-server/private/util))

@defmodule[web-server/private/util]

There are a number of other miscellaneous utilities the @web-server
needs. They are provided by @filepath{private/util.ss}.

@subsection{Contracts}
@defthing[non-empty-string/c contract?]{Contract for non-empty strings.}
@defthing[port-number? contract?]{Equivalent to @scheme[(between/c 1 65535)].}
@defthing[path-element? contract?]{Equivalent to @scheme[(or/c path-string? (symbols 'up 'same))].}

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

@defproc[(url-path->string [url-path (listof path/param?)])
         string?]{
 Formats @scheme[url-path] as a string with @scheme["/"] as a delimiter
 and no params.
}

@subsection{Paths}
@defproc[(explode-path* [p path-string?])
         (listof path-element?)]{
 Like @scheme[normalize-path], but does not resolve symlinks.
}

@defproc[(path-without-base [base path-string?]
                            [p path-string?])
         (listof path-element?)]{
 Returns, as a list, the portion of @scheme[p] after @scheme[base],
 assuming @scheme[base] is a prefix of @scheme[p].
}

@defproc[(directory-part [p path-string?])
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

@subsection{Bytes}

@defproc[(bytes-ci=? [b1 bytes?] [b2 bytes?]) boolean?]{
 Compares two bytes case insensitively.
}
                                                        
@defproc[(read/bytes [b bytes?])
         serializable?]{
 @scheme[read]s a value from @scheme[b] and returns it.
}

@defproc[(write/bytes [v serializable?])
         bytes?]{
 @scheme[write]s @scheme[v] to a bytes and returns it.
}
