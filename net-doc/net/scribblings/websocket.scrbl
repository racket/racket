#lang scribble/doc
@(require "common.rkt"
          scribble/bnf
          (for-label net/url unstable/contract web-server/http racket/list
                     racket/async-channel
                     (prefix-in raw: (for-label net/tcp-unit))
                     net/websocket
                     net/websocket/client
                     net/websocket/server
                     net/websocket/conn))

@title[#:tag "websocket"]{WebSocket}

@defmodule[net/websocket]

The @racketmodname[net/websocket] library provides
utilities to run and communicate with WebSocket servers,
as specified in @link["http://www.whatwg.org/specs/web-socket-protocol/"]{the WebSocket protocol} IETF draft
as of August 16th, 2010.

This module provides the exports from @racketmodname[net/websocket/client] and @racketmodname[net/websocket/server].
 
@section{Client API}

@defmodule[net/websocket/client]

@defproc[(ws-url? [x any/c]) boolean?]{ Returns true if @racket[x] is a @racket[url?] and has a @racket[url-scheme] equal to @litchar["ws"] or @litchar["wss"]. }

@defproc[(wss-url? [x any/c]) boolean?]{ Returns true if @racket[x] is a @racket[url?] and has a @racket[url-scheme] equal to @litchar["wss"]. }

@defproc[(ws-connect [u ws-url?]
                     [#:headers headers (listof header?) empty])
         open-ws-conn?]{
 Connects to the WebSocket server specified by @racket[u], providing @racket[headers] as additional headers.
 Returns the connection handle.
}

This module also provides the exports from @racketmodname[net/websocket/conn].

@section{Server API}

@defmodule[net/websocket/server]

@defproc[(ws-serve [conn-handle (open-ws-conn? any/c . -> . void)]
                   [#:conn-headers 
                    conn-headers
                    (bytes? (listof header?) . -> . (values (listof header?) any/c))
                    (λ (b hs) (values empty (void)))]
                   [#:tcp@ tcp@ (unit/c (import) (export tcp^)) raw:tcp@]
                   [#:port port tcp-listen-port? 80]
                   [#:listen-ip listen-ip (or/c string? false/c) #f]
                   [#:max-waiting max-waiting integer? 4]
                   [#:timeout timeout integer? (* 60 60)]
                   [#:confirmation-channel
                    confirm-ch
                    (or/c false/c async-channel?)
                    #f])
         (-> void)]{
 
 Starts a WebSocket server where each new connection uses @racket[conn-headers] to compute
 what headers the client receives based on the client's request line and headers. @racket[conn-headers]
 also returns a piece of state that will be passed to @racket[conn-handle] as its second argument.
 After the connection handshake is finished, @racket[conn-handle] receives the connection and is in
 sole control until the WebSocket connection completes.
 
 All other arguments are used as in a @secref["dispatch-server-unit" #:doc '(lib "web-server/scribblings/web-server-internal.scrbl")]. Similarly, the return result is a function that shuts down the server, just like a dispatch server.

 The @racket[#:tcp@] keyword is provided for building an SSL server.
}
       
This module also provides the exports from @racketmodname[net/websocket/conn].

@section{Connections}

@defmodule[net/websocket/conn]

WebSocket connection are synchronizable events.

@defparam[framing-mode mode (symbols 'old 'new)]{ Controls whether framing is as before August 16th, 2010 or after. (Most Web browsers currently support only @racket['old] and they are incompatible, so you must choose the correct one.) Defaults to @racket['old].}

@defproc[(ws-conn? [x any/c]) boolean?]{ Returns true if @racket[x] is a WebSocket connection. }

@defproc[(open-ws-conn? [x any/c]) boolean?]{ Returns true if @racket[x] is an open WebSocket connection. }

@defproc[(ws-conn-line [ws ws-conn?]) bytes?]{ Returns the request/response line of the WebSocket connection. }
@defproc[(ws-conn-closed? [ws ws-conn?]) boolean?]{ Returns true if the WebSocket connection has been closed. }
@defproc[(ws-conn-headers [ws ws-conn?]) (listof header?)]{ Returns the headers of the WebSocket connection. }

WebSocket connection support only blocking calls:

@defproc[(ws-send! [ws open-ws-conn?] [s string?]) void]{ Sends @racket[s] over @racket[ws]. }
@defproc[(ws-recv [ws open-ws-conn?]) (or/c string? eof-object?)]{ Receives a string from @racket[ws]. Returns @racket[eof] if the other end closes the connection. }

@defproc[(ws-close! [ws open-ws-conn?]) void]{ Closes @racket[ws]. }

@section{Example}

This is a WebSocket echo server compatible with the browser origin security model:

@racketblock[
(ws-serve 
 #:port 8080
 (λ (wsc _)
   (let loop ()
     (define m (ws-recv wsc))
     (printf "~a\n" m)
     (unless (eof-object? m)
       (ws-send! wsc m)
       (loop))))
 #:conn-headers 
 (λ (_ hs)
   (define origin 
     (header-value (headers-assq* #"Origin" hs)))
   (values 
    (list
     (make-header #"Sec-WebSocket-Origin" origin)
     (make-header #"Sec-WebSocket-Location"
                  #"ws://localhost:8080/"))
    #f)))
]
