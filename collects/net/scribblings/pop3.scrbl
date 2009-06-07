#lang scribble/doc
@(require "common.ss"
          (for-label net/pop3
                     net/pop3-unit
                     net/pop3-sig))

@(define pt (tt ">"))

@title[#:tag "pop3"]{POP3: Reading Mail}

@defmodule[net/pop3]{The @schememodname[net/pop3] module provides
tools for the Post Office Protocol version 3 @cite["RFC977"].}

@defstruct[communicator ([sender output-port?]
                         [receiver input-port?]
                         [server string?]
                         [port (integer-in 0 65535)]
                         [state (one-of/c 'disconnected 'authorization 'transaction)])]{

Once a connection to a POP-3 server has been established, its state is
stored in a @scheme[communicator] instance, and other procedures take
@scheme[communicator] instances as an argument.}


@defproc[(connect-to-server [server string?]
                            [port-number (integer-in 0 65535) 110])
         communicator?]{

Connects to @scheme[server] at @scheme[port-number].}


@defproc[(disconnect-from-server [communicator communicator?])
         void?]{

Disconnects @scheme[communicator] from the server, and sets
@scheme[communicator]'s state to @scheme['disconnected].}


@defproc[(authenticate/plain-text [user string?] [passwd string?]       
                                  [communicator communicator?])
          void?]{

Authenticates using @scheme[user] and @scheme[passwd]. If
authentication is successful, @scheme[communicator]'s state is set to
@scheme['transaction].}

@defproc[(get-mailbox-status [communicator communicator?])
         (values  exact-nonnegative-integer? exact-nonnegative-integer?)]{

Returns the number of messages and the number of octets in the
mailbox.}

@defproc[(get-message/complete [communicator communicator?]
                               [message-number exact-integer?])
         (values (listof string?) (listof string?))]{

Given a message number, returns a list of message-header lines and
list of message-body lines.}

@defproc[(get-message/headers [communicator communicator?]
                              [message-number exact-integer?])
         (values (listof string?) (listof string?))]{

Given a message number, returns a list of message-header lines.}

@defproc[(get-message/body [communicator communicator?]
                           [message-number exact-integer?])
         (values (listof string?) (listof string?))]{

Given a message number, returns a list of message-body lines.}

@defproc[(delete-message [communicator communicator?]
                         [message-number exact-integer?])
         void?]{

Deletes the specified message.}

@defproc[(get-unique-id/single [communicator communicator?]
                               [message-number exact-integer?])
         string?]{

Gets the server's unique id for a particular message.}


@defproc[(get-unique-id/all [communicator communicator?])
         (listof (cons/c exact-integer? string?))]{

Gets a list of unique id's from the server for all the messages in the
mailbox. The @scheme[car] of each item in the result list is the
message number, and the @scheme[cdr] of each item is the message's
id.}

@defproc[(make-desired-header [tag-string string?])
         regexp?]{

Takes a header field's tag and returns a regexp to match the field}

@defproc[(extract-desired-headers [header (listof string?)]
                                  [desireds (listof regexp?)])
         (listof string?)]{

Given a list of header lines and of desired regexps, returns the
header lines that match any of the @scheme[desireds].}

@; ----------------------------------------

@section[#:tag "pop3-exns"]{Exceptions}

@defstruct[(pop3 exn) ()]{

The supertype of all POP3 exceptions.}

@defstruct[(cannot-connect pop3) ()]{

Raised when a connection to a server cannot be established.}

@defstruct[(username-rejected pop3) ()]{

Raised if the username is rejected.}

@defstruct[(password-rejected pop3) ()]{

Raised if the password is rejected.}

@defstruct[(not-ready-for-transaction pop3) ([communicator communicator?])]{

Raised when the communicator is not in transaction mode.}

@defstruct[(not-given-headers pop3) ([communicator communicator?] 
                                     [message exact-integer?])]{

Raised when the server does not respond with headers for a message as
requested.}

@defstruct[(illegal-message-number pop3) ([communicator communicator?]
                                          [message exact-integer?])]{

Raised when the client specifies an illegal message number.}


@defstruct[(cannot-delete-message exn) ([communicator communicator?]
                                        [message exact-integer?])]{

Raised when the server is unable to delete a message.}

@defstruct[(disconnect-not-quiet pop3) ([communicator communicator?])]{

Raised when the server does not gracefully disconnect.}


@defstruct[(malformed-server-response pop3) ([communicator communicator?])]{

Raised when the server produces a mal-formed response.}

@section{Example Session}

@schemeblock[
 #,pt (require net/pop3)
 #,pt (define c (connect-to-server "cs.rice.edu"))
 #,pt (authenticate/plain-text "scheme" "********" c)
 #,pt (get-mailbox-status c)
 196
 816400
 #,pt (get-message/headers c 100)
 ("Date: Thu, 6 Nov 1997 12:34:18 -0600 (CST)"
  "Message-Id: <199711061834.MAA11961@new-world.cs.rice.edu>"
  "From: Shriram Krishnamurthi <shriram@cs.rice.edu>"
  ....
  "Status: RO")
 #,pt (get-message/complete  c 100)
 ("Date: Thu, 6 Nov 1997 12:34:18 -0600 (CST)"
  "Message-Id: <199711061834.MAA11961@new-world.cs.rice.edu>"
  "From: Shriram Krishnamurthi <shriram@cs.rice.edu>"
  ....
  "Status: RO")
 ("some body" "text" "goes" "." "here" "." "")
 #,pt (get-unique-id/single c 205)
 @#,schemeerror{no message numbered 205 available for unique id}
 #,pt (list-tail (get-unique-id/all c) 194)
 ((195 . "e24d13c7ef050000") (196 . "3ad2767070050000"))
 #,pt (get-unique-id/single c 196)
 "3ad2767070050000"
 #,pt (disconnect-from-server c)
]

@; ----------------------------------------

@section{POP3 Unit}

@defmodule[net/pop3-unit]

@defthing[pop3@ unit?]{

Imports nothing, exports @scheme[pop3^].}

@; ----------------------------------------

@section{POP3 Signature}

@defmodule[net/pop3-sig]

@defsignature[pop3^ ()]{}

Includes everything exported by the @schememodname[net/pop3] module.
