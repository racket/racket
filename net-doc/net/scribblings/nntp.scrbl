#lang scribble/doc
@(require "common.rkt" (for-label net/nntp net/nntp-unit net/nntp-sig))

@title[#:tag "nntp"]{NNTP: Newsgroup Protocol}

@defmodule[net/nntp]{The @racketmodname[net/nntp] module provides
tools to access Usenet group via NNTP @cite["RFC977"].}

@section{Connection and Operations}

@defstruct[communicator ([sender output-port?]
                         [receiver input-port?]
                         [server string?]
                         [port (integer-in 0 65535)])]{

Once a connection to a Usenet server has been established, its state
is stored in a @racket[communicator], and other procedures take
communicators as an argument.}

@defproc[(connect-to-server [server string?] [port-number (integer-in 0 65535) 119])
         communicator?]{

Connects to @racket[server] at @racket[port-number].}

@defproc[(disconnect-from-server [communicator communicator?]) void?]{

Disconnects an NNTP communicator.}

@defproc[(open-news-group [communicator communicator?]
                          [newsgroup string?])
         (values exact-nonnegative-integer?
                 exact-nonnegative-integer?
                 exact-nonnegative-integer?)]{

Selects the newsgroup of an NNTP connection. The returned values are
the total number of articles in the group, the first available
article, and the last available article.}

@defproc[(authenticate-user [communicator communicator?]
                            [username string?]
                            [password string?])
          void?]{

Tries to authenticate a user with the original authinfo command (uses
cleartext). The @racket[password] argument is ignored if the server
does not ask for it.}

@defproc[(head-of-message [communicator communicator?]
                          [message-index exact-nonnegative-integer?])
         (listof string?)]{

Given a message number, returns its header lines.}

@defproc[(body-of-message [communicator communicator?]
                          [message-index exact-nonnegative-integer?])
         (listof string?)]{

Given a message number, returns the body of the message.}

@defproc[(newnews-since [communicator communicator?]
                        [message-index exact-nonnegative-integer?])
         (listof string?)]{

Implements the @tt{NEWNEWS} command (often disabled on servers).}

@defproc[((generic-message-command [command string?] 
                                   [ok-code exact-integer?])
          [communicator communicator?]
          [message-index exact-nonnegative-integer?])
         (listof string?)]{

Useful primitive for implementing @racket[head-of-message],
@racket[body-of-message] and other similar commands.}

@defproc[(make-desired-header [tag-string string?])
         regexp?]{

Takes a header field's tag and returns a regexp to match the field}

@defproc[(extract-desired-headers [header (listof string?)]
                                  [desireds (listof regexp?)])
         (listof string?)]{

Given a list of header lines and of desired regexps, returns the
header lines that match any of the @racket[desireds].}


@section{Exceptions}

@defstruct[(nntp exn) ()]{

The supertype of all NNTP exceptions.}

@defstruct[(unexpected-response nntp) ([code exact-integer?]
                                       [text string?])]{

Raised whenever an unexpected response code is received.  The
@racket[text] field holds the response text sent by the server.}

@defstruct[(bad-status-line nntp) ([line string?])]{

Raised for mal-formed status lines.}

@defstruct[(premature-close nntp) ([communicator communicator?])]{

Raised when a remote server closes its connection unexpectedly.}

@defstruct[(bad-newsgroup-line nntp) ([line string?])]{

Raised when the newsgroup line is improperly formatted.}

@defstruct[(non-existent-group nntp) ([group string?])]{

Raised when the server does not recognize the name of the requested
group.}

@defstruct[(article-not-in-group nntp) ([article exact-integer?])]{

Raised when an article is outside the server's range for that group.}

@defstruct[(no-group-selected nntp) ()]{

Raised when an article operation is used before a group has been
selected.}

@defstruct[(article-not-found nntp) ([article exact-integer?])]{

Raised when the server is unable to locate the article.}

@defstruct[(authentication-rejected nntp) ()]{

Raised when the server reject an authentication attempt.}

@; ----------------------------------------

@section{NNTP Unit}

@margin-note{@racket[nntp@] and @racket[nntp^] are deprecated.
They exist for backward-compatibility and will likely be removed in
the future. New code should use the @racketmodname[net/nntp] module.}

@defmodule[net/nntp-unit]

@defthing[nntp@ unit?]{

Imports nothing, exports @racket[nntp^].}

@; ----------------------------------------

@section{NNTP Signature}

@defmodule[net/nntp-sig]

@defsignature[nntp^ ()]{}

Includes everything exported by the @racketmodname[net/nntp] module.
