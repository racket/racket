#lang scribble/doc
@(require "common.rkt" scribble/eval scribble/struct
          (for-label net/imap net/imap-unit net/imap-sig))

@(define (just-report)
  @elem{This operation does not communicate with the server. It merely reports
        the result of previous communication.})

@(define-syntax-rule (flag-table (section [sym0 flag0] [sym flag] ...) ...)
  (let ([spacer (hspace 1)]
        [to-flow (lambda (e)
                  (make-flow (list (make-paragraph (list e)))))])
   (make-table
    #f
    (append
     (list
      (list (to-flow spacer)
            (to-flow spacer)
            (to-flow spacer)
            (to-flow (italic "symbol"))
            (to-flow spacer)
            (to-flow (italic "IMAP flag"))))
     (let ([sec section])
       (list
         (list (to-flow spacer)
               (to-flow sec)
               (to-flow spacer)
               (to-flow (racket sym0))
               (to-flow spacer)
               (to-flow (racket flag0)))
         (list (to-flow spacer)
               (to-flow spacer)
               (to-flow spacer)
               (to-flow (racket sym))
               (to-flow spacer)
               (to-flow (racket flag)))
         ...))
     ...))))

@title[#:tag "imap"]{IMAP: Reading Mail}

@defmodule[net/imap]{The @racketmodname[net/imap] module provides
utilities for the client side of Internet Message Access Protocol
version 4rev1 @cite["RFC2060"].}

@; ----------------------------------------

@section{Connecting and Selecting Mailboxes}

@defproc[(imap-connection? [v any/c]) boolean?]{

Return @racket[#t] if @racket[v] is a IMAP-connection value (which is
opaque), @racket[#f] otherwise.}

@defproc[(imap-connect [server string?]
                       [username (or/c string? bytes?)]
                       [password (or/c string? bytes?)]
                       [mailbox (or/c string? bytes?)]
                       [#:tls? tls? any/c #f]
                       [#:try-tls? try-tls? any/c #t])
         (values imap-connection? exact-nonnegative-integer? exact-nonnegative-integer?)]{

Establishes an IMAP connection to the given server using the given
username and password, and selects the specified mailbox. If
@racket[tls?]  is true, a TLS connection is made to the server before
communicating using the IMAP protocol. If @racket[tls?] is @racket[#f]
but @racket[try-tls?] is true, then after the IMAP connection is
initially established, the connection is switched to a TLS connection
if the server supports it.

The first result value represents the connection.
The second and third return values indicate the total number of
messages in the mailbox and the number of recent messages (i.e.,
messages received since the mailbox was last selected), respectively.

See also @racket[imap-port-number].

A user's primary mailbox is always called
@racket["INBOX"]. (Capitalization doesn't matter for that mailbox
name.)

Updated message-count and recent-count values are available through
@racket[imap-messages] and @racket[imap-recent]. See also @racket[imap-new?] and
@racket[imap-reset-new!].}


@defparam[imap-port-number k (integer-in 0 65535)]{

A parameter that determines the server port number. The initial value
is @racket[143].}


@defproc[(imap-connect* [in input-port?]
                        [out output-port?]
                        [username (or/c string? bytes?)]
                        [password (or/c string? bytes?)]
                        [mailbox (or/c string? bytes?)]
                        [#:tls? tls? any/c #f]
                        [#:try-tls? try-tls? any/c #t])
         (values imap-connection? exact-nonnegative-integer? exact-nonnegative-integer?)]{

Like @racket[imap-connect], but given input and output ports (e.g.,
ports for an SSL session) instead of a server address.}


@defproc[(imap-disconnect [imap imap-connection?]) void?]{

Closes an IMAP connection. The close may fail due to a communication
error.}


@defproc[(imap-force-disconnect [imap imap-connection?]) void?]{

Closes an IMAP connection forcefully (i.e., without send a close
message to the server). A forced disconnect never fails.}


@defproc[(imap-reselect [imap imap-connection?]
                        [mailbox (or/c string? bytes?)])
         (values exact-nonnegative-integer? exact-nonnegative-integer?)]{

De-selects the mailbox currently selected by the connection and
selects the specified mailbox, returning the total and recent message
counts for the new mailbox. Expunge and message-state information is
removed.

Do not use this procedure to poll a mailbox to see whether there are
any new messages. Use @racket[imap-noop], @racket[imap-new?], and
@racket[imap-reset-new!]  instead.}


@defproc[(imap-examine [imap imap-connection?]
                       [mailbox (or/c string? bytes?)])
         (values exact-nonnegative-integer? exact-nonnegative-integer?)]{

Like @racket[imap-reselect], but the mailbox is selected as read-only.}

@; ----------------------------------------

@section{Selected Mailbox State}

@defproc[(imap-noop [imap imap-connection?])
         (values exact-nonnegative-integer? exact-nonnegative-integer?)]{

Sends a ``no-op'' message to the server, typically to keep the session
alive. As for many commands, the server may report message-state
updates or expunges, which are recorded in @racket[imap].

The return information is the same as for @racket[imap-reselect].}


@defproc[(imap-poll [imap imap-connection?]) void?]{

Does not send a request to the server, but checks for asynchronous
messages from the server that update the message count, to report
expunges, etc.}


@defproc[(imap-messages [imap imap-connection?]) exact-nonnegative-integer?]{

Returns the number of messages in the selected mailbox. The server can
update this count during most any interaction.

@just-report[]}


@defproc[(imap-recent [imap imap-connection?]) exact-nonnegative-integer?]{

Returns the number of ``recent'' messages in the currently selected
mailbox, as most recently reported by the server. The server can
update this count during most any interaction.

@just-report[]}


@defproc[(imap-unseen [imap imap-connection?]) (or/c exact-nonnegative-integer? #f)]{

Returns the number of ``unseen'' messages in the currently selected
mailbox, as most recently reported by the server. The server can
update this count during most any interaction. Old IMAP servers might
not report this value, in which case the result is @racket[#f].

@just-report[]}


@defproc[(imap-uidnext [imap imap-connection?]) (or/c exact-nonnegative-integer? #f)]{

Returns the predicted next uid for a message in the currently selected
mailbox, as most recently reported by the server. The server can
update this count during most any interaction. Old IMAP servers might
not report this value, in which case the result is @racket[#f].

@just-report[]}


@defproc[(imap-uidvalidity [imap imap-connection?]) (or/c exact-nonnegative-integer? #f)]{

Returns an id number that changes when all uids become invalid. The
server @emph{cannot} update this number during a session. Old IMAP
servers might not report this value, in which case the result is
@racket[#f].

@just-report[]}


@defproc[(imap-new? [imap imap-connection?]) boolean?]{

Returns @racket[#t] if the server has reported an increase in the
message count for the currently mailbox since the last call to
@racket[imap-reset-new!]. Selecting a mailbox implicitly calls
@racket[imap-reset-new!].

@just-report[]}


@defproc[(imap-reset-new! [imap imap-connection?]) void?]{

Resets the new flag for the session; see @racket[imap-new?].
This operation does not communicate with the server.}


@defproc[(imap-get-expunges [imap imap-connection?]) (listof exact-nonnegative-integer?)]{

Returns pending expunge notifications from the server for the selected
mailbox in terms of message positions (not uids), and clears the
pending notifications. The result list is sorted, ascending.

@just-report[]

The server can notify the client of newly deleted messages during most
other commands, but not asynchronously between commands. Furthermore,
the server cannot report new deletions during @racket[imap-get-messages] or
@racket[imap-store] operations.

Before calling any IMAP operation that works in terms of message
numbers, pending expunge notifications must be handled by calling
@racket[imap-get-expunges].}


@defproc[(imap-pending-expunges? [imap imap-connection?]) boolean?]{

Returns @racket[#f] if @racket[imap-get-expunges] would return an
empty list, @racket[#t] otherwise.}


@defproc[(imap-get-updates [imap imap-connection?])
         (listof (cons/c exact-nonnegative-integer?
                         (listof pair?)))]{

Returns information must like @racket[imap-get-messages], but includes
information reported asynchronously by the server (e.g., to notify a
client with some other client changes a message attribute).  Instead
of reporting specific requested information for specific messages, the
result is associates message positions to field-value association
lists. The result list is sorted by message position, ascending.

@just-report[] It also clears the update information from the
connection after reporting it.

When a server reports information that supersedes old reported
information for a message, or if the server reports that a message has
been deleted, then old information for the message is
dropped. Similarly, if @racket[imap-get-messages] is used to
explicitly obtain information, any redundant (or out-of-date)
information is dropped.

A client need not use @racket[imap-get-updates] ever, but accumulated
information for the connection consumes space.}


@defproc[(imap-pending-updates? [imap imap-connection?]) boolean?]{

Returns @racket[#f] if @racket[imap-get-updates] would return an 
list, @racket[#t] otherwise.}


@; ----------------------------------------

@section{Manipulating Messages}

@defproc[(imap-get-messages [imap imap-connection?]
                            [msg-nums (listof exact-nonnegative-integer?)]
                            [fields (listof (or/c 'uid
                                                  'header
                                                  'body
                                                  'flags))])
         (listof list?)]{

Downloads information for a set of messages. The @racket[msg-nums]
argument specifies a set of messages by their message positions (not
their uids). The @racket[fields] argument specifies the type of
information to download for each message. The available fields are:

@itemize[

  @item{@racket['uid] --- the value is an integer}

  @item{@racket['header] --- the value is a header (a string, but see
        @racketmodname[net/head])}

 @item{@racket['body] --- the value is a byte string, with
       CRLF-separated lines}

 @item{@racket['flags] --- the value is a list of symbols that
       correspond to IMAP flags; see @racket[imap-flag->symbol]}

]

The return value is a list of entry items in parallel to
@racket[msg-nums]. Each entry is itself a list containing value items
in parallel to @racket[fields].

Pending expunges must be handled before calling this function; see
@racket[imap-get-expunges].

@examples[
(eval:alts (imap-get-message imap '(1 3 5) '(uid header))
           '((107 #"From: larry@stooges.com ...")
             (110 #"From: moe@stooges.com ...")
             (112 #"From: curly@stooges.com ...")))
]}

@deftogether[(
@defproc[(imap-flag->symbol [flag symbol?]) symbol?]
@defproc[(symbol->imap-flag [sym symbol?]) symbol?]
)]{

An IMAP flag is a symbol, but it is generally not a convenient one to
use within a Racket program, because it usually starts with a
backslash. The @racket[imap-flag->symbol] and
@racket[symbol->imap-flag] procedures convert IMAP flags to convenient
symbols and vice-versa:

@flag-table[
("message flags:"
     ['seen         '|\Seen|]
     ['answered     '|\Answered|]
     ['flagged      '|\Flagged|]
     ['deleted      '|\Deleted|]
     ['draft        '|\Draft|]
     ['recent       '|\Recent|]
)
("mailbox flags:"
     ['noinferiors   '|\Noinferiors|]
     ['noselect      '|\Noselect|]
     ['marked        '|\Marked|]
     ['unmarked      '|\Unmarked|]
     ['hasnochildren '|\HasNoChildren|]
     ['haschildren   '|\HasChildren|]
)
]

The @racket[imap-flag->symbol] and @racket[symbol->imap-flag]
functions act like the identity function when any other symbol is
provided.}


@defproc[(imap-store [imap imap-connection?]
                     [mode (or/c '+ '- '!)]
                     [msg-nums (listof exact-nonnegative-integer?)]
                     [imap-flags (listof symbol?)])
         void?]{

Sets flags for a set of messages. The mode argument specifies how
flags are set:

@itemize[

 @item{@racket['+] --- add the given flags to each message}

 @item{@racket['-] --- remove the given flags from each message}

 @item{@racket['!] --- set each message's flags to the given set}

]

The @racket[msg-nums] argument specifies a set of messages by their
message positions (not their uids). The @racket[flags] argument
specifies the imap flags to add/remove/install.

Pending expunges must be handled before calling this function; see
@racket[imap-get-expunges]. The server will not report back
message-state changes (so they will not show up through
@racket[imap-get-updates]).

@examples[
(eval:alts (imap-store imap '+ '(1 2 3) (list (symbol->imap-flag 'deleted)))
           (void))
(code:comment @#,t{marks the first three messages to be deleted})
(eval:alts (imap-expunge imap) (void))
(code:comment @#,t{permanently removes the first three messages (and possibly})
(code:comment @#,t{others) from the currently-selected mailbox})
]}


@defproc[(imap-expunge [imap imap-connection?]) void?]{

Purges every message currently marked with the @racket['|\Deleted|]
flag from the mailbox.}

@; ----------------------------------------

@section{Querying and Changing (Other) Mailboxes}

@defproc[(imap-copy [imap imap-connection?]
                    [msg-nums (listof exact-nonnegative-integer?)]
                    [dest-mailbox (or/c string? bytes?)])
          void?]{

Copies the specified messages from the currently selected mailbox to
the specified mailbox.

Pending expunges must be handled before calling this function; see
@racket[imap-get-expunges].}


@defproc[(imap-append [imap imap-connection?]
                      [mailbox string?]
                      [message (or/c string? bytes?)]
                      [flags (listof (or/c 'seen 'answered 'flagged 
                                           'deleted 'draft 'recent)) 
                             '(seen)])
         void?]{

Adds a new message (containing @racket[message]) to the given
mailbox.}


@defproc[(imap-status [imap imap-connection?]
                      [mailbox (or/c string? bytes?)]
                      [statuses (listof symbol?)])
         list?]{

Requests information about a mailbox from the server, typically
@emph{not} the currently selected mailbox.

The @racket[statuses] list specifies the request, and the return value
includes one value for each symbol in @racket[statuses]. The allowed
status symbols are:

@itemize[

   @item{@racket['messages] --- number of messages}
   @item{@racket['recent] --- number of recent messages}
   @item{@racket['unseen] --- number of unseen messages}
   @item{@racket['uidnext] --- uid for next received message}
   @item{@racket['uidvalidity] --- id that changes when all uids are changed}

]

Use @racket[imap-messages] to get the message count for the currently
selected mailbox, etc. Use @racket[imap-new?] and
@racket[imap-reset-new!] to detect when new messages are available in
the currently selected mailbox.}


@defproc[(imap-mailbox-exists? [imap imap-connection?]
                               [mailbox (or/c string? bytes?)])
         boolean?]{

Returns @racket[#t] if @racket[mailbox] exists, @racket[#f]
otherwise.}


@defproc[(imap-create-mailbox [imap imap-connection?]
                              [mailbox (or/c string? bytes?)])
         void?]{

Creates @racket[mailbox]. (It must not exist already.)}


@defproc[(imap-list-child-mailboxes [imap imap-connection?]
                                    [mailbox (or/c string? bytes? #f)]
                                    [delimiter (or/c string? bytes?)
                                               (imap-get-hierarchy-delimiter)])
         (listof (list/c (listof symbol?) bytes?))]{

Returns information about sub-mailboxes of @racket[mailbox]; if
@racket[mailbox] is @racket[#f], information about all top-level
mailboxes is returned. The @racket[delimiter] is used to parse mailbox
names from the server to detect hierarchy.

The return value is a list of mailbox-information lists. Each
mailbox-information list contains two items:

@itemize[

 @item{a list of imap flags for the mailbox}
 
 @item{the mailbox's name}

]}


@defproc[(imap-get-hierarchy-delimiter [imap imap-connection?]) bytes?]{

Returns the server-specific string that is used as a separator in
mailbox path names.}


@defproc[(imap-mailbox-flags [imap imap-connection?]
                             [mailbox (or/c string? bytes?)])
         (listof symbol?)]{

Returns a list of IMAP flags for the given mailbox. See also
@racket[imap-flag->symbol].}

@; ----------------------------------------

@section{IMAP Unit}

@margin-note{@racket[imap@] and @racket[imap^] are deprecated.
They exist for backward-compatibility and will likely be removed in
the future. New code should use the @racketmodname[net/imap] module.}

@defmodule[net/imap-unit]

@defthing[imap@ unit?]{

Imports nothing, exports @racket[imap^].}

@; ----------------------------------------

@section{IMAP Signature}

@defmodule[net/imap-sig]

@defsignature[imap^ ()]{}

Includes everything exported by the @racketmodname[net/imap] module.
