#lang scribble/doc
@(require "common.rkt"
          (for-label net/tcp-sig net/tcp-unit net/url-unit net/tcp-redirect
                     net/ssl-tcp-unit racket/tcp))

@title[#:tag "tcp"]{TCP: Unit and Signature}

The @racketmodname[net/tcp-sig] and @racketmodname[net/tcp-unit]
libraries define a @racket[tcp^] signature and @racket[tcp@]
implementation, where the implementation uses
@racketmodname[racket/tcp].

Some units in the @filepath{net} collection import @racket[tcp^], so
that they can be used with transports other than plain TCP. For
example, @racket[url@] imports @racket[tcp^].

See also @racket[tcp-redirect] and @racket[make-ssl-tcp@].

@section{TCP Signature}

@defmodule[net/tcp-sig]

@defsignature[tcp^ ()]{

@defproc[(tcp-listen [port-no (and/c exact-nonnegative-integer?
                                     (integer-in 0 65535))]
                     [max-allow-wait exact-nonnegative-integer? 4]
                     [reuse? any/c #f]
                     [hostname (or/c string? false/c) #f])
         @#,sigelem[tcp^ tcp-listener?]]{

Like @racket[tcp-listen] from @racketmodname[racket/tcp].}

@defproc[(tcp-connect [hostname string?]
                      [port-no (and/c exact-nonnegative-integer?
                                     (integer-in 1 65535))]
                      [local-hostname (or/c string? false/c) #f]
                      [local-port-no (or/c (and/c exact-nonnegative-integer?
                                                  (integer-in 1 65535))
                                           false/c)
                                     #f])
          (values input-port? output-port?)]{

Like @racket[tcp-connect] from @racketmodname[racket/tcp].}

@defproc[(tcp-connect/enable-break [hostname string?]
                      [port-no (and/c exact-nonnegative-integer?
                                     (integer-in 1 65535))]
                      [local-hostname (or/c string? false/c) #f]
                      [local-port-no (or/c (and/c exact-nonnegative-integer?
                                                  (integer-in 1 65535))
                                           false/c)])
          (values input-port? output-port?)]{

Like @racket[tcp-connect/enable-break] from @racketmodname[racket/tcp].}

@defproc[(tcp-accept [listener @#,sigelem[tcp^ tcp-listener?]])
         (values input-port? output-port?)]{

Like @racket[tcp-accept] from @racketmodname[racket/tcp].}

@defproc[(tcp-accept/enable-break [listener @#,sigelem[tcp^ tcp-listener?]])
         (values input-port? output-port?)]{

Like @racket[tcp-accept/enable-break] from @racketmodname[racket/tcp].}

@defproc[(tcp-accept-ready? [listener @#,sigelem[tcp^ tcp-listener?]]) boolean?]{

Like @racket[tcp-accept-ready?] from @racketmodname[racket/tcp].}

@defproc[(tcp-close [listener @#,sigelem[tcp^ tcp-listener?]]) void?]{

Like @racket[tcp-close] from @racketmodname[racket/tcp].}

@defproc[(tcp-listener? [v any/c]) boolean?]{

Like @racket[tcp-listener?] from @racketmodname[racket/tcp].}

@defproc[(tcp-abandon-port [tcp-port port?]) void?]{

Like @racket[tcp-abandon-port] from @racketmodname[racket/tcp].}

@defproc[(tcp-addresses [tcp-port port?]
                        [port-numbers? any/c #f])
         (or/c (values string? string?)
               (values string? (integer-in 1 65535)
                       string? (integer-in 1 65535)))]{

Like @racket[tcp-addresses] from @racketmodname[racket/tcp].}

}

@section{TCP Unit}

@defmodule[net/tcp-unit]

@defthing[tcp@ unit?]{

Imports nothing and exports @racket[tcp^], implemented using
@racketmodname[racket/tcp].}
