#lang scribble/doc
@(require "common.ss"
          (for-label net/tcp-sig
                     net/tcp-unit
                     net/url-unit
                     net/tcp-redirect
                     net/ssl-tcp-unit
                     scheme/tcp))

@title[#:tag "tcp"]{TCP: Unit and Signature}

The @schememodname[net/tcp-sig] and @schememodname[net/tcp-unit]
libraries define a @scheme[tcp^] signature and @scheme[tcp@]
implementation, where the implementation uses
@schememodname[scheme/tcp].

Some units in the @filepath{net} collection import @scheme[tcp^], so
that they can be used with transports other than plain TCP. For
example, @scheme[url@] imports @scheme[tcp^].

See also @scheme[tcp-redirect] and @scheme[make-ssl-tcp@].

@section{TCP Signature}

@defmodule[net/tcp-sig]

@defsignature[tcp^ ()]{

@defproc[(tcp-listen [port-no (and/c exact-nonnegative-integer?
                                     (integer-in 1 65535))]
                     [max-allow-wait exact-nonnegative-integer? 4]
                     [reuse? any/c #f]
                     [hostname (or/c string? false/c) #f]) 
         @#,sigelem[tcp^ tcp-listener?]]{

Like @scheme[tcp-listen] from @schememodname[scheme/tcp].}

@defproc[(tcp-connect [hostname string?]
                      [port-no (and/c exact-nonnegative-integer?
                                     (integer-in 1 65535))]
                      [local-hostname (or/c string? false/c) #f]
                      [local-port-no (or/c (and/c exact-nonnegative-integer?
                                                  (integer-in 1 65535))
                                           false/c)
                                     #f])
          (values input-port? output-port?)]{

Like @scheme[tcp-connect] from @schememodname[scheme/tcp].}

@defproc[(tcp-connect/enable-break [hostname string?]
                      [port-no (and/c exact-nonnegative-integer?
                                     (integer-in 1 65535))]
                      [local-hostname (or/c string? false/c) #f]
                      [local-port-no (or/c (and/c exact-nonnegative-integer?
                                                  (integer-in 1 65535))
                                           false/c)])
          (values input-port? output-port?)]{

Like @scheme[tcp-connect/enable-break] from @schememodname[scheme/tcp].}

@defproc[(tcp-accept [listener @#,sigelem[tcp^ tcp-listener?]])
         (values input-port? output-port?)]{

Like @scheme[tcp-accept] from @schememodname[scheme/tcp].}

@defproc[(tcp-accept/enable-break [listener @#,sigelem[tcp^ tcp-listener?]])
         (values input-port? output-port?)]{

Like @scheme[tcp-accept/enable-break] from @schememodname[scheme/tcp].}

@defproc[(tcp-accept-ready? [listener @#,sigelem[tcp^ tcp-listener?]]) boolean?]{

Like @scheme[tcp-accept-ready?] from @schememodname[scheme/tcp].}

@defproc[(tcp-close [listener @#,sigelem[tcp^ tcp-listener?]]) void?]{

Like @scheme[tcp-close] from @schememodname[scheme/tcp].}

@defproc[(tcp-listener? [v any/c]) boolean?]{

Like @scheme[tcp-listener?] from @schememodname[scheme/tcp].}

@defproc[(tcp-abandon-port [tcp-port port?]) void?]{

Like @scheme[tcp-abandon-port] from @schememodname[scheme/tcp].}

@defproc[(tcp-addresses [tcp-port port?]
                        [port-numbers? any/c #f]) 
         (or/c (values string? string?)
               (values string? (integer-in 1 65535) 
                       string? (integer-in 1 65535)))]{

Like @scheme[tcp-addresses] from @schememodname[scheme/tcp].}

}

@section{TCP Unit}

@defmodule[net/tcp-unit]

@defthing[tcp@ unit?]{

Imports nothing and exports @scheme[tcp^], implemented using
@schememodname[scheme/tcp].}
