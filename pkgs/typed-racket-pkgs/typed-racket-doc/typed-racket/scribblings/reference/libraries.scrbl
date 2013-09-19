#lang scribble/manual

@begin[(require "../utils.rkt")
       (require (for-label (only-meta-in 0 [except-in typed/racket for])
                           (only-in racket/base for)
                           racket/list srfi/14 net/url
                           version/check))]


@title{Libraries Provided With Typed Racket}

The @racketmodname[typed/racket] language corresponds to the
@racketmodname[racket] language---that is, any identifier provided
by @racketmodname[racket], such as @racket[modulo] is available by default in
@racketmodname[typed/racket].

@racketmod[typed/racket
(modulo 12 2)
]

The @racketmodname[typed/racket/base] language corresponds to the
@racketmodname[racket/base] language.

Some libraries have counterparts in the @racketidfont{typed}
collection, which provide the same exports as the untyped versions.
Such libraries include @racketmodname[srfi/14],
@racketmodname[net/url], and many others.

@racketmod[typed/racket
(require typed/srfi/14)
(char-set= (string->char-set "hello")
           (string->char-set "olleh"))
]

Other libraries can be used with Typed Racket via
@racket[require/typed].

@racketmod[typed/racket
(require/typed version/check
               [check-version (-> (U Symbol (Listof Any)))])
(check-version)
]

The following libraries are included with Typed Racket in the
@racketfont{typed} collection:

@(define-syntax-rule @defmodule/incl[name]
   @defmodule[name #:no-declare])

@;; framework and mred left out until support for classes
@;; is more complete
@defmodule/incl[typed/file]
@defmodule/incl[typed/net/base64]
@defmodule/incl[typed/net/cgi]
@defmodule/incl[typed/net/cookie]
@defmodule/incl[typed/net/dns]
@defmodule/incl[typed/net/ftp]
@defmodule/incl[typed/net/gifwrite]
@defmodule/incl[typed/net/head]
@defmodule/incl[typed/net/imap]
@defmodule/incl[typed/net/mime]
@defmodule/incl[typed/net/nntp]
@defmodule/incl[typed/net/pop3]
@defmodule/incl[typed/net/qp]
@defmodule/incl[typed/net/sendmail]
@defmodule/incl[typed/net/sendurl]
@defmodule/incl[typed/net/smtp]
@defmodule/incl[typed/net/uri-codec]
@defmodule/incl[typed/net/url]
@defmodule/incl[typed/rackunit]
@defmodule/incl[typed/srfi/14]

Other libraries included in the main distribution that are either
written in Typed Racket or have adapter modules that are typed:

@(define-syntax-rule @defmodule/also[name]
   @defmodule[name #:no-declare #:link-target? #f #:indirect])

@defmodule/also[math]
@defmodule/also[plot/typed]

@section{Porting Untyped Modules to Typed Racket}

To adapt a Racket library not included with Typed Racket, the
following steps are required:

@itemlist[
  @item{Determine the data manipulated by the library, and how it will
  be represented in Typed Racket.}
  @item{Specify that data in Typed Racket, using @racket[require/typed]
  and @racket[#:opaque] and/or @racket[#:struct].}
  @item{Use the data types to import the various functions and constants
  of the library.}
  @item{Provide all the relevant identifiers from the new adapter module.}
]

For example, the following module adapts the untyped
@racketmodname[racket/bool] library:

@racketmod[typed/racket
  (require/typed racket/bool
                 [true Boolean]
                 [false Boolean]
                 [symbol=? (Symbol Symbol -> Boolean)]
                 [boolean=? (Boolean Boolean -> Boolean)]
                 [false? (Any -> Boolean)])
  (provide true false symbol=? boolean=? false?)
]

More substantial examples are available in the @racketfont{typed}
collection.

