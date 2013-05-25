#lang scribble/doc
@(require "web-server.rkt")

@(require 
  (for-label
   web-server/stuffers/stuffer
   web-server/stuffers/base64
   web-server/stuffers/gzip
   web-server/stuffers/hash
   web-server/stuffers/serialize
   web-server/stuffers/store
   web-server/stuffers/hmac-sha1
   (only-in web-server/lang/stuff-url
            default-stuffer
            make-default-stuffer
            is-url-too-big?)))

@title[#:tag "stuffers"]{Stuffers}

@defmodule[web-server/stuffers]

The @racketmodname[web-server] language provides serializable continuations. 
The serialization functionality is abstracted into @deftech{stuffers} that control how it operates.
You can supply your own (built with these functions) when you write a stateless servlet.

@section{Basic Combinators}

@defmodule[web-server/stuffers/stuffer]{

@defstruct[stuffer ([in (any/c . -> . any/c)]
                    [out (any/c . -> . any/c)])]{

 A @tech{stuffer} is essentially an invertible function captured in this structure.
 The following should hold:
 @racketblock[
  (out (in x)) = x
  (in (out x)) = x
 ]
}

@defproc[(stuffer/c [dom any/c] [rng any/c])
         contract?]{
 Constructs a contract for a @tech{stuffer} where @racket[in] has
 the contract @racket[(dom . -> . rng)] and @racket[out] has the contract
 @racket[(rng . -> . dom)].
}
                   
@defthing[id-stuffer (stuffer/c any/c any/c)]{
 The identitiy @tech{stuffer}.
}

@defproc[(stuffer-compose [g (stuffer/c any/c any/c)]
                          [f (stuffer/c any/c any/c)])
         (stuffer/c any/c any/c)]{
 Composes @racket[f] and @racket[g], i.e., applies @racket[f] then
 @racket[g] for @racket[in] and @racket[g] then @racket[f] for
 @racket[out].
}

@defproc[(stuffer-sequence [f (stuffer/c any/c any/c)]
                           [g (stuffer/c any/c any/c)])
         (stuffer/c any/c any/c)]{
 @racket[stuffer-compose] with arguments swapped.
}

@defproc[(stuffer-if [c (bytes? . -> . boolean?)]
                     [f (stuffer/c bytes? bytes?)])
         (stuffer/c bytes? bytes?)]{
 Creates a @tech{stuffer} that stuffs with @racket[f] if @racket[c] is
 true on the input to @racket[in]. Similarly, applies @racket[f] during
 @racket[out] if it was applied during @racket[in] (which is recorded by
 prepending a byte.)
}

@defproc[(stuffer-chain [x (or/c stuffer? (bytes? . -> . boolean?))]
                        ...)
         stuffer?]{
 Applies @racket[stuffer-sequence] and @racket[stuffer-if] to successive tails of @racket[x].
}
                  
}

@section{Serialization}

@(require (for-label racket/serialize
                     web-server/private/util))
@defmodule[web-server/stuffers/serialize]{

@defthing[serialize-stuffer (stuffer/c serializable? bytes?)]{
 A @tech{stuffer} that uses @racket[serialize] and @racket[write/bytes]
 and @racket[deserialize] and @racket[read/bytes].
}

}

@section{Base64 Encoding}

@(require (for-label net/base64))
@defmodule[web-server/stuffers/base64]{

@defthing[base64-stuffer (stuffer/c bytes? bytes?)]{
 A @tech{stuffer} that uses @racket[base64-encode] and @racket[base64-decode].
   
 Useful for getting URL-safe bytes.
}
         
}

@section{GZip Compression}

@(require (for-label file/gzip file/gunzip web-server/private/gzip))
@defmodule[web-server/stuffers/gzip]{

@defthing[gzip-stuffer (stuffer/c bytes? bytes?)]{
 A @tech{stuffer} that uses @racket[gzip/bytes] and @racket[gunzip/bytes].

 @warning{You should compose this with @racket[base64-stuffer] to get
 URL-safe bytes.}
}

}

@section{Key/Value Storage}

The @racketmodname[web-server/stuffers/hash] @tech{stuffers} rely on a
key/value store.

@defmodule[web-server/stuffers/store]{

 @defstruct[store ([write (bytes? bytes? . -> . void)]
                   [read (bytes? . -> . bytes?)])]{
  The following should hold:
  @racketblock[
   (begin (write k v) (read k)) = v
  ]
 }

 @defproc[(dir-store [root path-string?])
          store?]{
  A store that stores key @racket[key]'s value in a file located at 
  @racketblock[
  (build-path 
   root
   (bytes->string/utf-8 key))
  ]
 }

 It should be easy to use this interface to create store for databases like SQLite, CouchDB, or BerkeleyDB.
}

@section{Hash-addressed Storage}

@(require (for-label file/md5))
@defmodule[web-server/stuffers/hash]{

 @defthing[hash-fun/c contract?]{
  Equivalent to @racket[(bytes? . -> . bytes?)].
 }

 @defproc[(hash-stuffer [H hash-fun/c]
                        [store store?])
          (stuffer/c bytes? bytes?)]{
  A content-addressed storage @tech{stuffer} that stores input bytes,
  @racket[input], in @racket[store] with the key @racket[(H input)] and
  returns the key. Similarly, on @racket[out] the original bytes are
  looked up.
 }

 @defproc[(md5-stuffer [root path-string?])
          (stuffer/c bytes? bytes?)]{
  Equivalent to @racket[(hash-stuffer md5 (dir-store root))]
 }

}

@section{HMAC-SHA1 Signing}

@defmodule[web-server/stuffers/hmac-sha1]{

 @defproc[(HMAC-SHA1 [kb bytes?] [db bytes?])
          bytes?]{
  Performs a HMAC-SHA1 calculation on @racket[db] using @racket[kb] as
  the key. The result is guaranteed to be 20 bytes.  (You could curry
  this to use it with @racket[hash-stuffer], but there is little value
  in doing so over @racket[md5].)
 }

 @defproc[(HMAC-SHA1-stuffer [kb bytes?])
          (stuffer/c bytes? bytes?)]{
  A @tech{stuffer} that signs input using @racket[HMAC-SHA1] with
  @racket[kb] as the key. The result of the @tech{stuffer} is the hash
  prepended to the input data. When the @tech{stuffer} is run in
  reverse, it checks if the first 20 bytes are the correct has for the
  rest of the data.

  @warning{You should compose this with @racket[base64-stuffer] to get
  URL-safe bytes.}

  @warning{Without explicit provision, it is possible for users to
  modify the continuations they are sent through the other
  @tech{stuffers}.  This @tech{stuffer} allows the servlet to certify
  that stuffed data was truly generated by the servlet. Therefore, you
  @bold{should} use this if you are not using the
  @racket[hash-stuffer]s.}

  @warning{This @tech{stuffer} does @bold{not} encrypt the data in
  anyway, so users can still observe the stuffed values.}
 }

}

@section{Helpers}

@defmodule[web-server/lang/stuff-url]{

@defproc[(is-url-too-big? [v bytes?])
         boolean?]{
 Determines if stuffing @racket[v] into the current servlet's URL would
 result in a URL that is too big for Internet Explorer.
 (@link["http://www.boutell.com/newfaq/misc/urllength.html"]{IE only
 supports URLs up to 2048 characters.})
}

@defproc[(make-default-stuffer [root path-string?])
         (stuffer/c serializable? bytes?)]{
 Constructs a @tech{stuffer} that serializes, then if the URL is too
 big, compresses (and base64-encodes), if the URL is still too big then
 it stores it in an MD5-indexed database rooted at @racket[root].

 Equivalent to:
 @racketblock[
  (stuffer-chain
   serialize-stuffer
   is-url-too-big?
   (stuffer-chain
    gzip-stuffer 
    base64-stuffer)
   is-url-too-big?
   (md5-stuffer root))
 ]
}

@defthing[default-stuffer (stuffer/c serializable? bytes?)]{
 Equivalent to:
 @racketblock[
  (make-default-stuffer
   (build-path 
    (find-system-path 'home-dir)
    ".urls"))]
}

}
