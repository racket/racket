#lang scribble/doc
@(require "web-server.ss")

@(require 
  (for-label
   web-server/stuffers/stuffer
   web-server/stuffers/base64
   web-server/stuffers/gzip
   web-server/stuffers/hash
   web-server/stuffers/serialize
   web-server/stuffers/store
   (only-in web-server/lang/stuff-url
            default-stuffer
            make-default-stuffer
            is-url-too-big?)))

@title[#:tag "stuffers.ss"]{Stuffers}

@defmodule[web-server/stuffers]

The @schememod[web-server] module language provides serializable continuations. 
The serialization functionality is abstracted into @deftech{stuffers} that control how it operates.
You can supply your own (built with these functions) when you write a stateless servlet.

@section{Basic Combinators}

@defmodule[web-server/stuffers/stuffer]{
                                        
@defstruct[stuffer ([in (any/c . -> . any/c)]
                    [out (any/c . -> . any/c)])]{

 A @tech{stuffer} is essentially an invertible function captured in this structure.
 The following should hold:
 @schemeblock[
  (out (in x)) = x
  (in (out x)) = x
 ]                                               
}
                                                
@defproc[(stuffer/c [dom any/c] [rng any/c])
         contract?]{
 Constructs a contract for a @tech{stuffer} where @scheme[in] has
 the contract @scheme[(dom . -> . rng)] and @scheme[out] has the contract
 @scheme[(rng . -> . dom)].
}
                   
@defthing[id-stuffer (stuffer/c any/c any/c)]{
 The identitiy @tech{stuffer}.
}

@defproc[(stuffer-compose [g (stuffer any/c any/c)]
                          [f (stuffer any/c any/c)])
         (stuffer any/c any/c)]{
 Composes @scheme[f] and @scheme[g], i.e., applies @scheme[f] then @scheme[g] for @scheme[in]
 and @scheme[g] then @scheme[f] for @scheme[out].
}
                               
@defproc[(stuffer-sequence [f (stuffer any/c any/c)]
                           [g (stuffer any/c any/c)])
         (stuffer any/c any/c)]{
 @scheme[stuffer-compose] with arguments swapped.
}
                               
@defproc[(stuffer-if [c (bytes? . -> . boolean?)]
                     [f (stuffer bytes? bytes?)])
         (stuffer bytes? bytes?)]{
 Creates a @tech{stuffer} that stuffs with @scheme[f] if @scheme[c] is true on the input
 to @scheme[in]. Similarly, applies @scheme[f] during @scheme[out] if it was applied during
 @scheme[in] (which is recorded by prepending a byte.)
}
                                 
@defproc[(stuffer-chain [x (or/c stuffer? (bytes? . -> . boolean?))]
                        ...)
         stuffer?]{
 Applies @scheme[stuffer-sequence] and @scheme[stuffer-if] to successive tails of @scheme[x].
}
                  
}

@section{Serialization}

@(require (for-label scheme/serialize
                     web-server/private/util))
@defmodule[web-server/stuffers/serialize]{
                                          
@defthing[serialize-stuffer (stuffer/c serializable? bytes?)]{
 A @tech{stuffer} that uses @scheme[serialize] and @scheme[write/bytes] and @scheme[deserialize] and @scheme[read/bytes].
}
                                          
}

@section{Base64 Encoding}

@(require (for-label net/base64))
@defmodule[web-server/stuffers/base64]{

@defthing[base64-stuffer (stuffer/c bytes? bytes?)]{
 A @tech{stuffer} that uses @scheme[base64-encode] and @scheme[base64-decode].
   
 Useful for getting URL-safe bytes.
}
         
}

@section{GZip Compression}

@(require (for-label file/gzip file/gunzip))
@defmodule[web-server/stuffers/gzip]{

@defthing[gzip-stuffer (stuffer/c bytes? bytes?)]{                                 
 A @tech{stuffer} that uses @schememodname[file/gzip] and @schememodname[file/gunzip].
   
 Note: You should probably compose this with @scheme[base64-stuffer] to get URL-safe bytes.
}
         
}

@section{Key/Value Storage}

The @schememodname[web-server/stuffers/hash] @tech{stuffers} rely on a key/value store.

@defmodule[web-server/stuffers/store]{
                                      
 @defstruct[store ([write (bytes? bytes? . -> . void)]
                   [read (bytes? . -> . bytes?)])]{
  The following should hold:
  @schemeblock[
   (begin (write k v) (read k)) = v
  ]
 }
                                                   
 @defproc[(dir-store [root path-string?])
          store?]{
  A store that stores key @scheme[key]'s value in a file located at @scheme[(build-path root (bytes->string/utf-8 key))].
 }

 It should be easy to use this interface to create store for databases, like SQLite, CouchDB, or BerkeleyDB.
}

@section{Hash-addressed Storage}

@(require (for-label file/md5))
@defmodule[web-server/stuffers/hash]{
                                     
 @defthing[hash/c contract?]{
  Equivalent to @scheme[(bytes? . -> . bytes?)].
 }
          
 @defproc[(hash-stuffer [H hash/c]
                        [store store?])
          (stuffer/c bytes? bytes?)]{
  A content-addressed storage @tech{stuffer} that stores input bytes, @scheme[input], in @scheme[store] with the key @scheme[(H input)]
  and returns the key. Similarly, on @scheme[out] the original bytes are looked up.
 }
                                    
 @defproc[(md5-stuffer [root path-string?])
          (stuffer/c bytes? bytes?)]{
  Equivalent to @scheme[(hash-stuffer md5 (dir-store root))]
 }
                                     
}

@section{Helpers}

@defmodule[web-server/lang/stuff-url]{
                                      
@defproc[(is-url-too-big? [v bytes?])
         boolean?]{
 Determines if stuffing @scheme[v] into the current servlet's URL would result in a URL that is too big for Internet Explorer.
 (@link["http://www.boutell.com/newfaq/misc/urllength.html"]{IE only supports URLs up to 2048 characters.}).
}
                  
@defproc[(make-default-stuffer [root path-string?])
         (stuffer/c serializable? bytes?)]{
 Constructs a @tech{stuffer} that serializes, then if the URL is too big, compresses (and base64-encodes), if the URL is still too big
 then it stores it in an MD5-indexed database rooted at @scheme[root].
                                           
 Equivalent to:
 @schemeblock[
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
 Equivalent to @scheme[(make-default-stuffer
                        (build-path (find-system-path 'home-dir) ".urls"))].
}

}