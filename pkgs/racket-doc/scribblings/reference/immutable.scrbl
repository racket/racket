#lang scribble/manual

@(require "mz.rkt"
          (for-label racket/immutable))

@title{Immutable Strings and Vectors}

@note-lib-only[racket/immutable]

@section{Immutable String Operations}

@defproc[(immutable-string? [v any/c]) boolean?]{
Returns @racket[#true] if @racket[v] is an immutable string, and @racket[#false]
otherwise.

@mz-examples[
  (require racket/immutable)
  (immutable-string? "Apple")
  (immutable-string? 'apple)
  (immutable-string? (make-string 5 #\a))
  (immutable-string? (make-immutable-string 5 #\a))
]}

@deftogether[[
  @defproc[(make-immutable-string [k exact-nonnegative-integer?]
                                  [char char? #\nul])
           immutable-string?]
  @defproc[(build-immutable-string [n exact-nonnegative-integer?]
                                   [proc (exact-nonnegative-integer? . -> . char?)])
           immutable-string?]
  @defproc[(immutable-string [char char?] ...) immutable-string?]
  @defproc[(immutable-substring [str immutable-string?] 
                                [start exact-nonnegative-integer?]
                                [end exact-nonnegative-integer? (string-length str)])
           immutable-string?]
  @defproc[(immutable-string-append [str immutable-string?] ...) immutable-string?]
  @defproc[(string-append* [str immutable-string?] ... [strs (listof immutable-string?)])
           immutable-string?]
  @defproc[(immutable-string-join [strs (listof immutable-string?)] [sep immutable-string? " "]
                                  [#:before-first before-first immutable-string? ""]
                                  [#:before-last  before-last  immutable-string? sep]
                                  [#:after-last   after-last   immutable-string? ""])
           immutable-string?]
  @defproc[(list->immutable-string [lst (listof char?)]) immutable-string?]
  @defproc[(number->immutable-string [z number?] [radix (or/c 2 8 10 16) 10])
           immutable-string?]
  @defproc[(symbol->immutable-string [sym symbol?]) immutable-string?]
  @defproc[(keyword->immutable-string [keyword keyword?]) immutable-string?]
  @defproc[(immutable-string-normalize-spaces [str immutable-string?]
                                              [sep (or/c immutable-string? regexp?) #px"\\s+"]
                                              [space immutable-string? " "]
                                              [#:trim? trim? any/c #t]
                                              [#:repeat? repeat? any/c #f])
           immutable-string?]
  @defproc[(immutable-string-replace [str  immutable-string?]
                                     [from (or/c immutable-string? regexp?)]
                                     [to   immutable-string?]
                                     [#:all? all? any/c #t])
           immutable-string?]
  @defproc[(immutable-string-split [str immutable-string?]
                                   [sep (or/c immutable-string? regexp?) #px"\\s+"]
                                   [#:trim? trim? any/c #t]
                                   [#:repeat? repeat? any/c #f])
           (listof immutable-string?)]
  @defproc[(immutable-string-trim [str immutable-string?]
                                  [sep (or/c immutable-string? regexp?) #px"\\s+"]
                                  [#:left? left? any/c #t]
                                  [#:right? right? any/c #t]
                                  [#:repeat? repeat? any/c #f])
           immutable-string?]
  @defproc[(immutable-string-upcase [str immutable-string?]) immutable-string?]
  @defproc[(immutable-string-downcase [str immutable-string?]) immutable-string?]
  @defproc[(immutable-string-titlecase [str immutable-string?]) immutable-string?]
  @defproc[(immutable-string-foldcase [str immutable-string?]) immutable-string?]
  @defproc[(immutable-string-normalize-nfd [str immutable-string?]) immutable-string?]
  @defproc[(immutable-string-normalize-nfkd [str immutable-string?]) immutable-string?]
  @defproc[(immutable-string-normalize-nfc [str immutable-string?]) immutable-string?]
  @defproc[(immutable-string-normalize-nfkc [str immutable-string?]) immutable-string?]
]]{
Versions of @racket[make-string], @racket[substring], @racket[string-append] etc.
that return immutable strings instead of mutable ones.
}

@section{Immutable Byte String Operations}

@defproc[(immutable-bytes? [v any/c]) boolean?]{
Returns @racket[#true] if @racket[v] is an immutable byte-string, and
@racket[#false] otherwise.

@mz-examples[
  (immutable-bytes? #"Apple")
  (immutable-bytes? 'apple)
  (immutable-bytes? "Apple")
  (immutable-bytes? (make-bytes 5 65))
  (immutable-bytes? (make-immutable-bytes 5 65))
]}

@deftogether[[
  @defproc[(make-immutable-bytes [k exact-nonnegative-integer?] [b byte? 0])
           immutable-bytes?]
  @defproc[(immutable-bytes [b byte?] ...) immutable-bytes?]
  @defproc[(immutable-subbytes [bstr immutable-bytes?]
                               [start exact-nonnegative-integer?]
                               [end exact-nonnegative-integer? (bytes-length str)])
           immutable-bytes?]
  @defproc[(immutable-bytes-append [bstr immutable-bytes?] ...) immutable-bytes?]
  @defproc[(immutable-bytes-append* [bstr immutable-bytes?] ... [bstrs (listof immutable-bytes?)])
           immutable-bytes?]
  @defproc[(immutable-bytes-join [bstrs (listof immutable-bytes?)] [sep immutable-bytes?])
           immutable-bytes?]
  @defproc[(list->immutable-bytes [lst (listof byte?)]) immutable-bytes?]
  @defproc[(bytes->immutable-string/utf-8 [bstr bytes?]
                                          [err-char (or/c #f char?) #f]
                                          [start exact-nonnegative-integer? 0]
                                          [end exact-nonnegative-integer? (bytes-length bstr)])
           immutable-string?]
  @defproc[(bytes->immutable-string/locale [bstr bytes?]
                                           [err-char (or/c #f char?) #f]
                                           [start exact-nonnegative-integer? 0]
                                           [end exact-nonnegative-integer? (bytes-length bstr)])
           immutable-string?]
  @defproc[(bytes->immutable-string/latin-1 [bstr bytes?]
                                            [err-char (or/c #f char?) #f]
                                            [start exact-nonnegative-integer? 0]
                                            [end exact-nonnegative-integer? (bytes-length bstr)])
           immutable-string?]
  @defproc[(string->immutable-bytes/utf-8 [str string?]
                                          [err-byte (or/c #f byte?) #f]
                                          [start exact-nonnegative-integer? 0]
                                          [end exact-nonnegative-integer? (string-length str)])
           immutable-bytes?]
  @defproc[(string->immutable-bytes/locale [str string?]
                                           [err-byte (or/c #f byte?) #f]
                                           [start exact-nonnegative-integer? 0]
                                           [end exact-nonnegative-integer? (string-length str)])
           immutable-bytes?]
  @defproc[(string->immutable-bytes/latin-1 [str string?]
                                            [err-byte (or/c #f byte?) #f]
                                            [start exact-nonnegative-integer? 0]
                                            [end exact-nonnegative-integer? (string-length str)])
           immutable-bytes?]
]]{
Versions of @racket[make-bytes], @racket[subbytes], @racket[bytes-append] etc.
that return immutable byte-strings instead of mutable ones.
}

@section{Immutable Vector Operations}

@defproc[(immutable-vector? [v any/c]) boolean?]{
Returns @racket[#true] if @racket[v] is an immutable vector, and @racket[#false]
otherwise.

@mz-examples[
  (immutable-vector? #(1 2 3))
  (immutable-vector? 'apple)
  (immutable-vector? (vector 1 2 3))
  (immutable-vector? (vector-immutable 1 2 3))
]}

@deftogether[[
  @defproc[(make-immutable-vector [size exact-nonnegative-integer?]
                                  [v any/c 0])
           immutable-vector?]
  @defproc[(build-immutable-vector [n exact-nonnegative-integer?]
                                   [proc (exact-nonnegative-integer? . -> . any/c)])
           immutable-vector?]
  @defproc[(list->immutable-vector [lst list?]) immutable-vector?]
  @defproc[(immutable-vector-map [proc procedure?] [vec immutable-vector?] ...+) 
           immutable-vector?]
  @defproc[(immutable-vector-append [vec immutable-vector?] ...) immutable-vector?]
  @defproc[(immutable-vector-take [vec immutable-vector?] [pos exact-nonnegative-integer?])
           immutable-vector?]
  @defproc[(immutable-vector-take-right [vec immutable-vector?] [pos exact-nonnegative-integer?])
           immutable-vector?]
  @defproc[(immutable-vector-drop [vec immutable-vector?] [pos exact-nonnegative-integer?])
           immutable-vector?]
  @defproc[(immutable-vector-drop-right [vec immutable-vector?] [pos exact-nonnegative-integer?])
           immutable-vector?]
  @defproc[(immutable-vector-split-at [vec immutable-vector?] [pos exact-nonnegative-integer?])
           (values immutable-vector? immutable-vector?)]
  @defproc[(immutable-vector-split-at-right [vec immutable-vector?] [pos exact-nonnegative-integer?])
           (values immutable-vector? immutable-vector?)]
  @defproc[(immutable-vector-copy [vec immutable-vector?] 
                                  [start exact-nonnegative-integer? 0]
                                  [end exact-nonnegative-integer? (vector-length v)]) 
           immutable-vector?]
  @defproc[(immutable-vector-filter [pred procedure?] [vec immutable-vector?])
           immutable-vector?]
  @defproc[(immutable-vector-filter-not [pred procedure?] [vec immutable-vector?])
           immutable-vector?]
]]{
Versions of @racket[build-vector], @racket[vector-append], @racket[vector-map] etc.
that return immutable vectors instead of mutable ones.
}

