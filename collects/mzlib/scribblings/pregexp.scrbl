#lang scribble/doc
@(require "common.rkt"
          (for-label mzlib/pregexp
                     (only-in scheme/base regexp-quote)))

@mzlib[#:mode title pregexp]

@deprecated[@racketmodname[racket/base]]{}

The @racketmodname[mzlib/pregexp] library provides wrappers around
@racket[regexp-match], @|etc| that coerce string and byte-string
arguments to @racket[pregexp] matchers instead of @racket[regexp]
matchers.

The library also re-exports: @racket[pregexp], and it re-exports
@racket[regexp-quote] as @racket[pregexp-quote].

@deftogether[(
@defproc[(pregexp-match [pattern (or/c string? bytes? regexp? byte-regexp?)]
                       [input (or/c string? bytes? input-port?)]
                       [start-pos exact-nonnegative-integer? 0]
                       [end-pos (or/c exact-nonnegative-integer? false/c) #f]
                       [output-port (or/c output-port? false/c) #f])
         (or/c (listof (or/c (cons (or/c string? bytes?)
                                   (or/c string? bytes?))
                             false/c))
               false/c)]
@defproc[(pregexp-match-positions [pattern (or/c string? bytes? regexp? byte-regexp?)]
                        [input (or/c string? bytes? input-port?)]
                        [start-pos exact-nonnegative-integer? 0]
                        [end-pos (or/c exact-nonnegative-integer? false/c) #f]
                        [output-port (or/c output-port? false/c) #f])
          (or/c (listof (or/c (cons exact-nonnegative-integer?
                                    exact-nonnegative-integer?)
                              false/c))
                false/c)]
@defproc[(pregexp-split [pattern (or/c string? bytes? regexp? byte-regexp?)]
                       [input (or/c string? bytes? input-port?)]
                       [start-pos exact-nonnegative-integer? 0]
                       [end-pos (or/c exact-nonnegative-integer? false/c) #f])
         (listof (or/c string? bytes?))]
@defproc[(pregexp-replace [pattern (or/c string? bytes? regexp? byte-regexp?)]
                         [input (or/c string? bytes?)]
                         [insert (or/c string? bytes? 
                                       (string? . -> . string?)
                                       (bytes? . -> . bytes?))])
         (or/c string? bytes?)]
@defproc[(pregexp-replace* [pattern (or/c string? bytes? regexp? byte-regexp?)]
                          [input (or/c string? bytes?)]
                          [insert (or/c string? bytes? 
                                        (string? . -> . string?)
                                        (bytes? . -> . bytes?))])
         (or/c string? bytes?)]
)]{

Like @racket[regexp-match], @|etc|, but a string @racket[pattern]
argument is compiled via @racket[pregexp], and a byte string
@racket[pattern] argument is compiled via @racket[byte-pregexp].}
