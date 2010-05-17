#lang scribble/doc
@(require "common.rkt"
          (for-label mzlib/pregexp
                     (only-in scheme/base regexp-quote)))

@mzlib[#:mode title pregexp]

The @schememodname[mzlib/pregexp] library provides wrappers around
@scheme[regexp-match], @|etc| that coerce string and byte-string
arguments to @scheme[pregexp] matchers instead of @scheme[regexp]
matchers.

The library also re-exports: @scheme[pregexp], and it re-exports
@scheme[regexp-quote] as @scheme[pregexp-quote].

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

Like @scheme[regexp-match], @|etc|, but a string @scheme[pattern]
argument is compiled via @scheme[pregexp], and a byte string
@scheme[pattern] argument is compiled via @scheme[byte-pregexp].}
