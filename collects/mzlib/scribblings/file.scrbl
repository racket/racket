#lang scribble/doc
@(require "common.rkt"
          (for-label mzlib/file
                     scheme/contract))


@mzlib[#:mode title file]

@deprecated[@racketmodname[racket/file]]{}

The @racketmodname[mzlib/file] library mostly re-exports from
@racketmodname[scheme/file]:

@racketblock[
find-relative-path
explode-path
normalize-path
filename-extension
file-name-from-path
path-only
delete-directory/files
copy-directory/files
make-directory*
make-temporary-file
get-preference
put-preferences
fold-files
find-files
pathlist-closure
]

@deftogether[(
@defproc[(call-with-input-file* [file path-string?] 
                                [proc (input-port? -> any)]
                                [mode (one-of/c 'text 'binary) 'binary])
         any]
@defproc[(call-with-output-file* [file path-string?] 
                                 [proc (output-port? -> any)]
                                 [mode (one-of/c 'text 'binary) 'binary]
                                 [exists (one-of/c 'error 'append 'update
                                                   'replace 'truncate 'truncate/replace) 'error])
         any]
)]{

Like @racket[call-with-input-file]and @racket[call-with-output-file],
except that the opened port is closed if control escapes from the body
of @racket[proc].}

@deftogether[(
@defproc[(build-relative-path [base (or/c path-string?
                                          (one-of/c 'up 'same))]
                              [sub (or/c (and/c path-string? 
                                                relative-path?)
                                         (one-of/c 'up 'same))] ...)
         (and/c path? relative-path?)]
@defproc[(build-absolute-path [base (or/c (and/c path-string?
                                                 (not/c relative-path?))
                                          (one-of/c 'up 'same))]
                              [sub (or/c (and/c path-string? 
                                                (not/c complete-path?))
                                         (one-of/c 'up 'same))] ...)
         (and/c path? absolute-path?)]
)]{

Like @racket[build-path], but with extra constraints to ensure a
relative or absolute result.}
