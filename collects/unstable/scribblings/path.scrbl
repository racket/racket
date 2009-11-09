#lang scribble/doc
@(require scribble/base
          scribble/manual
          (for-label unstable/path
                     unstable/contract
                     scheme/contract
                     scheme/base))

@title[#:tag "path"]{Path}

@defmodule[unstable/path]

@defproc[(explode-path* [p path-string?])
         (listof path-element?)]{
 Like @scheme[normalize-path], but does not resolve symlinks.
}

@defproc[(path-without-base [base path-string?]
                            [p path-string?])
         (listof path-element?)]{
 Returns, as a list, the portion of @scheme[p] after @scheme[base],
 assuming @scheme[base] is a prefix of @scheme[p].
}

@defproc[(directory-part [p path-string?])
         path?]{
 Returns the directory part of @scheme[p], returning @scheme[(current-directory)]
 if it is relative.
}

@defproc[(build-path-unless-absolute [base path-string?]
                                     [p path-string?])
         path?]{
 Prepends @scheme[base] to @scheme[p], unless @scheme[p] is absolute.
}

@defproc[(strip-prefix-ups [p (listof path-element?)])
         (listof path-element?)]{
 Removes all the prefix @scheme[".."]s from @scheme[p].
}
