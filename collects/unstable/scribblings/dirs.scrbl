#lang scribble/manual
@(require scribble/eval "utils.rkt" (for-label racket unstable/dirs))

@(define unsyntax #f)

@(define (new-evaluator)
   (let* ([e (make-base-eval)])
     (e '(require (for-syntax racket/base)
                  unstable/dirs))
     e))

@(define evaluator (new-evaluator))

@(define reference-path
   '(lib "scribblings/reference/reference.scrbl"))

@title[#:tag "dirs"]{Directories}

@defmodule[unstable/dirs]

@unstable[@author+email["Carl Eastlund" "cce@ccs.neu.edu"]]

This library defines utilities dealing with the directory paths used by the
Racket distribution.

@defproc[(path->directory-relative-string
          [path path-string?]
          [#:default default any/c (if (path? path) (path->string path) path)]
          [#:dirs dirs
                  (listof (cons/c (-> path?) any/c))
                  library-relative-directories])
         (or/c string? (one-of/c default))]{

Produces a string rendering of @racket[path], replacing distribution-specific
paths (normally: collections, user-installed collections, or PLanet cache) with
short abbreviations.

The set of paths and their abbreviations may be overridden by the
@racket[#:dirs] option, which accepts an association list.  Its keys must be
thunks which produce a path.  Its values may be either @racket[#f] for no
abbreviation (the directory prefix is simply omitted) or any other value to be
@racket[display]ed in the output.  For instance, @filepath{document.txt}
relative to a path abbreviated @racket["path"] would be rendered as
@racket["<path>/document.txt"].

If the path is not relative to one of the given directories, the default return
value is a string rendering of the unmodified path.  This default may be
overridden by providing @racket[default].

@defexamples[#:eval evaluator
(path->directory-relative-string
 (build-path "source" "project.rkt"))
(path->directory-relative-string
 (build-path (current-directory) "source" "project.rkt"))
(path->directory-relative-string
 (build-path "/" "source" "project.rkt"))
(path->directory-relative-string
 (build-path "/" "source" "project.rkt")
 #:default #f)
(path->directory-relative-string
 (build-path "/" "source" "project.rkt")
 #:dirs (list
         (cons (lambda () (build-path "/" "source"))
               'src)))
]

}

@defthing[library-relative-directories (listof (cons (-> path?) any/c))]{

Represents the default directory substitutions for
@racket[path->directory-relative-string].  By default, the collections directory
is replaced by @racketresult[collects], the user-installed collections directory
is replaced by @racketresult[user], and the PLaneT cache is replaced by
@racketresult[planet].

}

@defthing[setup-relative-directories (listof (cons (-> path?) any/c))]{

Represents the directory substitutions used by @exec{setup-plt}.  The
collections directory is omitted, the user-installed collections directory is
replaced by @racketresult[user], and the PLaneT cache is replaced by
@racketresult[planet].

}

@close-eval[evaluator]
