#lang scribble/doc
@(require "common.rkt"
          (for-label net/git-checkout))

@title[#:tag "git-checkout"]{Git Repository Checkout}

@defmodule[net/git-checkout]{The @racketmodname[net/git-checkout]
library provides support for extracting a directory tree from a Git
repository that is hosted by a server that implements the @tt{git://}
protocol or its layering over HTTP(S). The
@racketmodname[net/git-checkout] library does not rely on external
binaries (such as a @exec{git} client) or Git-specific native
libraries (such as @filepath{libgit}).}

When run as a program, @racket[net/git-checkout] accepts command-line
arguments to drive the checkout. Use
@;
@commandline{racket -l- net/git-checkout -h}
@;
for information on command-line arguments and flags.

@defproc[(git-checkout [hostname string?]
                       [repository string?]
                       [#:dest-dir dest-dir (or/c path-string? #f)]
                       [#:ref ref string? "master"]
                       [#:transport transport (or/c 'git 'http 'https) 'git]
                       [#:depth depth (or/c #f exact-positive-integer?) 1]
                       [#:status-printf status-printf (string? any/c ... . -> . void?) (lambda args
                                                                                         (apply printf args)
                                                                                         (flush-output))]
                       [#:tmp-dir given-tmp-dir (or/c #f path-string?) #f]
                       [#:clean-tmp-dir? clean-tmp-dir? any/c (not given-tmp-dir)]
                       [#:verify-server? verify-server? any/c #t]
                       [#:port port (or/c #f (integer-in 1 65535)) (case transport
                                                                     [(git) 9418]
                                                                     [(http) 80]
                                                                     [(https) 443])])
         string?]{

Contacts the server at @racket[hostname] and @racket[port]
(where @racket[#f] is replaced by the default)
to download the repository whose name on the server is
@racket[repository] (normally ending in @filepath{.git}).  The tree
within the repository that is identified by @racket[ref] (which can be
a branch, tag, commit ID, or tree ID) is extracted to
@racket[dest-dir], and the result id an ID corresponding to @racket[ref].

If @racket[transport] is @racket['git], then the server is contacted
using Git's native transport. If @racket[transport] is
@racket['http] or @racket['https], then the server is contacted using
HTTP(S).  In the case of @racket['https],
the server's identity is verified unless @racket[verify-server?] is
false or the @indexed-envvar{GIT_SSL_NO_VERIFY} environment variable
is set.

If @racket[dest-dir] is @racket[#f], then the result is an ID
determined for @racket[ref] from just the server's report of the
available branches and tags, or @racket[ref] itself if it does not
match a branch or tag name and looks like an ID.

A local clone of the repository is @emph{not} preserved, but is
instead discarded after the tree is extracted to @racket[dest-dir].
If @racket[dest-dir] does not exist, it is created. If
@racket[dest-dir] does exist, its existing content is left in place
except as replaced by content from the Git repository.

If @racket[ref] identifies a branch or tag by either name or by
commit ID, then the @tt{git://} protocol allows @racket[git-checkout]
to download only the commits and objects relevant to the branch or
tag. Furthermore, the default @racket[depth] argument allows
@racket[git-checkout] to obtain only the latest commit and its
objects, instead of the entire history of the branch or commit.  If
@racket[ref] is any other commit ID or tree ID, then the entire
repository is downloaded, including all branches.

Status information is reported via @racket[status-printf]. The same
information is always logged with the name @racket['git-checkout] at
the @racket['info] level.

If @racket[tmp-dir] is not @racket[#f], then it is used to store a
temporary clone of the repository, and the files are preserved unless
@racket[clean-tmp-dir?] is true. The clone does not currently match
the shape that is recognized by other tools, such as @exec{git}, and
so a preserved temporary directory is useful mainly for debugging.

@history[#:added "6.1.1.1"]}
