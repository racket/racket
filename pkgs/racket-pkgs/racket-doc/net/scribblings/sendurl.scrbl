#lang scribble/doc
@(require "common.rkt" (for-label net/sendurl racket/file))

@title[#:tag "sendurl"]{Send URL: Opening a Web Browser}

@defmodule[net/sendurl]{Provides @racket[send-url] for opening a URL
in the user's chosen web browser.}

See also @racketmodname[browser/external #:indirect], which requires
@racket[racket/gui], but can prompt the user for a browser if no
browser preference is set.


@defproc[(send-url [str string?] [separate-window? any/c #t]
                   [#:escape escape? any/c #t])
         void?]{

Opens @racket[str], which represents a URL, in a platform-specific
manner. For some platforms and configurations, the
@racket[separate-window?] parameter determines if the browser creates
a new window to display the URL or not.

On Windows, @racket[send-url] normally uses @racket[shell-execute]
to launch a browser. (If the URL appears to contain a fragment, it may
use an intermediate redirecting file due to a bug in IE7.)

On Mac OS X, @racket[send-url] runs @exec{osascript} to start the
user's chosen browser.

On Unix, @racket[send-url] uses a user-preference, or when none is
set, it will look for a known browser.  See the description of
@racket[external-browser] for details.

The @racket[url] string is usually escaped to avoid dangerous shell
characters (quotations, dollar signs, backslashes, and non-ASCII).  Note
that it is a good idea to encode URLs before passing them to this
function.

On all platforms, @racket[external-browser] parameter can be set to a
procedure to override the above behavior --- the procedure will be
called with the @racket[url] string.}

@defproc[(send-url/file [path path-string?] [separate-window? any/c #t]
                        [#:fragment fragment (or/c string? false/c) #f]
                        [#:query query (or/c string? false/c) #f])
         void?]{

Similar to @racket[send-url], but accepts a path to a file to be
displayed by the browser.  Use this function when you want to display
a local file: it takes care of the peculiarities of constructing the
correct @litchar{file://} URL, and uses @racket[send-url] to display
the file.  If you need to use an anchor fragment or a query string,
use the corresponding keyword arguments.}

@defproc[(send-url/contents [contents string?] [separate-window? any/c #t]
                            [#:fragment fragment (or/c string? false/c) #f]
                            [#:query query (or/c string? false/c) #f]
                            [#:delete-at seconds (or/c number? false/c) #f])
         void?]{

Similar to @racket[send-url/file], but it consumes the contents of a
page to show, and displayes it from a temporary file.

If @racket[delete-at] is a number, the temporary file is removed after
this many seconds.  The deletion happens in a thread, so if racket
exits before that it will not happen --- when this function is called
it scans old generated files (this happens randomly, not on every
call) and removes them to avoid cluttering the temporary directory.
If @racket[delete-at] is @racket[#f], no delayed deletion happens, but
old temporary files are still deleted as described above.}

@defparam[external-browser cmd browser-preference?]{

A parameter that can hold a procedure to override how a browser is
started, or @racket[#f] to use the default platform-dependent command.

On Unix, the command that is used depends on the
@racket['external-browser] preference.  If the preference is unset,
@racket[send-url] uses the first of the browsers from
@racket[unix-browser-list] for which the executable is found.
Otherwise, the preference should hold a symbol indicating a known
browser (from the @racket[unix-browser-list]), or it a pair of a prefix
and a suffix string that are concatenated around the @racket[url] string
to make up a shell command to run.  In addition, the
@racket[external-browser] paremeter can be set to one of these values,
and @racket[send-url] will use it instead of the preference value.

Note that the URL is encoded to make it work inside shell double-quotes:
URLs can still hold characters like @litchar{#}, @litchar{?}, and
@litchar{&}, so if the @racket[external-browser] is set to a pair of
prefix/suffix strings, they should use double quotes around the url.

If the preferred or default browser can't be launched,
@racket[send-url] fails. See @racket[get-preference] and
@racket[put-preferences] for details on setting preferences.}

@defproc[(browser-preference? [a any/c]) boolean?]{

Returns @racket[#t] if @racket[v] is a valid browser preference,
@racket[#f] otherwise. See @racket[external-browser] for more
information.}

@defthing[unix-browser-list (listof symbol?)]{

A list of symbols representing Unix executable names that may be tried
in order by @racket[send-url]. The @racket[send-url] function
internally includes information on how to launch each executable with
a URL.}

