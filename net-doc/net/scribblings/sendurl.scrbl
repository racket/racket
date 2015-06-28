#lang scribble/doc
@(require "common.rkt" (for-label net/sendurl racket/file))

@title[#:tag "sendurl"]{Send URL: Opening a Web Browser}

@defmodule[net/sendurl]{Provides @racket[send-url] for opening a URL
in the user's chosen web browser.}

See also @racketmodname[browser/external #:indirect], which requires
@racket[racket/gui], but can prompt the user for a browser if no
browser preference is set.


@defproc[(send-url [str string?] [separate-window? any/c #t]
                   [#:escape? escape? any/c #t])
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

If @racket[escape?] is true, then @racket[str] is escaped (by UTF-8
encoding followed by ``%'' encoding) to avoid dangerous shell
characters: single quotes, double quotes, backquotes, dollar signs,
backslashes, non-ASCII characters, and non-graphic characters. Note
that escaping does not affect already-encoded characters in
@racket[str].

On all platforms, @racket[external-browser] parameter can be set to a
procedure to override the above behavior --- the procedure will be
called with the @racket[url] string.}

@defproc[(send-url/file [path path-string?] [separate-window? any/c #t]
                        [#:fragment fragment (or/c string? false/c) #f]
                        [#:query query (or/c string? false/c) #f])
         void?]{

Similar to @racket[send-url] (with @racket[#:escape? #t]), but accepts
a path to a file to be displayed by the browser, along with optional
@racket[fragment] (with no leading @litchar{#}) and @racket[query]
(with no leading @litchar{?}) strings.  Use @racket[send-url/file] to
display a local file, since it takes care of the peculiarities of
constructing the correct @litchar{file://} URL.

The @racket[path], @racket[fragment], and @racket[query] arguments are
all encoded in the same way as a path provided to @racket[send-url],
which means that already-encoded characters are used as-is.}

@defproc[(send-url/contents [contents string?] [separate-window? any/c #t]
                            [#:fragment fragment (or/c string? false/c) #f]
                            [#:query query (or/c string? false/c) #f]
                            [#:delete-at seconds (or/c number? false/c) #f])
         void?]{

Similar to @racket[send-url/file], but it consumes the contents of a
page to show and displays it from a temporary file.

When @racket[send-url/content] is called, it scans old generated files
(this happens randomly, not on every call) and removes them to avoid
cluttering the temporary directory.  If the @racket[#:delete-at]
argument is a number, then the temporary file is more eagerly removed
after the specified number of seconds; the deletion happens in a
thread, so if Racket exits earlier, the deletion will not happen.  If
the @racket[#:delete-at] argument is @racket[#f], no eager deletion
happens, but old temporary files are still deleted as described
above.}

@defproc[(send-url/mac [url string?]
                       [#:browser browser (or/c string? #f) #f])
         void?]{
 Like @racket[send-url], but only for use on a Mac OS X machine.

 The optional @racket[browser] argument, if present, should be the name
 of a browser installed on the system. For example,
 @racketblock[(send-url/mac "http://www.google.com/" #:browser "Firefox")]
 would open the url in Firefox, even if that's not the default browser.
 Passing @racket[#f] means to use the default browser.
}

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

