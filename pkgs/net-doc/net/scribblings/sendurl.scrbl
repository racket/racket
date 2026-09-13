#lang scribble/doc
@(require "common.rkt" (for-label net/sendurl racket/file))

@title[#:tag "sendurl"]{Send URL: Opening a Web Browser}

@defmodule[net/sendurl]{Provides @racket[send-url] for opening a URL
in the user's chosen web browser.}

See also @racketmodname[browser/external #:indirect], which requires
@racketmodname[racket/gui #:indirect], but can prompt the user for a browser if no
browser preference is set.


@defproc[(send-url [str string?] [separate-window? any/c #t]
                   [#:escape? escape? any/c #t])
         void?]{

Opens @racket[str], which represents a URL, in a platform-specific
manner.
In particular, the first value in @racket[browser-list] will determine
which browser will be used to open the URL.
For some platforms and configurations, the
@racket[separate-window?] parameter determines if the browser creates
a new window to display the URL or not.

If @racket[escape?] is true, then @racket[str] is escaped (by UTF-8
encoding followed by ``%'' encoding) to avoid dangerous shell
characters: single quotes, double quotes, backquotes, dollar signs,
backslashes, non-ASCII characters, and non-graphic characters. Note
that escaping does not affect already-encoded characters in
@racket[str].

There are two ways to override the above behavior: the @racket[external-browser] parameter and
the @racket['external-browser] preference.
A valid setting for both the @racket[external-browser] parameter and the @racket['external-browser] preference
must satisfy @racket[browser-preference?], with a restriction that
the setting for the @racket['external-browser] preference cannot be a procedure.
The @racket[external-browser] parameter takes priority over @racket['external-browser] preference:
the preference is only used when @racket[(external-browser)] is @racket[#f].
See @racket[put-preferences] for details on setting preferences.

On Unix, it's recommended to not override the default behavior, but to rely on @tt{xdg-open} in @racket[browser-list].}

@defproc[(send-url/file [path path-string?] [separate-window? any/c #t]
                        [#:fragment fragment (or/c string? #f) #f]
                        [#:query query (or/c string? #f) #f])
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
                            [#:fragment fragment (or/c string? #f) #f]
                            [#:query query (or/c string? #f) #f]
                            [#:delete-at seconds (or/c number? #f) #f])
         void?]{

Similar to @racket[send-url/file], but it consumes the contents of a
page to show and displays it from a temporary file.

When @racket[send-url/contents] is called, it scans old generated files
(this happens randomly, not on every call) and removes them to avoid
cluttering the temporary directory.  If the @racket[seconds]
argument is a number, then the temporary file is more eagerly removed
after the specified number of seconds; the deletion happens in a
thread, so if Racket exits earlier, the deletion will not happen.  If
the @racket[seconds] argument is @racket[#f], no eager deletion
happens, but old temporary files are still deleted as described
above.}

@defproc[(send-url/mac [url string?]
                       [#:browser browser (or/c string? #f) #f])
         void?]{
 Like @racket[send-url], but only for use on a Mac OS machine.

 The optional @racket[browser] argument, if present, should be the name
 of a browser installed on the system. For example,
 @racketblock[(send-url/mac "https://www.google.com/" #:browser "Firefox")]
 would open the url in Firefox, even if that's not the default browser.
 Passing @racket[#f] means to use the default browser.
}

@defparam[external-browser cmd browser-preference?]{
A parameter that can hold a browser preference to override how a browser is
started for @racket[send-url]. See @racket[browser-preference?] for details.}

@defproc[(browser-preference? [a any/c]) boolean?]{

Returns @racket[#t] if @racket[v] is a valid browser preference,
@racket[#f] otherwise. A valid browser preference is either:

@itemlist[
  @item{The value @racket[#f], which falls back to the next preference method.}
  @item{A @racket[procedure?] that accepts a URL string.
        This value is not allowed for the @racket['external-browser] preference.}
  @item{A symbol in @racket[browser-list] that indicates a browser to use.}
  @item{A pair of strings to be concatenated with a URL string to form a shell command to run.
        The first string is the command prefix and the second string is the command suffix.
        This method requires extra care:

        @itemlist[
          @item{The URL can hold characters like @litchar{#}, @litchar{?}, and
                @litchar{&}, so the pair of strings should place double quotes around the URL.}
          @item{The URL should be encoded to make it work inside shell double-quotes,
                so the default value of @racket[escape?] in @racket[send-url] should be used.}]}]}

@defthing[browser-list (listof symbol?)]{

A list of symbols representing executable names that may be tried
in order by @racket[send-url]. The @racket[send-url] function
internally includes information on how to launch each executable with
a URL.

@history[#:added "7.5.0.10"]}

@defthing[unix-browser-list (listof symbol?)]{

@deprecated[#:what "value" @racket[browser-list]]

The same as @racket[browser-list].

@history[#:changed "7.5.0.10" @elem{Changed the value to be an alias of @racket[browser-list].}]}
