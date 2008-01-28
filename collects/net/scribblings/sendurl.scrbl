#lang scribble/doc
@(require "common.ss"
          (for-label net/sendurl
                     scheme/file))

@title{Opening a Web Browser}

@defmodule[net/sendurl]{Provides @scheme[send-url] for opening a URL
in the user's chosen web browser.}

See also @schememodname[browser/external], which requires
@scheme[scheme/gui], but can prompt the user for a browser if no
browser preference is set.


@defproc[(send-url [str string?] [separate-window? any/c #t])
         void?]{

Opens @scheme[str], which represents a URL, in a platform-specific
manner. For some platforms and configurations, the
@scheme[separate-window?] parameter determines if the browser creates
a new window to display the URL or not.

Under Windows, @scheme[send-url] normally uses @scheme[shell-execute]
to launch a browser. If the URL appears to contain a fragment, it may
instead use @exec{ftype htmlfile} to obtain a command-line to run,
since @scheme[shell-execute] drops a fragment.

Under Mac OS X, @scheme[send-url] runs @exec{osascript} to start the
user's chosen browser.

Under Unix, @scheme[send-url] uses the value of the
@scheme[external-browser] parameter to select a browser.}


@defparam[external-browser cmd browser-preference?]{

A parameter that, under Unix, determines the browser started
@scheme[send-url].

The parameter is initialized to the value of the
@scheme['external-browser] preference.

The parameter value can be any of the symbols in
@scheme[unix-browser-list], @scheme[#f] to indicate that the
preference is unset, or a pair of strings.  If the preference is
unset, @scheme[send-url] uses the first of the browsers from
@scheme[unix-browser-list] for which the executable is found. If the
parameter is a pair of strings, then a command line is constructed by
concatenating in order the first string, the URL string, and the
second string.

If the preferred or default browser can't be launched,
@scheme[send-url] fails. See @scheme[get-preference] and
@scheme[put-preferences] for details on setting preferences.}

@defproc[(browser-preference? [a any/c]) boolean?]{

Returns @scheme[#t] if @scheme[v] is a valid browser preference,
@scheme[#f] otherwise. See @scheme[external-browser] for more
information.}

@defthing[unix-browser-list (listof symbol?)]{

A list of symbols representing Unix executable names that may be tried
in order by @scheme[send-url]. The @scheme[send-url] function
internally includes information on how to launch each executable with
a URL.}

