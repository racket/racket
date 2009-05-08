#lang scribble/doc
@(require "web-server.ss")

@title[#:tag "responders.ss"]{Standard Responders}
@(require (for-label web-server/configuration/responders
                     web-server/http
                     net/url))

@defmodule[web-server/configuration/responders]{

This module provides some functions that help constructing HTTP responders.
These functions are used by the default dispatcher constructor (see @secref["web-server-unit.ss"]) to
turn the paths given in the @scheme[configuration-table] into responders for the associated circumstance.

@defproc[(file-response (http-code natural-number/c) (short-version string?) (text-file string?) (header header?) ...)
         response/c]{
 Generates a @scheme[response/full] with the given @scheme[http-code] and @scheme[short-version]
as the corresponding fields; with the content of the @scheme[text-file] as the body; and, with
the @scheme[header]s as, you guessed it, headers.

This does not cause redirects to a well-known URL, such as @filepath{conf/not-found.html}, but rather use the contents
of @filepath{not-found.html} (for example) as its contents. Therefore, any relative URLs in @scheme[text-file] are relative
to whatever URL @scheme[file-response] is used to respond @emph{to}. Thus, you should probably use absolute URLs in these files.
}

@defproc[(servlet-loading-responder (url url?) (exn exn?))
         response/c]{
 Gives @scheme[exn] to the @scheme[current-error-handler] and response with a stack trace and a "Servlet didn't load" message.
}

@defproc[(gen-servlet-not-found (file path-string?))
         ((url url?) . -> . response/c)]{
 Returns a function that generates a standard "Servlet not found." error with content from @scheme[file].
}

@defproc[(servlet-error-responder (url url?) (exn exn?))
         response/c]{
 Gives @scheme[exn] to the @scheme[current-error-handler] and response with a stack trace and a "Servlet error" message.
}
                                       
@defproc[(gen-servlet-responder (file path-string?))
         ((url url?) (exn any/c) . -> . response/c)]{
 Prints the @scheme[exn] to standard output and responds with a "Servlet error." message with content from @scheme[file].
}

@defproc[(gen-servlets-refreshed (file path-string?))
         (-> response/c)]{
 Returns a function that generates a standard "Servlet cache refreshed." message with content from @scheme[file].
}

@defproc[(gen-passwords-refreshed (file path-string?))
         (-> response/c)]{
 Returns a function that generates a standard "Passwords refreshed." message with content from @scheme[file].
}

@defproc[(gen-authentication-responder (file path-string?))
         ((url url?) (header header?) . -> . response/c)]{
 Returns a function that generates an authentication failure error with content from @scheme[file] and
@scheme[header] as the HTTP header.
}

@defproc[(gen-protocol-responder (file path-string?))
         ((url url?) . -> . response/c)]{
 Returns a function that generates a "Malformed request" error with content from @scheme[file].
}

@defproc[(gen-file-not-found-responder (file path-string?))
         ((req request?) . -> . response/c)]{
 Returns a function that generates a standard "File not found" error with content from @scheme[file].
}

@defproc[(gen-collect-garbage-responder (file path-string?))
         (-> response/c)]{
 Returns a function that generates a standard "Garbage collection run" message with content from @scheme[file].
}

}
