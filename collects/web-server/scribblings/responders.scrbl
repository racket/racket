#lang scribble/doc
@(require "web-server.rkt")

@title[#:tag "responders"]{Standard Responders}
@(require (for-label web-server/configuration/responders
                     web-server/http
                     net/url))

@defmodule[web-server/configuration/responders]{

This module provides some functions that help constructing HTTP
responders.  These functions are used by the default dispatcher
constructor (see @secref["web-server-unit"]) to turn the paths given in
the @racket[configuration-table] into responders for the associated
circumstance.

@defproc[(file-response [http-code natural-number/c]
                        [short-version string?]
                        [text-file string?]
                        [header header?] ...)
         response?]{
 Generates a @racket[response?] with the given @racket[http-code] and
 @racket[short-version] as the corresponding fields; with the content of
 the @racket[text-file] as the body; and, with the @racket[header]s as,
 you guessed it, headers.

This does not cause redirects to a well-known URL, such as
@filepath{conf/not-found.html}, but rather use the contents of
@filepath{not-found.html} (for example) as its contents. Therefore, any
relative URLs in @racket[text-file] are relative to whatever URL
@racket[file-response] is used to respond @emph{to}. Thus, you should
probably use absolute URLs in these files.
}

@defproc[(servlet-loading-responder [url url?] [exn exn?])
         response?]{
 Gives @racket[exn] to the @racket[current-error-handler] and response
 with a stack trace and a "Servlet didn't load" message.
}

@defproc[(gen-servlet-not-found [file path-string?])
         ((url url?) . -> . response?)]{
 Returns a function that generates a standard "Servlet not found." error
 with content from @racket[file].
}

@defproc[(servlet-error-responder [url url?] [exn exn?])
         response?]{
 Gives @racket[exn] to the @racket[current-error-handler] and response
 with a stack trace and a "Servlet error" message.
}

@defproc[(gen-servlet-responder [file path-string?])
         ((url url?) (exn any/c) . -> . response?)]{
 Prints the @racket[exn] to standard output and responds with a "Servlet
 error." message with content from @racket[file].
}

@defproc[(gen-servlets-refreshed [file path-string?])
         (-> response?)]{
 Returns a function that generates a standard "Servlet cache refreshed."
 message with content from @racket[file].
}

@defproc[(gen-passwords-refreshed [file path-string?])
         (-> response?)]{
 Returns a function that generates a standard "Passwords refreshed."
 message with content from @racket[file].
}

@defproc[(gen-authentication-responder [file path-string?])
         ((url url?) (header header?) . -> . response?)]{
 Returns a function that generates an authentication failure error with
 content from @racket[file] and @racket[header] as the HTTP header.
}

@defproc[(gen-protocol-responder [file path-string?])
         ((url url?) . -> . response?)]{
 Returns a function that generates a "Malformed request" error with
 content from @racket[file].
}

@defproc[(gen-file-not-found-responder [file path-string?])
         ((req request?) . -> . response?)]{
 Returns a function that generates a standard "File not found" error
 with content from @racket[file].
}

@defproc[(gen-collect-garbage-responder [file path-string?])
         (-> response?)]{
 Returns a function that generates a standard "Garbage collection run"
 message with content from @racket[file].
}

}
