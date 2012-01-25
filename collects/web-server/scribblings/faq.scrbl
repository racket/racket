#lang scribble/doc
@(require "web-server.rkt")

@title[#:tag "faq"]{Troubleshooting and Tips}

@section{Why is my servlet failing with a @racket[can-be-response?] contract violation after updating Racket?}

After 5.0.2, the Web Server had a backwards incompatible change that prevents X-expressions and lists of bytes from being directly returned from servlets. Please read @filepath{PLTHOME/collects/web-server/compat/0/README} to learn about porting your servlets forward. Don't worry. It's easy.

@section{Why are my templates not updating on the server when I change the file on disk?}

Templates are compiled into your application, so when you change them there is no connection between that change in the filesystem and the compiled bytecode that is already loaded in a running Web server process. For more discussion, see @secref["update-servlets"].

@section{Why are templates compiled into programs?}

@(require (for-label web-server/templates))

Since templates can include arbitrary Racket code, macros, etc and refer to
arbitrary identifiers, @racket[include-template] is really just an obscured
@racket[require].

@section[#:tag "update-servlets"]{Why are my stateful servlets not updating on the server when I change the file on disk?}

@(require (for-label web-server/dispatchers/dispatch-servlets
                     web-server/servlet-env))

If you are using @racket[serve/servlet], it starts a Web server that directly references a closure that has no connection
to some file on the disk.

If you are using the command-line tool, or configuration file, then by default,
the server uses @racket[make-cached-url->servlet] to load servlets
from the disk. As it loads them, they are cached and the disk is not referred to for future
requests. This ensures that there is a single namespace for each servlet, so that different instances
can share resources, such as database connections, and communicate through the store. The default
configuration of the server (meaning the dispatcher sequence used when you load a configuration file)
provides a special URL to localhost that will reset the cache: @filepath{/conf/refresh-servlets}.

If you want the server to reload your changed servlet code, then GET this URL and the server will reload the
servlet on the next request. However, you may be surprised by what happens on the next request. For more discussion, see @secref["refresh-servlets"].

@section[#:tag "refresh-servlets"]{After refreshing my stateful servlet, old captured continuations don't change or old global effects are gone. Why?}

Every load of your servlet is in a fresh namespace. When you refresh, a new namespace without the old effects is created. Old captured continuations
refer to the original namespace and will never update. It is impossible, in general, to port a continuation from one namespace to another, because the
code could be arbitrarily different.

@section{How are stateless servlets different from stateful servlets vis a vis refreshing?}

Continuations are serialized with a hash that ensures that any source
code modifications makes all the old continuations incompatible for
the same reason native continuations naturally are.

However, this hash only protects against changes in a single source file. Therefore if you modularize
your application, then only continuations that refer to changed source files will be incompatible.
For example, if you put all your templates in a single module, then it can change without
invalidating old continuations.

@section{What special considerations are there for security with the Web Server?}

The biggest problem is that a naive usage of continuations will allow continuations to subvert
authentication mechanisms. Typically, all that is necessary to execute a continuation is its URL.
Thus, URLs must be as protected as the information in the continuation.

Consider if you link to a public site from a private continuation URL: the @exec{Referrer} field in
the new HTTP request will contain the private URL. Furthermore, if your HTTP traffic is in the clear,
then these URLs can be easily poached.

One solution to this is to use a special cookie as an authenticator. This way, if a URL escapes, it will
not be able to be used, unless the cookie is present. For advice about how to do this well, see
@link["http://cookies.lcs.mit.edu/pubs/webauth.html"]{Dos and Don'ts of Client Authentication on the Web}
from the MIT Cookie Eaters.

Note: It may be considered a great feature that URLs can be shared this way, because delegation is
easily built into an application via URLs.

@section{My browser displays my page strangely: my CSS is ignored, sections are missing, etc.}

@(require (for-label xml
                     web-server/http))

@(define xexpr @tech[#:doc '(lib "xml/xml.scrbl")]{X-expression})

Most @web-server developers use @|xexpr|s for representing the
HTML of their page. However, @|xexpr|s only represent XML and HTML is not exactly
XML. This is a common source of problems.

For example, XML allows the "empty tag shorthand", e.g. @litchar{<img
src='...' />}, on every tag, while HTML occasionally requires an end
tag,
e.g. @link["http://www.w3.org/TR/html401/interact/forms.html#h-17.7"]{TEXTAREA}.
Similarly, XML allows an end tag, e.g. @litchar{<img
src='...'></img>}, on every tag, while HTML occasionally forbides an
end tag,
e.g. @link["http://www.w3.org/TR/html401/struct/objects.html#h-13.2"]{IMG}.
(Of course, browsers do not necessarily implement their HTML parsing
as specified and may be more or less lenient towards XML-like HTML, so
your test browser may not treat these forms as problematic.)

Since the @web-server uses @racketmodname[xml] to format @|xexpr|s, it
inherits @racketmodname[xml]'s default rendering behavior in general
and its use of "empty tag shorthand" in
particular. @racketmodname[xml]'s default is always use the shorthand
with the tags from @racket[html-empty-tags] and never otherwise. This
list should contain the W3C's approved list. You can change it with
the @racket[empty-tag-shorthand] parameter.

You can also change your @xexpr so that an end tag is forced. For
example, @racket['(textarea [(name "text")])] renders as
@litchar{<textarea name="text" />}, while @racket['(textarea [(name
"text")] "")] renders as @litchar{<textarea name="text"></textarea>},
because of the string content in the @|xexpr|. In this case, the end
tag will always be present regardless of the value of
@racket[empty-tag-shorthand]. It is not possible to force the other
possibility; i.e., never include an end tag.

You may think the @web-server could do a better job advertising that
the contents it serves is more like XML by default. Unfortunately,
browser support for such
@link["http://www.w3.org/TR/xhtml-media-types/#media-types"]{advertisement}
is @link["http://www.w3.org/MarkUp/2004/xhtml-faq#ie"]{lacking}.  You
can use @racket[response/xexpr] to easily customize your
application's MIME type and response headers.

Finally, you may find Web browser inspectors such as the Safari
Inspector, Firebug, and the Google Chrome error console to be useful
tools in identifying offending tags.

@section{How do I use templates ``dynamically"?}

@(require (for-label web-server/templates))

A common feature request is to include one template in another dynamically. It should hopefully be obvious that @racket[include-template] can be included in a template to include a @emph{static} sub-template. For example,
@racketblock[
 (include-template "posts.html")
]
may appear inside the @filepath{blog.html} template. But you will quickly find that @racket[(include-template _expr)] will fail when @racket[_expr] is not syntactically a path, e.g.:
@racketblock[
....
(include-template (if logged-in?
                      "user-info.html"
                      "auth.html"))
....
]

What is the solution? The templating system already allows you to parameterize templates so particular components come from the including scope. There is no reason those values can not be the results of other templates. In the previous example, suppose the includer was
@racketblock[
(define (main-page logged-in?)
  (include-template "site.html"))
]

We could change it to:
@racketblock[
(define (main-page logged-in?)
  (define user-content
    (if logged-in?
        (include-template "user-info.html")
        (include-template "auth.html")))
  (include-template "site.html"))
]

and @filepath{site.html} to:
@racketblock[
....
user-content
....
]

This allows you to do the same thing but is safer and more efficient: safer because there is no way to include templates that are not named by the programmer and more efficient because all the templates are compiled (and optimized) with the rest of the code.

If you insist on dynamicism, there is always @racket[eval].
