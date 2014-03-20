#lang scribble/manual
@(require scribble/bnf
          (for-label (except-in racket/base #%top #%module-begin)
                     scribble/html
                     plt-web))

@(define (tag . s) @tt{<@|s|>})

@title{Creating PLT-Style Web Pages}

@defmodulelang[plt-web]{The @racketmodname[plt-web] language builds on
@racketmodname[scribble/html] for generating pages in the same style
as @url["http://racket-lang.org/"].}

Unlike @racketmodname[scribble/html], the values of expressions in a
@racketmodname[plt-web] module are not treated as HTML. Instead,
top-level expressions in @racketmodname[plt-web] are as in
@racket[racket/base].

Meanwhile, a @racket[main] submodule is added that runs
@racket[render-all] (after parsing command-line arguments) to render
all registered resources, including HTML pages. Pages are meant to be
registers through a @racket[page] form that is defined by a
@racket[define-context] or @racket[define+provide-context] declaration
that configures a particular site (i.e., collection of pages).

@; ----------------------------------------

@section{Configuring a Site}

A site is identifiers by a relative directory path, which determines
where the site's content is generated. For a non-local build, the
relative directory is mapped to a destination URL via
@racket[url-roots].

@defproc[(site [dir path-string?]
               [#:url url (or/c string? #f) #f]
               [#:always-abs-url? always-abs-url? any/c #t]
               [#:generate? generate? any/c #t]
               [#:share-from share-from (or/c site? #f) #f]
               [#:page-style? page-style? any/c #t]
               [#:page-headers page-headers outputable/c null]
               [#:meta? meta? any/c page-style?]
               [#:robots robots (or/c #f #t outputable/c) #t]
               [#:htaccess htaccess (or/c #f #t outputable/c) #t]
               [#:navigation navigation (listof outputable/c) null])
         site?]{

Creates a value that represents a site.

If @racket[url] is not @racket[#f], then it will be registered to
@racket[url-roots] for a build in web mode (as opposed to local mode)
and recorded as the target for @racket[dir] in a @filepath{sites.rktd}
file when building in @tech{deployment mode}.

If @racket[always-abs-url?] is true (the default), then @racket[url]
is registered with a @racket['abs] flag, so that (in @tech{deployment
mode}) references within a site are relative to the site root, as
opposed to relative to the referencing resource.

If @racket[generate?] is @racket[#f], then resources for the site
(such as icons or CSS files) are not generated.

If @racket[share-from] is a site, then resources generated for the
site (such as icons or CSS files) are used when as possible for the
new site.

If @racket[page-style?] is true, HTML pages generated for the site
include content to set the style of the overall page. Otherwise, only
sufficient resources and content are included to specify the style of
the PLT web-page header (i.e., a bar with the Racket logo).

The @racket[page-headers] argument provides content for the header of
any HTML page generated at the site via @racket[page] or @racket[page*].

If @racket[meta?] is true, then @filepath{.htaccess},
@filepath{robots.txt}, and similar files are generated for the site.
The @racket[robots] and @racket[htaccess] arguments determine robot
and access information included by the default resource-mapping
function. A @racket[#t] value enables normal access, a @racket[#f]
value disables access, and any other value is used as the
corresponding specification.

The @racket[navigation] argument determines content (usually links) to
be included in the PLT header. Currently, up to four such links are
supported.}


@defproc[(site? [v any/c]) boolean?]{

Returns @racket[#t] if @racket[v] represents a site, @racket[#f] otherwise.}


@defproc[(site-dir [s site?]) path-string?]{

Extracts the destination directory of @racket[s].}


@defproc[(site-css-path [s site?]) outputable/c]{

Extracts a reference to a CSS resource for HTML pages at site
@racket[s].}

@defproc[(site-favicon-path [s site?]) outputable/c]{

Extracts a reference to a @filepath{favicon.ico} file for the
site @racket[s]. The result is @racket[#f] if meta-file resources are not
generated for the site.}


@defproc[(site-navbar [s site?]) outputable/c]{

Generates HTML for the banner on HTML pages at the site @racket[s].}


@defproc[(site-navbar-dynamic-js [s site?]) outputable/c]{

Generates a JavaScript definition of a @tt{AddNavbarToBody} function,
which adds a banner dynamically to the current page for a page at site
@racket[s].}

@; ----------------------------------------

@section{Creating Site Content}

@defform[(page keyword-arg ... form ...)
         #:grammar ([keyword-arg (code:line keyword expr)])]{

Equivalent to @racket[(page* keyword-arg ... (lambda () (begin/text form ...)))].}

@defproc[(page* [#:site s site?]
                [#:html-only html-only? any/c #f]
                [#:id id path-string? #f]
                [#:file file (or/c path-string? #f) #f]
                [#:title title string? (... id)]
                [#:link-title link-title outputable/c title]
                [#:window-title window-title string? (string-append "Racket: " label)]
                [#:width width (or/c #f 'full outputable/c) #f]
                [#:description description string? #f]
                [#:extra-headers extra-headers outputable/c #f]
                [#:extra-body-attrs body-attrs outputable/c #f]
                [#:referrer referrer (string? outputable/c ... . -> . outputable/c)
                           (λ (url . content)
                             (a href: url (if (null? content) linktitle content)))]
                [#:part-of part-of (or/c #f symbol?) #f]
                [content outputable/c])
          outputable/c]{

Registers an HTML page as a resource (via @racket[resource]) and
returns a value that can be used to refer to the page within another
resource, assuming that @racket[html-only?] is @racket[#f]. If
@racket[html-only?] is true, then the result represents HTML for the
page, instead of a way to reference the page, and no resource is
registered.

The page is generated as part of the site @racket[s], and either an
@racket[id] or @racket[file] must be provided to identify the page
within the site. Furthermore, either @racket[id] or @racket[title]
must be provided to determine the page's title.

The @racket[link-title] and @racket[window-title] arguments control
separate the title of the page as used by references and for the page
as viewed.

The @racket[width] argument determines the page wide: @racket[#f] is
the default, @racket['full] is full width, and any other value is used
as a CSS width.

The @racket[description] argument provides a meta tag for the page.

The @racket[part-of] argument determines where the page resides in a
larger site when the layout uses a global navigation bar (but the
current format does not use a navigation bar in that sense).}

@defform[(plain keyword-arg ... form ...)
         #:grammar ([keyword-arg (code:line keyword expr)])]{

Equivalent to @racket[(plain* keyword-arg ... (lambda () (begin/text form ...)))].}

@defproc[(plain* [#:site s site?]
                 [#:id id path-string? #f]
                 [#:suffix suffix (or/c #f string?) #f]
                 [#:file file (or/c path-string? #f) #f]
                 [#:referrer referrer (string? outputable/c ... . -> . outputable/c)
                           (λ (url . content)
                             (a href: url (if (null? content) linktitle content)))]
                 [#:newline newline? any/c #t]
                 [content outputable/c])
          outputable/c]{

Like @racket[page*], but for a resource that is a plain file.}

@deftogether[(
@defproc[(copyfile [#:site s site?]
                   [src path-string?]
                   [dest string? (basename src)])
         outputable/c]
@defproc[(symlink [#:site s site?]
                  [src path-string?]
                  [dest string? (basename src)])
         outputable/c]
)]{

Registers a resource that is either a copy of a file or a symbolic link,
returning a value that can be used to reference the resource.}

@defproc[(make-indexes [s site?]
                       [dir (or/c 'same relative-path?)]
                       [#:depth depth (or/c #f exact-nonnegative-integer?) #f]
                       [#:use-dir? use-dir? ((or/c 'same relative-path?) . -> . any/c) (lambda (dir) #t)])
         void?]{

Uses @racket[index-site] and @racket[index-page] to register an
@filepath{index.html} file for every directory within
@racket[dir] (relative to the current directory) that does not have an
@filepath{index.html} file already. If @racket[depth] is not @racket[#f],
then subdirectories are explored at most @racket[depth] layers deep.
The @racket[use-dir?] predicate is called for each directory to determine
whether the directory's subdirectories are traversed.

The generated index files are registered for the site @racket[s] at
destinations that correspond to treating the current directory as the
site root.}

@deftogether[(
@defproc[(index-site? [v any/c]) boolean?]
@defproc[(index-site [site site?]) index-site?]
@defproc[(index-page [isite index-site?]
                     [path (or/c 'same relative-path?)]
                     [content (listof (cons/c path-string? (or/c exact-integer? 'dir)))]
                     [#:html-only? html-only? any/c #f])
         outputable/c]
)]{

The @racket[index-page] function registers an individual
@filepath{index.html} file (or returns its content if
@racket[html-only?] is true) for the given index site, where an index
site is created once for a given site (to register support
resources, such as icons). The @filepath{index.html} file is
generated for the subdirectory indicated by @racket[path]. The index
file lists the content specified by @racket[content], where
an integer corresponds to a file size and @racket['dir] indicates
a directory.}

@defproc[(call-with-registered-roots [thunk (-> any)]) any]{

Calls @racket[thunk] with @racket[url-roots] set to a mapping for
registered sites.}

@; ----------------------------------------

@section{Generating Site Content}

To generate web pages from a @racket[plt-web] module, run the module,
typically with a @Flag{o} flag to specify the output directory. For
example, if @filepath{pages.rkt} is the module, then

@commandline{racket pages.rkt -o /tmp/pages}

builds the output to a @filepath{/tmp/pages} directory.

The command-line flags are recognized by the @racket[main] submodule
that is introduced by @racketmodname[plt-web]:

@itemlist[

 @item{@Flag{w} or @DFlag{web} --- Build output in @deftech{deployment
       mode}, where references within a top-level site use relative
       paths or site-relative paths (that start with @litchar{/}),
       while references across top-level sites use absolute URLs. This
       mode is the default.}

 @item{@Flag{l} or @DFlag{local} --- Build output in local mode using
       @filepath{file://} URLs between top-level sites and relative
       paths within a site.}

 @item{@Flag{r} or @DFlag{relative} --- Build output in local-relative mode, where
       all references use relative paths, exploiting the fact that
       sites are rendered in adjacent directories within the output
       directory. (You may need to deal with an occasional manual
       selection of @filepath{index.html} when viewing relative output.)}

 @item{@Flag{o} @nonterm{dir} or @DFlag{output} @nonterm{dir} ---
       Writes output to subdirectories of @nonterm{dir}, which
       defaults to the current directory. All existing files and
       directories within @nonterm{dir} will be deleted. As a safety
       check, the destination directory must overlap with any
       installed package directory.}

 @item{@Flag{f} or @DFlag{force} --- Overwrite files in the destination
       directory.}

 @item{@PFlag{e} @nonterm{path} or @DPFlag{extra} @nonterm{path} ---
       Require the module @nonterm{path} before running
       @racket[build-all]. This flag can be used multiple times.}

]

@; ----------------------------------------

@section{Utilities}

@defproc[(basename [p path-string?]) string?]{

Extracts a file name from a path.}


@defproc[(web-path [str string?] ...) string?]{

Joins @racket[str]s with a @racket["/"] separator to form a relative
URL path.}
