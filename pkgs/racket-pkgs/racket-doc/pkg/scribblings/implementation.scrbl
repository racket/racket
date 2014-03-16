#lang scribble/manual
@(require "common.rkt"
          scribble/bnf
          (for-label racket/base
                     setup/collects
                     setup/main-collects
                     setup/path-to-relative
                     setup/dirs
                     (only-in scribble/manual secref)
                     (only-in scribble/base-render render%)
                     (only-in scribble/html-render render-mixin)
                     (only-in scribble/xref load-xref make-data+root)))

@(define ref-doc '(lib "scribblings/reference/reference.scrbl"))
@(define raco-doc '(lib "scribblings/raco/raco.scrbl"))
@(define scribble-doc '(lib "scribblings/scribble/scribble.scrbl"))

@title[#:tag "implementation"]{How Package Installation and Distribution Works}

The package manager builds on three main pieces of infrastructure:

@itemlist[

 @item{@tech[#:doc ref-doc]{Collection links files} as supported by the
       Racket runtime system.

       Installation of a package installs collection links, so the
       package's collections can be found to compile and load modules
       that use the package's modules.

       If you use @seclink["link" #:doc raco-doc]{@exec{raco link -l}}
       to view installed links, you will see links that were put in
       place by the package system. Obviously, you should not directly
       modify those links.}

 @item{The @seclink["setup" #:doc raco-doc]{@exec{raco setup}} tool for
       building installed collections, including their documentation.

       The @exec{raco setup} tool drives @seclink["make" #:doc
       raco-doc]{@exec{raco make}} to compile Racket sources to
       bytecode form. Recompilation is determined by changes to file
       timestamps, SHA-1 hashes, and dependencies recorded in
       @filepath{dep} files.

       Since package installations are reflected as collection links,
       @exec{raco setup} operations on collections implicitly handle
       packages.  The @exec{raco setup} tool is ``aware'' of packages
       to only a limited extent: it uses functions like
       @racket[path->pkg] to print progress information in terms of
       packages, and it uses similar package-inspection functions to
       connect modules to package and check actual dependencies
       against declared package dependencies.}

 @item{The @pkgname{racket-index} package, which extends @exec{raco
       setup} to drive @seclink["top" #:doc scribble-doc]{Scribble} for
       collection-based documentation.

       The @pkgname{racket-index} package implements the
       documentation-rendering analogue of @exec{raco make}, detecting
       changes in documentation declarations and re-rendering
       documents as needed to pick up cross-reference changes. The
       @pkgname{racket-index} package also implements special
       documents for the entry point to HTML-rendered documentation
       (i.e., the listing of all installed documentation), the HTML
       search page, the local-redirection page (which server-search
       links to locally installed files), and so on.}

]

Each of the three levels accommodate the @exec{user} and
@exec{installation} @tech{package scopes}, where the details in each
case often differ between the scopes. Generally, references in the
@exec{installation} scope must be implemented as relative, so that an
in-place installing of Racket can be moved to a different
location. References in the @exec{user} scope, meanwhile, may refer
directly to the installation at some level; most references are
collection-relative or installation-relative, so package content
can be built in @exec{user} scope and then assembled into a @tech{built
package} or @tech{binary package} for installation elsewhere.

@section{Relative References}

Functions like @racket[path->collects-relative] and
@racket[path->main-collects-relative] are used to serialize paths into
relative form, and then the paths can be deserialized with functions
like @racket[collects-relative->path] and
@racket[main-collects-relative->path]. The
@racket[make-path->relative-string] function generalizes support for
such serialization and deserialization relative to a given set of
directories.

Dependencies in a @exec{raco make}-generated @filepath{.dep} file use
collection-relative paths whenever possible, and it should always be
possible for dependencies within a collection. Similarly,
cross-reference information for documentation uses collection-relative
paths when possible.

In a @tech[#:doc ref-doc]{collection links file}, paths are relative
to the link file's directory. Installation-wide links then work when
an in-place installation is moved.

In cross-reference information for documentation that
installation-wide, paths can be stored relative to the installation's
@filepath{doc} directory. For documentation that is built in user
scope, cross-reference information within the built document is
recorded relative to the document's directory via the
@racket[root-path] initialization argument to @racket[render%]; the
cross-reference information can be unpacked to a different
destination, where the use-time path is provided the @racket[#:root]
argument to @racket[load-xref] and/or @racket[make-data+root]
structures.


@section{Separate Documentation Rendering}

Unlike module references, which must create no reference cycles,
documentation can have reference cycles. Documentation also tends to
be less compact than code, and while we attempt to minimize module
dependencies in code, documentation should freely reference any other
documentation that is relevant. Finally, documentation references are
less static than module references; for example, a document references
@racket[cons] by referring to @racketmodname[racket/base], and the
documentation system must figure out which other document defines
@racket[cons]. A naive implementation of documentation rendering would
load all documents to render any one document, which is prohibitively
expensive in both time and space.

Scribble supports separate document rendering by marshaling and
unmarshaling cross-reference information. The @pkgname{racket-index}
extension of @exec{raco setup} stores a document's information in
@filepath{.sxref} files. Some documents, such as the @seclink["top"
#:doc ref-doc]{reference}, export a large volume of cross-reference
information, so @exec{raco setup} breaks up a document's exported
cross-reference information into multiple
@filepath{out@nonterm{n}.sxref} files. Information about ``imported''
cross-reference information---that is, the cross references that were
used the last time a document was built---is kept in
@filepath{in.sxref} files. Finally, to detect which
@filepath{out@nonterm{n}.sxref} files need to be loaded while building
a document, a mapping of cross-reference keys to
@filepath{out@nonterm{n}.sxref} files is kept in a SQLite database,
@filepath{docindex.sqlite}. Lazy loading of
@filepath{out@nonterm{n}.sxref} files is implemented though the
@racket[#:demand-source] argument to @racket[load-xref], providing a
function that consults @filepath{docindex.sqlite} to map a key to a
cross-reference file.

Various kinds of paths within cross-reference files are stored with
various relative-path conventions. The @filepath{docindex.sqlite} file
in an installation can be moved unmodified with the installation. The
@filepath{docindex.sqlite} file for user-scoped packages is
non-portable (and outside any package), while the @filepath{in.sxref}
and @filepath{out@nonterm{n}.sxref} files can be included as-is in a
@tech{binary package} or @tech{built package}.


@section{Cross-Document HTML References}

The HTML generated for a Scribble document needs relative
links. Unlike data that is unmarshaled by Racket code, however, there
is no way to turn paths that are relative to various installation
directories into paths that a browser understands---at least, not
using only HTML. Generated HTML for documentation therefore relies on
JavaScript to rewrite certain references, with a fallback path through
a server to make documentation also work as plain HTML.

References within a single document are rendered as relative links in
HTML. A reference from one document to another is rendered as a query
to, say, @url{http://docs.racket-lang.org/}. However, every document
also references @filepath{local-redirect.js} and (in the case of
documentation for user-specific collections)
@filepath{local-user-redirect.js}. Those fragments of JavaScript
dynamically rewrite query references to direct filesystem
references---to installation-wide and user-specific targets,
respectively---when local targets are available. When local targets
are not available, the query link is left unmodified to go through a
server.

The @filepath{local-redirect.js} and @filepath{local-user-redirect.js}
files map documentation-directory names to specific paths. Most query
references contain a documentation-directory name and a relative path
within the directory, in which case the mapping from directory names
to paths is sufficient. Indirect links, such as those created by
@racket[(seclink #:indirect? #t ...)], embed a cross-reference key,
and so @filepath{local-redirect.js} and
@filepath{local-user-redirect.js} must also embed a part of the
cross-reference database. (This copy of the database is broken into
multiple files, each of which is loaded on demand.) The
@filepath{local-redirect.js} and @filepath{local-user-redirect.js}
files are generated as part of the special @filepath{local-redirect}
document that is implemented by the @pkgname{racket-index} package.

The indirection through @filepath{local-redirect.js} and
@filepath{local-user-redirect.js} reduces the problem of relative
links to the problem of referencing those two files. They are
referenced as absolute paths in a user-specific document build. To
create a @tech{built package} or @tech{binary package} that includes
documentation, each @filepath{.html} file must be modified to remove
the absolute paths, and then each @filepath{.html} file must be
modified again on installation to put the target installation's paths
in path.

The @pkgname{racket-index} package's extension of @exec{raco setup} to
build Scribble documentation puts these indirections in place using
the @racket[set-external-tag-path] method of @racket[render-mixin]
from @racketmodname[scribble/html-render]. The
@url{http://docs.racket-lang.org/} path is not hardwired, but
instead based on the installation's configuration as reported by
@racket[get-doc-search-url]. That configuration, in turn, can be
determined when building a Racket distribution; the main distributions
from PLT set the URL to a version-specific site, so that searches work
even after new Racket versions are released, while snapshots similarly
set the URL to a snapshot-specific site.


@section{HTML Documentation Searching and Start Page}

The @pkgname{racket-index} package provides a special document to
implement the initial page for installed HTML documentation. The
document uses @filepath{info.rkt}-file @racket[scribblings] flags to
depend on all documents for their titles.

The @pkgname{racket-index} package also provides a special document to
implement searching. The search document uses JavaScript and a copy of
the cross-reference database (similar to @filepath{local-redirect.js})
to implement interactive searching.

If any user-specific collections have been installed, then
@pkgname{racket-index} generates two copies of the start and search
documents: one for the installation, and one specific to the user. The
user pages are an extension of the installation pages. The
user-specific search page reads the installation-wide search page's
database, which both avoids duplication and allows the search to pick
up any additions to the installation without requiring a rebuild of
the user-specific search page. The user-specific start page, in
contrast, must be rebuilt after any installation-wide additions to
pick up the additions.

When DrRacket or @exec{raco docs} opens documentation in a browser, it
opens the user-specific start or search page, if it exists. If those
pages are visited for any reason, browser local storage or (if local
storage is not supported) a cookie is installed. The local-storage key
or cookie is named ``PLT_Root.@nonterm{version},'' it points to the
location of the user-specific documentation. Thereafter, using the
local value of cookie, searching in any documentation page or going to
the ``top'' page goes to the user-specific page, even from an
installation-wide page.
