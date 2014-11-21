#lang scribble/manual
@(require "common.rkt"
          scribble/bnf)

@title[#:tag "getting-started"]{Getting Started with Packages}

There are two ways to manage Racket package installations:

@itemlist[

 @item{The package manager graphical interface.

       Most users access the package manager graphical interface
       through DrRacket, which provides a @onscreen["Package
       Manager..."]  item in the @onscreen["File"] menu.

       You can also install the @pkgname{gui-pkg-manager} package,
       with provides a @onscreen{Racket Package Manager} application
       (which can be launched as @exec{racket-package-manager} in a
       command-line environment).}

 @item{The @exec{raco pkg} command-line tool.

      The @exec{raco} executable launches various Racket command-line
      tools, and the @exec{raco pkg} command groups various
      package-management sub-commands. The different @exec{raco pkg}
      sub-commands are documented in @secref["cmdline"].}

]

We'll use the @exec{raco pkg} command to describe package-management
operations here, but the graphical interface allows the same
operations.

@; ----------------------------------------

@section{What is a Package?}

A @tech{package} is not something that you refer to directly in your
Racket programs. Instead, a @tech{package} is a set of libraries that
fit into the @rtech{collection} hierarchy, and you refer to libraries
through their @rtech{collection}-based paths. Libraries that are close
in the hierarchy may be provided by different packages, while a single
package may provide libraries that are far from each other in the
hierarchy (but that are conceptually related, somehow).

Racket documentation tells you which package provides a given library.
For example, the documentation for the @racketmodname[pict/face]
library says that it is provided by the @pkgname{pict-lib}
package.@margin-note*{If you're reading this in a web browser, click
@racketmodname[pict/face] to go straight to its documentation.}

Over time, packages may be refactored so that a library moves to a
different package, but the original package should continue to provide
the library, too, by declaring a dependency on the new package. More
generally, a package is intended to have an interface that only grows
in terms of libraries, bindings, and functionality, which provides a
basic level of backward compatibility. Incompatible changes should be
implemented in a new package.

@; ----------------------------------------

@section{Inspecting Your Installation}

To see the packages that you have installed already, use the
@command-ref{show} subcommand:

@commandline{raco pkg show}

Unless you have an especially minimal Racket installation, you will
have packages installed already, probably listed in the
``Installation-wide'' section. In fact, if you have a typical Racket
installation, then @command-ref{show} will initially show a
@pkgname{main-distribution} package and a @pkgname{racket-lib}
package:

@verbatim[#:indent 2]{
 Installation-wide:
  Package              Checksum          Source
  main-distribution    01..........ef    (catalog main-distribution)
  racket-lib           fe..........01    (catalog racket-lib)
 User-specific for installation ....:
  [none]
}

The ``Checksum'' column reports the specific implementation of each
package that is installed. A package can have a @tech{version} in a
more traditional sense, but the @tech{checksum} is the ``version'' as
far as the package system is concerned. When you request an update,
then a package installation is updated if the current implementation
of the package has a different @tech{checksum} than the installed
package, whether or not the package author adjusted the package's
@tech{version}.

The ``Source'' column indicates how each package was installed. A
@tt{catalog} source indicates that the package was installed by
consulting a @tech{package catalog}. The name after @tt{catalog} indicates the
name of the package as requested from the catalog, which is normally
(but not necessarily) the name of the package as it exists in your
installation. We discuss other possibilities for ``Source'' in
@secref["installing-packages"].

Neither the @pkgname{main-distribution} package nor the
@pkgname{racket-lib} package actually provides any libraries on its own,
but each declares dependencies on other packages. The
@pkgname{racket-lib} package depends on native-library packages, if
any, for your platform. The @pkgname{main-distribution} package
depends on lots of packages that have been selected for inclusion in
the main Racket distribution. If you provide the @DFlag{all} flag to
@command-ref{show}, then you can see the packages that were
automatically installed as a result of installing
@pkgname{main-distribution} and @pkgname{racket-lib} (or whatever
packages you have explicitly selected for your installation).

@commandline{raco pkg show --all}

An asterisk appears beside the name of every package that was
``auto-installed'' to satisfy a dependency. All auto-installed
packages are as available for your use in the same way as explicitly
installed packages, but normally your code should refer only to
packages that you have explicitly installed. The difference between an
auto-installed and an explicitly installed package is how various
commands, such as @command-ref{show}, treat the package. If you
specifically request installation of a package that is
auto-installed, then the package is promoted and thereafter
treated as a explicitly installed package.

@; ----------------------------------------

@section[#:tag "finding-packages"]{Finding Packages}

The PLT @tech{package catalog} at

@centerline{@url{http://pkgs.racket-lang.org}}

provides a centralized listing of available Racket packages. The PLT
@tech{package catalog} normally will be the first place you check when
looking for a package.

There are other ways to distribute and reference packages. For
example, a package can be installed directly from a @filepath{.zip}
file---available locally or served from on a web site---or from a
Git repository. Such direct references make sense when a package is
not yet ready for wide distribution or when it will never be of
interest to a wide audience. So, you may find non-catalog references
in mailing-list posts, recommended by your friends, or advertised in
e-mail spam.

There may be other @tech{package catalog} services besides PLT's. Note
that even if you discover a package name from PLT's @tech{package
catalog}, your installation may be configured to consult a different
@tech{package catalog} to locate the package's implementation (to
obtain a pre-built version of the package, for example), but you
should expect the installation-configured @tech{package catalog} to
deliver the package that is described on the PLT @tech{package
catalog}.

@; ----------------------------------------

@section[#:tag "installing-packages"]{Installing Packages}

If you find a package by name from a @tech{package catalog}, then
use the package's name with @command-ref{install}:

@commandline{raco pkg install @nonterm{pkg-name}}

If the package depends on other packages that you do not have
installed already, then @command-ref{install} will alert you and ask
whether it should install them, too. Use @DFlag{auto} to
skip the question and make dependencies installed automatically.
Either way, packages installed to satisfy dependencies are marked as
auto-installed, which makes them easier to uninstall, and it also makes them hidden by
default for @command-ref{show} (since packages that are installed for
dependencies are an implementation detail that you usually do not care
about).

The argument that you provide to @command-ref{install} does not have
to be a package name that is recognized by a @tech{package
catalog}. In general, each argument to @command-ref{install} is a
@tech{package source}. A @tech{package source} can refer to a
@filepath{.zip} file, a @filepath{.tar} file, a Git repository, a
directory-structured web site, or a few other possibilities. In each
of those cases, a @tech{package name} is inferred from the
@tech{package source}. After the package is installed, you use the
package name with other @exec{raco pkg} commands to refer to the installed
package.

In fact, a @tech{package catalog} does not actually serve package
implementations. It simply maps each @tech{package name} to a
@tech{package source}. When the package manager consults a
@tech{package catalog}, it gets back a @tech{package source} for the
actual package implementation, so each package installed from a
@tech{package catalog} is actually installed from a @filepath{.zip}
file, Git repository, etc. Registering with a @tech{package
catalog} is just a way of making your package easier to find and
update.

@; ----------------------------------------

@section[#:tag "updating-packages"]{Updating Packages}

If your package installations become out of date, you can update
packages with @command-ref{update}:

@commandline{raco pkg update @nonterm{pkg-name}}

Either specify individual packages to update, or use @DFlag{all} to
update all installed packages for which a new @tech{checksum} is
available.

The way that the package manager finds updates depends on the way that
a package was installed. If it was installed by using a @tech{package
name} that was resolved by a @tech{package catalog}, then the
@tech{package catalog} is consulted again to get the current
@tech{checksum} for the package, and the package is updated if the
@tech{checksum} doesn't match the current installation. If the package
was installed directly from a Git reference, then the Git repository is
consulted to get the current commit of a particular branch or tag, and the
package is updated if the commit identifier doesn't match the
@tech{checksum} of the current installation.

In some cases, updating a package may require an update to one of the
package's dependencies. That should happen only when the package
requires a new binding, feature, or bug fix from the dependent
package, since packages are meant to evolve in an otherwise
backward-compatible way. Package @tech{versions} provide a way for
package authors to declare (and for the package manager to check)
those dependencies. The end result is that @command-ref{update} might
report a version-mismatch error that forces you to request more
package updates than you originally requested.

Normally, you provide @tech{package names} to
@command-ref{update}. More generally, you can provide a @tech{package
source} to @command-ref{update}. In that case, a package with the same
name must be installed already, and the installed package is replaced
with the specified one. Replacing a package with a new @tech{package
source} is a generalization of fetching a replacement package that has
a new @tech{checksum} at a previously specified source.

@; ----------------------------------------

@section[#:tag "removing-packages"]{Removing Packages}

As you might expect, @command-ref{remove} removes a package:

@commandline{raco pkg remove @nonterm{pkg-name}}

If the installation of a package triggered auto-installs of other
packages, then removing the package @emph{does not} automatically
remove the auto-installed packages. Supply the @DFlag{auto} flag for
@command-ref{remove}, either by itself or when uninstalling packages,
to also remove any auto-installed packages that are left without
dependents.

The @command-ref{remove} command will not remove a package if other
installed packages depend on it, unless you force the removal. If you want
to demote a package from explicitly installed to auto-installed (for
clean-up later, perhaps when other packages are removed), then
supply the @DFlag{demote} flag to @command-ref{remove}.

@; ----------------------------------------

@section[#:tag "how-to-create"]{Creating Packages}

A package normally starts life as a directory containing module files
and grows up to become a Git repository that is registered with a
@tech{package catalog}.

@subsection[#:tag "automatic-creation"]{Automatic Creation}

As a convenience, @command-ref{new} can automate the creation of
a @tech{single-collection package}.
To create @nonterm{pkg-name}:

@commandline{raco pkg new @nonterm{pkg-name}}

@subsection[#:tag "manual-creation"]{Manual Creation}

To create a package manually, first make a directory and select its name,
@nonterm{pkg-name}:

@commandline{mkdir @nonterm{pkg-name}}

Although a package can provide libraries in any number of
@rtech{collections}, it's common for a package to provide only
libraries in a collection that matches the package name. If that's the
case for your package, then files implementing modules in the
@nonterm{pkg-name} collection will go directly in the
@nonterm{pkg-name} directory that you have created.

If your package implements multiple @rtech{collections}, then you'll
need to add a basic @filepath{info.rkt} file in the
@nonterm{pkg-name} directory:

@commandline{cd @nonterm{pkg-name}}
@commandline{echo "#lang info" > info.rkt}
@commandline{echo "(define collection 'multi)" >> info.rkt}

The @racket[collection] definition tells the package manager that the
package implements libraries in multiple collections, and each
collection is represented by a sub-directory whose name matches the
collection name. Libraries for each collection go in the collection's
directory.

You can start with a @tech{single-collection package} and later change
it to a @tech{multi-collection package} by restructuring the package
directory, so you don't have to worry much about the choice when you
get started.

@subsection[#:tag "working-new-pkgs"]{Linking and Developing New Packages}

Whether creating a @tech{single-collection package} or a
@tech{multi-collection package}, the next step is to link your
development directory as a locally installed package. Use
@command-ref{install} in the @nonterm{pkg-name} directory:

@commandline{raco pkg install}

If you use @command-ref{show} at this point, you'll see a line for
@nonterm{pkg-name}. The ``Source'' column will show that it's a
linked package, and the ``Checksum'' column will say @tt{#f},
which means that there is no checksum. Sub-commands like
@command-ref{update} will not work on a linked package, because
``updates'' to the package happen whenever you modify the package's
implementation.

Finally, inside the @nonterm{pkg-name} directory, add directories
and/or files to implement the collections and/or modules that your
package provides. For example, the developer of a
@pkgname{tic-tac-toe} @tech{multi-collection package} that provides
@racketidfont{games/tic-tac-toe/main} and @racketidfont{data/matrix}
libraries might create directories and files like this:

@commandline{mkdir -p games/tic-tac-toe}
@commandline{touch games/tic-tac-toe/info.rkt}
@commandline{touch games/tic-tac-toe/main.rkt}
@commandline{mkdir -p data}
@commandline{touch data/matrix.rkt}

An @filepath{info.rkt} file is not necessary for a
@tech{single-collection package} with no dependencies, but you may
wish to create one, anyway, to hold dependency declarations. Every
package at least depends on @pkgname{base}, which provides the
collections and libraries of a minimal Racket installation. To make
your package work best for other users, you will ultimately need to
declare all dependencies. (Fortunately, @exec{raco setup} can check
dependencies and help you figure out what dependencies to declare.)

Even for a @tech{single-collection package}, you may want to create
@filepath{info.rkt} and include the definition

@racketblock[(define collection @#,racketvalfont{"}@#,nonterm{pkg-name}@#,racketvalfont{"})]

This definition may seem redundant, since @nonterm{pkg-name} is
available as the name of the enclosing directory, but declaring the
collection name explicitly prevents the meaning of your package's implementation
from depending on the way that the implementation is referenced.

Finally, in the case of a @tech{multi-collection package}, note that
the @filepath{info.rkt} file in @nonterm{pkg-name} is for the
package, not for a collection. Definitions such as
@racket[scribblings] or @racket[raco-commands] work only in a
collection's @filepath{info.rkt}. For a @tech{single-collection
package}, the @filepath{info.rkt} file serves double-duty for the
package and collection.

@; ----------------------------------------

@section[#:tag "how-to-share"]{Sharing Packages}

After your package is ready to deploy, choose either @secref["github-deploy"]
or @secref["manual-deploy"], and then go on to @secref["register-at-catalog"].

@; - - - - - - - - - - - - - - - - - - - - - - - -

@subsection[#:tag "github-deploy"]{GitHub Deployment}

First, @link["https://github.com/signup/free"]{create a free account}
on GitHub, then
@link["https://help.github.com/articles/create-a-repo"]{create a
repository for your package}. After that, your @tech{package source}
is:

@inset{@exec{https://github.com/@nonterm{user}/@nonterm{package}.git}}

If you want the package to be @nonterm{branch} or @nonterm{tag}
instead of @exec{master}, then add @filepath{#@nonterm{branch}} or
@filepath{#@nonterm{tag}} to the end of the package source.

Whenever you

@commandline{git push}

your changes will automatically be discovered by those who use
@command-ref{update} after installing from your
GitHub-based @tech{package source}.

As of Racket version 6.1.1.1, other Git repository services can work
just as well as GitHub---including Gitorious or BitBucket---as long as
the server supports either the ``smart'' HTTP(S) protocol or the
native Git protocol (but use a @exec{git://} path for the latter).

The Racket package manager provides more support for Git-based
development than just deployment. See @secref["git-workflow"] for more
information.

@; - - - - - - - - - - - - - - - - - - - - - - - -

@subsection[#:tag "manual-deploy"]{Manual Deployment}

Alternatively, you can deploy your package by publishing it on a URL
you control. If you do this, it is preferable to create an archive
from your package directory first:

@commandline{raco pkg create @nonterm{package}}

Then, upload the archive and its @tech{checksum} to your site:

@commandline{scp @nonterm{package}.zip @nonterm{package}.zip.CHECKSUM your-host:public_html/}

Your @tech{package source} is then something like

@inset{@exec{http://your-host/~@nonterm{user}/@nonterm{package}.zip}}

Whenever you want to provide a new release of a package, recreate and reupload the
package archive (and @tech{checksum}). Your changes will automatically be
discovered by those who used your package source when they use
@command-ref{update}.

@margin-note{By default, @command-ref{create} generates a
@filepath{.zip} archive. For more options, refer to the
@command-ref{create} documentation. If you want to generate an archive
through some other means, simply archive what you made in the first
part of this section. For more formal details, refer to the
@tech{package} definition.}

@; - - - - - - - - - - - - - - - - - - - - - - - -

@subsection[#:tag "register-at-catalog"]{Helping Others Discover Your Package}

By using either @secref["github-deploy"] or @secref["manual-deploy"],
anyone will be able to install your package by referring to your
@tech{package source}.  However, they will not be able to refer to it
by a simple name until it is listed on a @tech{package catalog}.

If you'd like to use the PLT @tech{package catalog}, browse
to
@link["http://pkgs.racket-lang.org/"]{http://pkgs.racket-lang.org/}
and upload a new package. You will need to create an account and log
in first.

You only need to go to this site @emph{once} to list your package. The
server will periodically check the package source you designate for
updates.

If you use this server, and if you use a public Git repository for
deployment, then you will never need to open a web browser to update
your package for end users. You just need to push to your Git
repository, then within 24 hours, the PLT @tech{package catalog} will
notice, and @command-ref{update} will work on your user's machines.

@; - - - - - - - - - - - - - - - - - - - - - - - -

@subsection{Naming and Designing Packages}

We suggest the following conventions for naming and designing
packages:

@itemlist[

@item{Packages should not include the name of the author or
organization that produces them, but be named based on the content of
the package. For example, @pkgname{data-priority-queue} is preferred
to @pkgname{johns-amazing-queues}.}

@item{Packages that provide an interface to a foreign library or
service should be named the same as the service. For example,
@pkgname{cairo} is preferred to @pkgname{Racket-cairo} or a similar
name.}

@item{Packages should not generally contain version-like elements in
their names, initially. Instead, version-like elements should be added
when backwards incompatible changes are necessary. For example,
@pkgname{data-priority-queue} is preferred to
@pkgname{data-priority-queue1}. Exceptions include packages that
present interfaces to external, versioned things, such as
@pkgname{sqlite3} or @pkgname{libgtk2}.}

@item{A @tech{version} declaration for a package is used only by other
package implementors to effectively declare dependencies on provided
features. Such declarations allow @command-ref{install} and
@command-ref{update} to help check dependencies.  Declaring and
changing a version is optional, and the @tech{package catalog}
ignores version declarations; in particular, a package is a candidate
for updating when its @tech{checksum} changes, independent of whether
the package's version changes or even in which direction the version
changes. We suggest using a version smaller than @racket["1.0"] to 
indicate that a package's interface is unstable and changing it to
@racket["1.0"] when you are ready to commit to backwards compatibility
going forward.}

@item{Packages should not combine large sets of utilities libraries
with other functionality. For example, 
a package that contain many extensions to the @filepath{racket} collection, like
@filepath{racket/more-lists.rkt} and
@filepath{racket/more-bools.rkt} 
should not also contain complete applications, as other packages
interested in the @filepath{racket/more-bools.rkt} library
will not wish to depend on in such application.}

@item{Packages should normally include both documentation and
implementation. To make the implementation of a package available
separately from its documentation (for use in environments where local
documentation is not useful), define a package
@pkgname{@nonterm{pkg-name}-lib} to hold just the implementation,
@pkgname{@nonterm{pkg-name}-doc} to hold the documentation, and
@pkgname{@nonterm{pkg-name}} that depends on both and that
``re-exports'' both with an @racketidfont{implies} declaration (see
@secref["metadata"]). If you want to keep tests separate, put them a
@pkgname{@nonterm{pkg-name}-test} package that is @emph{not} a
dependency of @pkgname{@nonterm{pkg-name}}. Similarly, use
@pkgname{@nonterm{pkg-name}-exe} for executables.}

@item{Packages should generally provide one collection with a name
similar to the name of the package. For example, @pkgname{libgtk1}
should provide a collection named @filepath{libgtk}. Exceptions
include extensions to existing collection, such as new data-structures
for the @filepath{data} collection, DrRacket tools, new games for PLT
Games, etc.}

]

@; - - - - - - - - - - - - - - - - - - - - - - - -

@subsection{Packages Compatible with Racket 5.3.5 and 5.3.6}

A beta version of the package system was added to Racket starting in
version 5.3.5. By the time version 6.0 was released, some features
were added.

By using only certain older features, it is possible to make a package
that can be used with Racket versions 5.3.5, 5.3.6, 6.0, and newer.

In your @racket[info.rkt], you should:

@itemlist[

  @item{Use @tt{#lang setup/infotab} (not @tt{#lang info)}.}

  @item{Use @racket[(define collection 'multi)]. Even if your package
  has a single collection, put it in a subdirectory and make a
  multi-collection package.}

  @item{If you depend on a specific version of another package, state
  this using the @racket[(_other-package-name _required-version)]
  form (not the form with @racket[#:version]).}

]

Finally, when listing your package on
@url{http://pkgs.racket-lang-org}, you should supply a GitHub source
using the URL format
@tt{github://github.com/@nonterm{user}/@nonterm{repo}/@nonterm{rev}@optional{/@nonterm{path}}} (not
the @tt{git://} or @exec{http://} format).

@subsubsection{Version Exceptions}

To make supporting multiple versions of Racket easier, the @tech{package
catalog} software supports @deftech{version exception}s. Version exceptions
allow package authors to specify alternative @tech{package source}s to be used
when installing a given package using a specific version of Racket.

For example, a package that uses on Racket 6.0-specific features could provide
a @tech{version exception} for Racket 5.3.6 using a different branch or tag in the
package's GitHub repository, or a different zip archive, as package source.
Users installing the package from Racket 6.0 will use the default source for
the package, while those using Racket 5.3.5 will install from the alternative
branch, tag, or archive.

For more details, see @secref{catalog-protocol}.
