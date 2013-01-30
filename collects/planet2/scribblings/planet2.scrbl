#lang scribble/manual
@(require scribble/bnf
          scribble/core
          (for-label planet2
                     (except-in racket/base remove)
                     setup/dirs))

@(define @|Planet1| @|PLaneT|)

@(define pkgname onscreen)
@(define reponame litchar)

@(define package-name-chars
   @list{@litchar{a} through @litchar{z}, 
         @litchar{A} through @litchar{Z}, 
         @litchar{0} through @litchar{9}, 
         @litchar{_}, and @litchar{-}})

@(define (inset . c)
   (cons (hspace 2) c))

@(define (gtech s)
   @tech[#:doc '(lib "scribblings/guide/guide.scrbl") s])

@(define (command s)
   @exec{raco pkg @|s|})

@(define (command-ref s)
   @(link-element "plainlink" @command[s] `(raco-pkg-cmd ,s)))

@(define (command/toc s)
   @(toc-target-element #f @command[s] `(raco-pkg-cmd ,s)))
                        

@; ----------------------------------------

@title{Package Management in Racket (Beta)}
@author[@author+email["Jay McCarthy" "jay@racket-lang.org"]]

The Racket package manager lets you install new libraries and
collections, and the Racket package sever helps other Racket
programmers find libraries that you make available.

@table-of-contents[]

@; ----------------------------------------

@section{Package Concepts}

A @deftech{package} is a set of modules in some number of
@gtech{collections}. Modules installed using the Racket package
manager are @racket[require]d like any other modules. For example, if
the package @pkgname{tic-tac-toe} contains the module
@filepath{matrix.rkt} in a @filepath{data} collection, then after
@pkgname{tic-tac-toe} is installed,

@racketblock[(require data/matrix)]

imports the module. The package name is not mentioned with
@racket[require], because packages are a way of managing library
collections, not a way of referencing them. It is common, however, for
a package to implement a collection whose name is the same as the
package name---in which case a @racket[require] might appear to be
referencing a @tech{package}, but it is actually referencing a
@gtech{collection} provided by the @tech{package}.

Each @tech{package} has associated @deftech{package metadata}:

@itemlist[
 @item{a @deftech{package name} --- a string made of the characters @|package-name-chars|.}
 @item{a @deftech{checksum} --- a string that identifies different releases of a package. A
                                package can be updated when its @tech{checksum} changes,
                                whether or not its @tech{version} changes.}
 @item{a @deftech{version} --- a string of the form @nonterm{maj}@litchar{.}@nonterm{min},
                     @nonterm{maj}@litchar{.}@nonterm{min}@litchar{.}@nonterm{sub}, or
                     @nonterm{maj}@litchar{.}@nonterm{min}@litchar{.}@nonterm{sub}@litchar{.}@nonterm{rel},
                     where @nonterm{maj}, @nonterm{min}, @nonterm{sub}, and @nonterm{rel} are
                     all canonical decimal representations of natural numbers, @nonterm{min} has no more
                     than two digits, and @nonterm{sub} and @nonterm{rel} has no more than
                     three digits. A version is intended to reflect available features of
                     a package, and should not be confused with different releases of
                     a package as indicated by the @tech{checksum}.}
 @item{a list of dependencies --- a list of packages to be installed simultaneously, optionally
                                 with a lower bound on each package's @tech{version}.}
]

A @tech{package} is typically represented by a directory with the same
name as the package. The @tech{checksum} is typically left implicit.
The package directory can contain a file named @filepath{info.rkt}
to declare other metadata (see @secref["metadata"]).

A @deftech{package source} identifies a @tech{package}
representation. Each package source type has a different way of
storing the @tech{checksum}. The valid package source types are:

@itemlist[

@item{a local file path naming an archive -- The name of the package
is the basename of the archive file. The @tech{checksum} for archive
@filepath{f.@nonterm{ext}} is given by the file @filepath{f.@nonterm{ext}.CHECKSUM}. For
example, @filepath{~/tic-tac-toe.zip}'s @tech{checksum} would be inside
@filepath{~/tic-tac-toe.zip.CHECKSUM}. The valid archive formats
are (currently) @filepath{.zip}, @filepath{.tar}, @filepath{.tgz}, 
@filepath{.tar.gz}, and
@filepath{.plt}.

A package source is inferred to refer to a file
only when it has a suffix matching a valid archive format
and when it does not start
with alphabetic characters followed by @litchar{://}. The inferred
package name is the filename without its suffix.}

@item{a local directory -- The name of the package is the name of the
directory. The @tech{checksum} is not present. For example,
@filepath{~/tic-tac-toe/} is directory package source.

A package source is inferred to refer
to a directory only when it does not have a file-archive suffix, does
not match the grammar of a package name, and does not start
with alphabetic characters followed by @litchar{://}. The inferred
package name is the directory name.}

@item{a remote URL naming an archive -- This type follows the same
rules as a local file path, but the archive and @tech{checksum} files are
accessed via HTTP(S). For example,
@filepath{http://game.com/tic-tac-toe.zip} is a remote URL package
source whose @tech{checksum} is found at
@filepath{http://game.com/tic-tac-toe.zip.CHECKSUM}.

A package source is inferred to be a URL only when it
starts with @litchar{http://} or @litchar{https://}, and it
is inferred to be a file URL when the URL ends with a path element
that could be inferred as a file archive.
The inferred package name is from the URL's file name in the same
way as for a file package source.}

@item{a remote URL naming a directory -- The remote directory must
contain a file named @filepath{MANIFEST} that lists all the contingent
files. These are downloaded into a local directory and then the rules
for local directory paths are followed. However, if the remote
directory contains a file named @filepath{.CHECKSUM}, then it is used
to determine the @tech{checksum}. For example,
@filepath{http://game.com/tic-tac-toe/} is a directory URL package
source whose @tech{checksum} is found at
@filepath{http://game.com/tic-tac-toe/.CHECKSUM}.

A package source is inferred to be a URL the same for a directory or
file, and it is treated as a directory URL when it does not end with a
path element that has an archive file suffix. The inferred package name
is the directory name.}

@item{a remote URL naming a GitHub repository -- The format for such
URLs is:

@inset{@exec{github://github.com/}@nonterm{user}@exec{/}@nonterm{repository}@;
@exec{/}@nonterm{branch-or-tag}@exec{/}@nonterm{optional-subpath}}

For example, @filepath{github://github.com/game/tic-tac-toe/master/}
is a GitHub package source.

The @exec{zip}-formatted archive for the repository (generated by
GitHub for every branch and tag) is used as a remote URL archive path,
except the @tech{checksum} is the hash identifying the branch (or
tag).

A package source is inferred to be a GitHub reference when it
starts with @litchar{github://}; a package source that is otherwise
specified as a GitHub reference is automatically prefixed with
@filepath{github://github.com/}. The inferred package name
is the last element of @nonterm{optional-subpath} if it is
non-empty, otherwise the inferred name is @nonterm{repository}.}

@item{a @tech{package name} -- A @tech{package name resolver} is
consulted to determine the source and @tech{checksum} for the package. For
example, @exec{tic-tac-toe} is a package name that can be used as a
package source.

A package source is inferred
to be a package name when it fits the grammar of package names, which
means that it has only the characters @|package-name-chars|.}

]

A @deftech{package name resolver} (@deftech{PNR}) is a server that
converts package names to other package sources. A PNR is identified
by a string representing a URL. This URL is combined with
@exec{pkg/}@nonterm{package} path segments (where @nonterm{package} is a package name) plus a
@exec{version=}@nonterm{version} query (where @nonterm{version} is the
Racket version number) to form a URL that should refer to a
@racket[read]-able hash table with the keys: @racket['source] mapped to
the @tech{package source} string and @racket['checksum] mapped to the
@tech{checksum} value. Typically, the @tech{package source} value for
@racket['source] will be a remote URL.

PLT supports two @tech{package name resolvers} that are enabled by
default: @url{https://pkg.racket-lang.org} for new
packages and @url{https://planet-compat.racket-lang.org} for
automatically generated packages for old @|PLaneT|
packages. Anyone may host their own @tech{package name resolver}. The
source for the PLT-hosted resolvers is in the
@racket[(build-path (find-collects-dir) "meta" "planet2-index")]
directory.

After a package is installed, the original source of its installation
is recorded, as well as if it was an @tech{automatic installation}. An
@deftech{automatic installation} is one that was installed because it
was a dependency of a non-@tech{automatic installation} package.

Two packages are in @deftech{conflict} if they contain the same
module. For example, if the package @pkgname{tic-tac-toe} contains the
module file @filepath{data/matrix.rkt} and the package
@pkgname{factory-optimize} contains the module file
@filepath{data/matrix.rkt}, then @pkgname{tic-tac-toe} and
@pkgname{factory-optimize} are in conflict. A package may also be in
conflict with Racket itself, if it contains a module file that is part
of the core Racket distribution. For example, any package that
contains @filepath{racket/list.rkt} is in conflict with Racket. For
the purposes of conflicts, a module is a file that ends in
@filepath{.rkt} or @filepath{.ss}.

Package A is a @deftech{package update} of Package B if (1) B is
installed, (2) A and B have the same name, and (3) A's @tech{checksum} is
different than B's. Note that a package @tech{version} is not taken
into account when determining a @tech{package update}, although a change
in a package's @tech{version} (in either direction) should normally
imply a change in the @tech{checksum}.

A @deftech{package scope} determines the effect of package installations,
updates, @|etc|, with respect to different users, Racket versions, and
Racket installations. The default @tech{package scope} can be configured, but it is
normally @exec{user}, which is user-specific and version-specific;
that is, package installation makes the package visible only for the
installing user and with the installing version of Racket. The
@exec{installation} scope means that package installation makes the
package visible to all users of the specific Racket installation that
is used to install the package. Finally, the @exec{shared} scope means
user-specific, but for all versions and installations of Racket.

@; ----------------------------------------

@section{Managing Packages}


The Racket package manager has two user interfaces: a command line @exec{raco}
sub-command and a library. They have the exact same capabilities, as
the command line interface invokes the library functions and
reprovides all their options.

@subsection[#:tag "cmdline"]{Command Line}

The @as-index{@exec{raco pkg}} sub-command provides the following
sub-sub-commands:

@itemlist[

@item{@command/toc{install} @nonterm{option} ... @nonterm{pkg-source} ... 
 --- Installs the given @tech{package sources} with the given
 @nonterm{option}s:

 @itemlist[

 @item{@DFlag{type} @nonterm{type} or @Flag{t} @nonterm{type} --- specifies an interpretation of the package source,
       where @nonterm{type} is either @exec{file}, @exec{dir}, @exec{url}, @exec{github}, 
       or @exec{name}.}

 @item{@DFlag{name} @nonterm{pkg} or @Flag{n} @nonterm{pkg} --- specifies the name of the package,
       which makes sense only when a single @nonterm{pkg-source} is provided. The name is normally
       inferred for each @nonterm{pkg-source}.}

 @item{@DFlag{deps} @nonterm{behavior} --- Selects the behavior for dependencies, where @nonterm{behavior} is one of
  @itemlist[
   @item{@exec{fail} --- Cancels the installation if dependencies are version requirements are unmet (default for most packages)}
   @item{@exec{force} --- Installs the package(s) despite missing dependencies or version requirements (unsafe)}
   @item{@exec{search-ask} --- Looks for the dependencies or updates via the configured @tech{package name resolvers} 
         (default if the dependency is an indexed name) but asks if you would like it installed or updated.}
   @item{@exec{search-auto} --- Like @exec{search-ask}, but does not ask for permission to install or update.}
  ]}

  @item{@DFlag{force} --- Ignores conflicts (unsafe)}

  @item{@DFlag{ignore-checksums} --- Ignores errors verifying package @tech{checksums} (unsafe).}

 @item{@DFlag{no-setup} --- Does not run @exec{raco setup} after installation. This behavior is also the case if the
       environment variable @envvar{PLT_PLANET2_NOSETUP} is set to any non-empty value.}

  @item{@DFlag{link} --- Implies @exec{--type dir} (and overrides any specified type),
        and links the existing directory as an installed package.}

 @item{@DFlag{scope} @nonterm{scope} --- Selects the @tech{package scope} for installation, where @nonterm{scope} is one of
  @itemlist[
   @item{@exec{installation} --- Install packages for all users of a Racket installation, rather than user-specific.}
   @item{@exec{user} --- Install packages as user-specific and Racket version-specific.}
   @item{@exec{shared} --- Install packages as user-specific, but for all Racket versions.}
  ]
  The default package scope is normally @exec{user}, but it can be configured with
  @command-ref{config}@exec{ -i --set default-scope @nonterm{scope}}.}
 @item{@Flag{i} or @DFlag{installation} --- Shorthand for @exec{--scope installation}.}
 @item{@Flag{u} or @DFlag{user} --- Shorthand for @exec{--scope user}.}
 @item{@Flag{s} or @DFlag{shared} --- Shorthand for @exec{--scope shared}.}
 ]
}


@item{@command/toc{update} @nonterm{option} ... @nonterm{pkg} ... 
--- Checks the specified packages for
@tech{package updates}. If an update is found, but it cannot be
installed (e.g. it is conflicted with another installed package), then
this command fails atomically. The @exec{update} sub-command accepts 
the following @nonterm{option}s:

 @itemlist[
 @item{@DFlag{deps} @nonterm{behavior} --- Same as for @command-ref{install}.}
 @item{@DFlag{all} or @Flag{a} --- Update all packages, if no packages are given in the argument list.}
 @item{@DFlag{update-deps} --- Checks the named packages, and their dependencies (transitively) for updates.}
 @item{@DFlag{no-setup} --- Same as for @command-ref{install}.}
 @item{@DFlag{scope} @nonterm{scope} --- Selects a @tech{package scope}, the same as for @command-ref{install}.}
 @item{@Flag{i} or @DFlag{installation} --- Shorthand for @exec{--scope installation}.}
 @item{@Flag{u} or @DFlag{user} --- Shorthand for @exec{--scope user}.}
 @item{@Flag{s} or @DFlag{shared} --- Shorthand for @exec{--scope shared}.}
 ]
}

@item{@command/toc{remove} @nonterm{option} ... @nonterm{pkg} ... 
--- Attempts to remove the given packages. If a package is the dependency of another package that is not 
listed, this command fails atomically. It accepts the following @nonterm{option}s:

 @itemlist[
 @item{@DFlag{force} --- Ignore dependencies when removing packages.}
 @item{@DFlag{no-setup} --- Same as for @command-ref{install}.}
 @item{@DFlag{auto} --- Remove packages that were installed by the @exec{search-auto} or @exec{search-ask}
                        dependency behavior and are no longer required.}
 @item{@DFlag{scope} @nonterm{scope} --- Selects a @tech{package scope}, the same as for @command-ref{install}.}
 @item{@Flag{i} or @DFlag{installation} --- Shorthand for @exec{--scope installation}.}
 @item{@Flag{u} or @DFlag{user} --- Shorthand for @exec{--scope user}.}
 @item{@Flag{s} or @DFlag{shared} --- Shorthand for @exec{--scope shared}.}
 ]
}

@item{@command/toc{show} @nonterm{option} ... --- Print information about currently installed packages. 
 By default, packages are shown for all installation modes (installation-wide,
 user- and Racket-version-specific, and user-specific all-version).
 The command accepts the following @nonterm{option}s:

 @itemlist[

 @item{@DFlag{scope} @nonterm{scope} --- Shows only packages in @nonterm{scope}, which is one of
  @itemlist[
   @item{@exec{installation} --- Show only installation-wide packages.}
   @item{@exec{user} --- Show only user-specific, version-specific packages.}
   @item{@exec{shared} --- Show only user-specific, all-version packages.}
  ]
  The default is to show packages for all @tech{package scopes}.}
 @item{@Flag{i} or @DFlag{installation} --- Shorthand for @exec{--scope installation}.}
 @item{@Flag{u} or @DFlag{user} --- Shorthand for @exec{--scope user}.}
 @item{@Flag{s} or @DFlag{shared} --- Shorthand for @exec{--scope shared}.}
 @item{@DFlag{version} @nonterm{vers} or @Flag{v} @nonterm{vers} --- Show only user-specific packages for Racket version @nonterm{vers}.}
 ]
}

@item{@command/toc{config} @nonterm{option} ... @nonterm{key} @nonterm{val} ... --- 
View and modify package configuration options. It accepts the following @nonterm{option}s:

 @itemlist[
 @item{@DFlag{set} --- Sets an option, rather than printing it.}
 @item{@DFlag{scope} @nonterm{scope} --- Selects a @tech{package scope}, the same as for @command-ref{install}.}
 @item{@Flag{i} or @DFlag{installation} --- Shorthand for @exec{--scope installation}.}
 @item{@Flag{u} or @DFlag{user} --- Shorthand for @exec{--scope user}.}
 @item{@Flag{s} or @DFlag{shared} --- Shorthand for @exec{--scope shared}.}
 ]

 The valid keys are:
 @itemlist[
  @item{@exec{indexes} --- A list of URLs for @tech{package name resolvers}.}
  @item{@exec{default-scope} --- Either @exec{installation}, @exec{user}, or @exec{shared}.
        This configuration option exists only with the @exec{installation} scope
        (i.e., it's an installation-wide configuration of the default @tech{package scope} for @exec{raco pkg} commands).}
 ]
}

@item{@command/toc{create} @nonterm{option} ... @nonterm{package-directory}
--- Bundles a package. It accepts the following @nonterm{option}s:

 @itemlist[
 @item{@DFlag{format} @nonterm{format} --- Specifies the archive format. 
      The allowed @nonterm{format}s are: @exec{zip} (the default), @exec{tgz}, and @exec{plt}. 
      This option must be specified if @DFlag{manifest} is not present.}
 @item{@DFlag{manifest} --- Creates a manifest file for a directory, rather than an archive.}
 ]
}
]

@subsection{Programmatic}

@defmodule[planet2]

The @racketmodname[planet2] module provides a programmatic interface
to the command sub-sub-commands.

@deftogether[
 (@defthing[install procedure?]             
  @defthing[update procedure?]             
  @defthing[remove procedure?]             
  @defthing[show procedure?]             
  @defthing[config procedure?]             
  @defthing[create procedure?])             
]{
 Duplicates the @seclink["cmdline"]{command line interface}. 

 Each long form option of the command-line interface is keyword
 argument. An argument corresponding to @DFlag{type}, @DFlag{deps},
 @DFlag{format}, or @DFlag{scope} accepts its argument as a symbol. All other options
 accept booleans, where @racket[#t] is equivalent to the presence of
 the option.}

@; ----------------------------------------

@section{Developing Packages}

To create a package, first make a directory for your package and
select its name, @nonterm{package}:

@commandline{mkdir @nonterm{package}}

Next, link your development directory to your local package
repository:

@commandline{raco pkg install --link @nonterm{package}}

Optionally, enter your directory and create a basic @filepath{info.rkt} file:

@commandline{cd @nonterm{package}}
@commandline{echo "#lang setup/infotab" > info.rkt}
@commandline{echo "(define deps (list))" >> info.rkt}

The @filepath{info.rkt} file is not necessary if you have no dependencies, but
you may wish to create it to simplify adding dependencies in the
future.

Next, inside the @nonterm{package} directory, create directories for
the collections and modules that your package will provide. For
example, the developer of @pkgname{tic-tac-toe} package that provides
@racketidfont{games/tic-tac-toe/main} and @racketidfont{data/matrix}
libraries might create directories and files like this:

@commandline{mkdir -p games/tic-tac-toe}
@commandline{touch games/tic-tac-toe/info.rkt}
@commandline{touch games/tic-tac-toe/main.rkt}
@commandline{mkdir -p data}
@commandline{touch data/matrix.rkt}

After your package is ready to deploy, choose either @secref["github-deploy"]
or @secref["manual-deploy"].

@subsection[#:tag "github-deploy"]{GitHub Deployment}

First, @link["https://github.com/signup/free"]{create a free account} on GitHub,
then @link["https://github.com/new"]{create a repository for your
package} (@link["https://help.github.com/articles/create-a-repo"]{documentation}).
Initialize the Git repository locally and do your first push like this:

@commandline{git init}
@commandline{git add *}
@commandline{git commit -m "First commit"}
@commandline{git remote add origin https://github.com/@nonterm{user}/@nonterm{package}.git}
@commandline{git push -u origin master}

Now, publish your package source as:

@inset{@exec{github://github.com/@nonterm{user}/@nonterm{package}/@nonterm{branch}}}

Typically, @nonterm{branch} will be @exec{master}, but you may wish to use
different branches for releases and development.

Whenever you

@commandline{git push}

your changes will automatically be discovered by those who used your
package source when they use @exec{raco pkg update}.

@subsection[#:tag "manual-deploy"]{Manual Deployment}

Alternatively, you can deploy your package by publishing it on a URL
you control. If you do this, it is preferable to create an archive
first:

@commandline{raco pkg create @nonterm{package}}

And then upload the archive and its @tech{checksum} to your site:

@commandline{scp @nonterm{package}.zip @nonterm{package}.zip.CHECKSUM your-host:public_html/}

Now, publish your package source as:

@inset{@exec{http://your-host/~@nonterm{user}/@nonterm{package}.zip}}

Whenever you want to provide a new release of a package, recreate and reupload the
package archive (and @tech{checksum}). Your changes will automatically be
discovered by those who used your package source when they use
@exec{raco pkg update}.

@subsection{Helping Others Discover Your Package}

By using either of the above deployment techniques, anyone will be
able to use your package by referring to your @tech{package source}.
However, they will not be able to refer to
it by a simple name until it is listed on a @tech{package name resolver}.

If you'd like to use the official @tech{package name resolver}, browse
to
@link["https://pkg.racket-lang.org/manage/upload"]{https://pkg.racket-lang.org/manage/upload}
and upload a new package. You will need to create an account and log
in first.

You only need to go to this site @emph{once} to list your package. The
server will periodically check the package source you designate for
updates.

If you use this server, and use GitHub for deployment, then you will
never need to open a web browser to update your package for end
users. You just need to push to your GitHub repository, then within 24
hours, the official @tech{package name resolver} will notice, and
@exec{raco pkg update} will work on your user's machines.

@subsection{Naming and Designing Packages}

Although of course not required, we suggest the following system for
naming and designing packages:

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
features. Such declarations allow @exec{raco pkg install} and
@exec{raco pkg update} to help check dependencies.  Declaring and
changing a version is optional, and @tech{package name resolvers}
ignore version declarations; in particular, a package is a candidate
for updating when its @tech{checksum} changes, independent of whether
the package's version changes or in which direction the version
changes.}

@item{Packages should not include large sets of utilities libraries
that are likely to cause conflicts. For example, packages should not
contain many extensions to the @filepath{racket} collection, like
@filepath{racket/more-lists.rkt} and
@filepath{racket/more-bools.rkt}. Instead, such as extensions should
be separated into their own packages.}

@item{Packages should generally provide one collection with a name
similar to the name of the package. For example, @pkgname{libgtk1}
should provide a collection named @filepath{libgtk}. Exceptions
include extensions to existing collection, such as new data-structures
for the @filepath{data} collection, DrRacket tools, new games for PLT
Games, etc.}

@item{Packages are not allowed to start with @pkgname{plt},
@pkgname{racket}, or @pkgname{planet} without special approval from
PLT curation.}

]

@; ----------------------------------------

@section[#:tag "metadata"]{Package Metadata}

Package metadata, including dependencies on other packages, is reported
by an @filepath{info.rkt} module within the package. This module must be
implemented in the @racketmodname[setup/infotab] language.

The following fields are used by the package manager:

@itemlist[

 @item{@racketidfont{version} --- a @tech{version} string. The default
       @tech{version} of a package is @racket["0.0"].}

 @item{@racketidfont{deps} --- a list of dependencies, where each
       dependency is either a @tech{package source} strings or a list
       containing a @tech{package source} string and a
       @tech{version} string.

       Each elements of the @racketidfont{deps} list determines a
       dependency on the @tech{package} whose name is inferred from
       the @tech{package source} (i.e., dependencies are on package
       names, not package sources), while the @tech{package source}
       indicates where to get the package if needed to satisfy the
       dependency.

       When provided, a @tech{version} string specifies a lower bound
       on an acceptable @tech{version} of the package.

       Use the package name @racket["racket"] to specify a dependency
       on the version of the Racket installation.}

 @item{@racketidfont{setup-collects} --- a list of path strings and/or
       lists of path strings, which are used as collection names to
       set up via @exec{raco setup} after the package is installed, or
       @racket['all] to indicate that all collections need to be
       setup. By default, only collections included in the package are
       set up (plus collections for global documentation indexes and
       links).}

]

For example, a basic @filepath{info.rkt} file might be

@codeblock{
#lang setup/infotab
(define version "1.0")
(define deps (list _package-source-string ...))
}

@; ----------------------------------------

@section{@|Planet1| Compatibility}

PLT maintains a @tech{package name resolver} to serve packages that
were developed using the original @seclink[#:doc '(lib
"planet/planet.scrbl") "top"]{@|Planet1|} package system.  This
compatibility resolver is at
@link["https://planet-compat.racket-lang.org/"]{https://planet-compat.racket-lang.org/},
which is included by default in the package-server search path.

Copies of @|Planet1| packages are automatically created by the
server according to the following system: for all packages that are in
the @litchar{4.x} @|Planet1| repository, the latest minor version of
@tt{@nonterm{user}/@nonterm{package}.plt/@nonterm{major-version}} will be available as
@pkgname{planet-@nonterm{user}-@nonterm{package}@nonterm{major-version}}. For example,
@tt{jaymccarthy/opencl.plt/1} minor version @tt{2}, will be available as
@pkgname{planet-jaymccarthy-opencl1}.

The contents of these copies is a single collection with the name
@filepath{@nonterm{user}/@nonterm{package}@nonterm{major-version}} with all the files from the
original @|Planet1| package in it.

Each file has been transliterated to use direct Racket-style requires
rather than @|Planet1|-style requires. For example, if any file contains
@racket[(planet jaymccarthy/opencl/module)], then it is transliterated
to @racket[jaymccarthy/opencl1/module]. @emph{This transliteration is
purely syntactic and is trivial to confuse, but works for most
packages, in practice.} Any transliterations that occurred are automatically added as
dependencies for the compatibility package.

We do not intend to improve this compatibility system much more over
time, because it is simply a stop-gap as developers port their @|Planet1|
packages to the new system. Additionally, the existence of the compatibility 
server is not meant
to imply that we will be removing @|Planet1| from existence in the near
future.

@; ----------------------------------------

@section[#:style 'quiet]{FAQ}

This section answers anticipated frequently asked questions about
the package manager.

@subsection{Are package installations versioned with respect to the
Racket version?}

By default, when you install a package, it is installed for a specific
user and a specific version of Racket. That is, the @tech{package
scope} is user- and version-specific.

You can change the default @tech{package scope} (for a particular
Racket installation) with @command-ref{config}@exec{ -i --set
default-scope installation}, in which case package operations apply
for all users of a Racket installation.  You can also use the @Flag{i}
or @DFlag{installation} flag with a specific @exec{raco pkg} command,
instead of changing the default scope for all uses of @exec{raco
pkg}. Note that an installation-wide package is not exactly
version-specific, because the version of an installation can change if
it corresponds to a source-code checkout that is periodically updated
and rebuilt.

If you change the default @tech{package scope}, you can use the
@Flag{u} or @DFlag{user} flag with a specific @exec{raco pkg} command
to perform the command with user- and version-specific @tech{package
scope}.

Finally, you can use the @Flag{s} or @DFlag{shared} flag
with @exec{raco pkg} commands to install user-specific packages that
apply to all Racket versions that you run. (In contrast, @|Planet1|
requires reinstallation of all packages every version change.)

@subsection{Where and how are packages installed?}

User-specific and Racket-version-specific packages are in @racket[(build-path
(find-system-path 'addon-dir) (version) "pkgs")], user-specific and
all-version packages are in @racket[(build-path (find-system-path
'addon-dir) "pkgs")], and installation-wide packages are in
@racket[(build-path (find-lib-dir) "pkgs")]. They are linked as
collection roots with @exec{raco link}.

@subsection{How are user-specific and installation-wide @tech{package scopes}
related for conflict checking?}

User-specific packages are checked against installation-wide packages
for conflicts. Installation-wide packages are checked only against
other installation-wide packages.

Beware that a new installation-wide package can invalidate previous
conflict checks for user-specific packages. Similarly, new
user-specific but all-version packages can invalidate previous
user-specific conflict checks for a different Racket version.

@subsection{Do I need to change a package's version when I update a package with error fixes, @|etc|?}

If you have new code for a package, then it should have a new
@tech{checksum}. When package updates are searched for, the checksum
of the installed package is compared with the checksum of the source,
if they are different, then the source is re-installed. This allows
code changes to be distributed. You do not need to declare an update a
version number, except to allow other package implementors to indicate
a dependency on particular features (where a bug fix might be
considered a feature, but it is not usually necessary to consider it
that way).

@subsection{How can I specify which version of a package I depend on
if its interface has changed and I need an @emph{old} version?}

In such a situation, the author of the package has released a
backwards incompatible edition of a package. The package manager provides
no help to deal with this situation (other than, of course, not
installing the ``update''). Therefore, package authors should not make
backwards incompatible changes to packages. Instead, they should
release a new package with a new name. For example, package
@pkgname{libgtk} might become @pkgname{libgtk2}. These packages
should be designed to not conflict with each other, as well.

@subsection{Why is the package manager so different than @|Planet1|?}

There are two fundamental differences between @|Planet1| and this package manager.

The first is that @|Planet1| uses ``internal linking'' whereas the current package manager
uses ``external linking.'' For example, an individual module requires a
@|Planet1| package directly in a require statement:

@racketblock[
 (require (planet game/tic-tac-toe/data/matrix))
]

whereas using the package manager, the module would simply require the module of
interest:

@racketblock[
 (require data/matrix)             
]

and would rely on the external system having the
@pkgname{tic-tac-toe} package installed.

This change is good because it makes the origin of modules more
flexible---so that code can migrate in and out of the core, packages
can easily be split up, combined, or taken over by other authors, etc.

This change is bad because it makes the meaning of your program
dependent on the state of the system.

The second major difference is that @|Planet1| is committed to
guaranteeing that packages that never conflict with one another, so
that any number of major and minor versions of the same package can be
installed and used simultaneously. The package manager does not share this
commitment, so package authors and users must be mindful of potential
conflicts and plan around them.

This change is good because it is simpler and lowers the burden of
maintenance (provided most packages don't conflict.)

The change is bad because users must plan around potential conflicts.

In general, the goal of the package manager is to be a lower-level
system, more like the package systems used by operating systems. The
goals of @|Planet1| are not bad, but we believe they are needed
infrequently and a system like @|Planet1| could be more easily built
atop the package manager than the reverse.

In particular, our plans to mitigate the downsides of these changes
are documented in @secref["short-term"].

@; ----------------------------------------

@section{Future Plans}

@subsection[#:tag "short-term"]{Short Term}

This section lists some short term plans for the package manager. These are
important, but didn't block its release. The package manager will be considered
out of beta when these are completed.

@itemlist[

@item{The official PNR will divide packages into three
categories: @reponame{planet}, @reponame{solar-system}, and @reponame{galaxy}. The definitions
for these categories are:

 @itemlist[

  @item{@reponame{galaxy} --- No restrictions.}

  @item{@reponame{solar-system} --- Must not conflict any package
in @reponame{solar-system} or @reponame{planet}.}

  @item{@reponame{planet} --- Must not conflict any package in @reponame{solar-system}
or @reponame{planet}. Must have documentation and tests. The author must be
responsive about fixing regressions against changes in Racket, etc.}

 ]

These categories will be curated by PLT.

Our goal is for all packages to be in the @reponame{solar-system}, with
the @reponame{galaxy} as a temporary place while the curators work with the
authors of conflicting packages to determine how modules should be
renamed for unity.

However, before curation is complete, each package will be
automatically placed in @reponame{galaxy} or @reponame{solar-system}
depending on its conflicts, with preference being given to older
packages. (For example, if a new package B conflicts with an old
package A, then A will be in @reponame{solar-system}, but B will be in
@reponame{galaxy}.) During curation, however, it is not necessarily
the case that older packages have preference. (For example,
@pkgname{tic-tac-toe} should probably not provide
@filepath{data/matrix.rkt}, but that could be spun off into another
package used by both @pkgname{tic-tac-toe} and
@pkgname{factory-optimize}.)

In contrast, the @reponame{planet} category will be a special category that
authors may apply for. Admission requires a code audit and implies
a "stamp of approval" from PLT. In the future, packages in this
category will have more benefits, such as automatic regression testing
on DrDr, testing during releases, provided binaries, and advertisement
during installation.

The @|Planet1| compatibility packages will also be included in
the @reponame{solar-system} category, automatically. 

}

@item{In order to mitigate the costs of external linking vis a vis the
inability to understand code in isolation, we will create a module
resolver that searches for providers of modules on the configured
@tech{package name resolvers}. For example, if a module requires
@filepath{data/matrix.rkt}, and it is not available, then the PNR will
be consulted to discover what packages provide it. @emph{Only packages
in @reponame{solar-system} or @reponame{planet} will be
returned.} (This category restriction ensures that the package to
install is unique.)

Users can configure their systems to then automatically install the
package provided is has the appropriate category (i.e., some users may
wish to automatically install @reponame{planet} packages but not
@reponame{solar-system} packages, while others may not want to install
any.)

This feature will be generalized across all @tech{package name
resolvers}, so users could maintain their own category definitions with
different policies.}

]

@subsection{Long Term}

This section lists some long term plans for the package manager. Many of these
require a lot of cross-Racket integration.

@itemlist[

@item{The official PNR is bare bones. It could conceivably do a lot
more: keep track of more statistics, enable "social" interactions
about packages, link to documentation, problem reports, licenses,
etc. Some of this is easy and obvious, but the community's needs are
unclear.}

@item{It would be nice to encrypt information from the official
@tech{package name resolver} with a public key shipped with Racket, and
allow other resolvers to implement a similar security scheme.}

@item{Packages in the @reponame{planet} category should be tested on
DrDr. This would require a way to communicate information about how
they should be run to DrDr. This is currently done via the
@filepath{meta/props} script for things in the core. We should
generalize this script to a @filepath{meta/props.d} directory so that
packages can install DrDr metadata to it.}

@item{We hope that this package system will encourage more incremental
improvements to pieces of Racket. In particular, it would be wonderful
to have a very thorough @filepath{data} collection of different
data-structures. However, our existing setup for Scribble would force
each new data structue to have a different top-level documentation
manual, rather than extending the documentation of the existing
@filepath{data} collection. Similar issues will exist for the
@filepath{net} and @filepath{file} collections. We should design a way
to have such "documentation plugins" in Scribble and support
similar "plugin" systems elsewhere in the code-base.}

@item{Packages can contain any kinds of files, including bytecode and
documentation, which would reduce the time required to install a
package (since we must run @exec{raco setup}). However, packages with
these included are painful to maintain and unreliable given users with
different versions of Racket installed.

One solution is to have a separate place where such "binary" packages
are available. For example, PLT could run a PNR for every Racket
version, i.e., @filepath{https://binaries.racket-lang.org/5.3.1.4},
that would contain the binaries for all the packages in the
@reponame{planet} category. Thus, when you install package
@pkgname{tic-tac-toe} you could also install the binary version from
the appropriate PNR.

There are obvious problems with this... it could be expensive for PLT
in terms of space and time... Racket compilation is not necessarily
deterministic or platform-independent.

This problem requires more thought.}

@item{The user interface could be improved, including integration with
DrRacket and a GUI. For example, it would be good if DrRacket would
poll for package updates periodically and if when it was first started
it would display available, popular packages.}

@item{The core distribution should be split apart into many more
packages. For example, Redex, Plot, the Web Server, and the teaching
languages are natural candidates for being broken off.}

@item{The core should be able to be distributed with packages that
will be installed as soon as the system is installed. Ideally, this
would be customizable by instructors so they could share small
distributions with just the right packages for their class.}

]
