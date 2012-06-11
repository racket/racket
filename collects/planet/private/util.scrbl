#lang scribble/manual

@(require
  (for-label
   racket/base
   scribble/manual
   planet/resolver
   planet/config
   planet/util
   planet/version
   planet/syntax
   planet/scribble))

@title{Utility Libraries}

The planet collection provides configuration and utilities for using @|PLaneT|. 

@section{Resolver}

@defmodule[planet/resolver]

The primary purpose of this library to for @racket[require] to find
@PLaneT packages. It also, however, provides some utilities for manipulating
the resolvers behavior.

@defproc*[(((planet-module-name-resolver [r-m-p resolved-module-path?])
            void?)
           ((planet-module-name-resolver [spec (or/c module-path? path?)]
                                         [module-path (or/c #f resolved-module-path?)]
                                         [stx (or/c #f syntax?)]
                                         [load boolean?]
                                         [orig-paramz parameterization?])
           resolved-module-path?))]{
  This implements the @|PLaneT| module resolution process. It is @racket[dynamic-require]d
  by racket when the first @|PLaneT| module require is needed. It acts much like a 
  @racket[current-module-name-resolver] would, but racket provides it with a special
  @racket[parameterization?] (giving it special privileges) that it uses when installing new packages.
}

@defproc[(get-planet-module-path/pkg [spec (or/c module-path? path?)]
                                     [module-path (or/c #f resolved-module-path?)]
                                     [stx (or/c #f syntax?)])
         (values path? pkg?)]{
  Returns the path corresponding to the package (interpreting the arguments
  the same way as @racket[planet-module-name-resolver] and @racket[(current-module-name-resolver)]).
}

@defproc[(resolve-planet-path [planet-path any/c]) path?]{
  Returns the path where the file named by the require spec @racket[planet-path] is located in the current installation.
}

@defparam[download? dl? boolean?]{
  A parameter that controls if @PLaneT attempts to download a package that isn't already present.
  If the package isn't present, the resolver will raise the @racket[exn:fail:planet?] exception
  instead of downloading it.
}

@defparam[install? inst? boolean?]{
  A parameter that controls if @PLaneT attempts to install a package that isn't already installed.
  If the package isn't installed, the resolver will raise the @racket[exn:fail:planet?] exception
  instead of installing it.
}

@subsection{Resolver file locking}

When @|PLaneT| is asked to resolve a module path for loading the file
(e.g., when the last argument to the @racket[(current-module-name-resolver)]
is @racket[#t] and that resolver triggers a call to the @|PLaneT| resolver),
it finds the directory
where the files are installed, say in this directory, which
corresponds to version 1.2 of dyoo's closure-compile.plt package:

@centered{@filepath{@racket[(CACHE-DIR)]/dyoo/closure-compile.plt/1/2/}}

If the file

@centered{@filepath{@racket[(CACHE-DIR)]/dyoo/closure-compile.plt/1/2.SUCCESS}}

is there, it assumes that there is no installation needed and it just
continues, using the path to the file inside that directory.

If the @filepath{2.SUCCESS} file is not there, then it attempts to grab an
@racket['exclusive] filesystem lock on this file (via @racket[port-try-file-lock?])

@centered{@filepath{@racket[(CACHE-DIR)]/dyoo/closure-compile.plt/1/2.LOCK}}

If it gets the lock, it then proceeds with the installation, calling
raco setup to do the unpacking, compilation, and docs building.
After the unpacking has finished, but before beginning compilation and docs
building, it creates the @filepath{2.UNPACKED} file:

@centered{@filepath{@racket[(CACHE-DIR)]/dyoo/closure-compile.plt/1/2.UNPACKED}}

When compilation and docs build are complete, it creates the @filepath{2.SUCCESS} file:

@centered{@filepath{@racket[(CACHE-DIR)]/dyoo/closure-compile.plt/1/2.SUCCESS}}

and releases the lock on the @filepath{2.LOCK} file.

If it fails to get the lock on @filepath{2.LOCK} and it does not already hold the 
lock (due to a re-entrant call to the resolver (the resolver knows about locks it 
holds via an internal parameter that gets created when the @racketmodname[planet/resolver]
module is instantiated) then it goes into a loop that polls for
the existence of the @filepath{2.SUCCESS} file; when it that file appears, it
just continues, without installing anything (since that means someone
else installed it).

In some situations (e.g., when a new namespace is created and a fresh instantiation of
@racketmodname[planet/resolver] is created), @|PLaneT| can be fooled into thinking that it
does not hold the lock on some installation. In order to cope with these situations somewhat,
@|PLaneT| takes an easier path when the resolver is only looking for information about
package files (i.e., when the last argument to the resolver is @racket[#f], or when
@racket[get-planet-module-path/pkg] is called directly (as opposed to being called
via module resolution). In those cases, @|PLaneT| will look only for the 
@filepath{2.UNPACKED} file instead of the @filepath{2.SUCCESS} file.

@section{Client Configuration}

@defmodule[planet/config]

The @racketmodname[planet/config] library provides several parameters
useful for configuring how PLaneT works.

Note that while these parameters can be useful to modify
programmatically, PLaneT code runs at module-expansion time, so
most user programs cannot set them until PLaneT has already
run. Therefore, to meaningfully change these settings, it is best to
manually edit the @filepath{config.rkt} file.

@defparam[PLANET-BASE-DIR dir path-string?]{
  The root of the tree where planet stores all of its files. Defaults to
  @racketblock[(let ([plt-planet-dir-env-var (getenv "PLTPLANETDIR")])
                 (if plt-planet-dir-env-var
                     (string->path plt-planet-dir-env-var)
                     (build-path (find-system-path 'addon-dir)
                                 "planet"
                                 (PLANET-CODE-VERSION))))]
}

@defparam[PLANET-DIR dir path-string?]{
The root of the version-specific PLaneT files. 
Defaults to @racket[(build-path (PLANET-BASE-DIR) (version))].
}

@defparam[CACHE-DIR dir path-string?]{
The root of the PLaneT client's cache directory.}

@defparam[UNINSTALLED-PACKAGE-CACHE dir path-string?]{
The root of the PLaneT client's uninstalled-packages cache. PLaneT
stores package distribution files in this directory, and searches for
them in this directory for them if necessary. Unlike the main PLaneT
cache, which contains compiled files and is specific to each
particular version of Racket, the uninstalled package cache is
shared by all versions of Racket that use the same package
repository, and it is searched if a package is not installed in the
primary cache and cannot be downloaded from the central PLaneT repository
(for instance due to a loss of Internet connectivity). This behavior
is intended to primarily benefit users who upgrade their Racket
installations frequently.}

@defparam[LINKAGE-FILE file path-string?]{
The file to use as the first place PLaneT looks to determine how a
particular PLaneT dependence in a file should be satisfied. The
contents of this file are used to ensure that no "magic upgrades"
occur after a package is installed. The default is the file @filepath{LINKAGE}
in the root PLaneT directory.}

@defparam[LOG-FILE file (or/c path-string? false?)]{
If @racket[#f], indicates that no logging should take place. Otherwise
specifies the file into which logging should be written. The default
is the file @filepath{INSTALL-LOG} in the root PLaneT directory.}

@defboolparam[USE-HTTP-DOWNLOADS? bool]{
PLaneT can use two different protocols to retrieve packages. If @racket[#t],
PLaneT will use the HTTP protocol; if @racket[#f] it will use the custom-built
PLaneT protocol. The default value for this parameter is @racket[#t] and setting
this parameter to @racket[#f] is not recommended.}

@defparam[HTTP-DOWNLOAD-SERVLET-URL url string?]{
The URL  for the servlet that will provide PLaneT packages if 
@racket[USE-HTTP-DOWNLOADS?] is @racket[#t], represented as a string.  
This defaults to the value of the @indexed-envvar{PLTPLANETURL} environment
variable if it is set and otherwise is
@racket["http://planet.racket-lang.org/servlets/planet-servlet.rkt"].}

@defparam[PLANET-SERVER-NAME host string?]{
The name of the PLaneT server to which the client should connect if
@racket[USE-HTTP-DOWNLOADS?] is @racket[#f]. The default value for this parameter is
@racket["planet.racket-lang.org"].}

@defparam[PLANET-SERVER-PORT port natural-number?]{
The port on the server the client should connect to if
@racket[USE-HTTP-DOWNLOADS?] is @racket[#f]. The default value for this parameter is
@racket[270].}

@defparam[HARD-LINK-FILE file path?]{
  The name of the file where hard links are saved. Defaults to
  @racket[(build-path (PLANET-BASE-DIR) (version) "HARD-LINKS")].
}
@defparam[PLANET-ARCHIVE-FILTER regexp-filter (or/c #f string? regexp?)]{
  A regular-expression based filter that is used to skip files when building a @|PLaneT| archive.
}
@defparam[PLANET-CODE-VERSION vers string?]{
  Used to compute @racket[PLANET-BASE-VERSION].
}

@defparam[DEFAULT-PACKAGE-LANGUAGE vers string?]{
  The package language used when communicating with the server to find
  which package to download.

  Defaults to @racket[(version)].
}

@section{Package Archives}

@defmodule[planet/planet-archives]

@defproc[(get-all-planet-packages)
         (listof (list/c (and/c path? absolute-path?) string? string? (listof string?) 
                         exact-nonnegative-integer?
                         exact-nonnegative-integer?))]{
  Returns the installed planet package. Each element of the result list corresponds to
  a single package. The first element in an inner list is the location of the installed files.
  The second and third elements are the owner and package names. The last two elements
  are the major and minor verisons
}

@defproc[(get-installed-planet-archives)
         (listof (list/c (and/c path? absolute-path?) string? string? (listof string?) 
                         exact-nonnegative-integer?
                         exact-nonnegative-integer?))]{
 Like @racket[get-all-planet-archives], except that it does not return packages linked in
      with ``raco planet link''.

}

@defproc[(get-hard-linked-packages)
         (listof (list/c (and/c path? absolute-path?) string? string? (listof string?) 
                         exact-nonnegative-integer?
                         exact-nonnegative-integer?))]{
 Like @racket[get-all-planet-archives], except that it return only packages linked in
      with ``raco planet link''.

}

@section[#:tag "util.rkt"]{Package Utils}

@defmodule[planet/util]

The @racketmodname[planet/util] library supports examination of the pieces of
PLaneT. It is meant primarily to support debugging and to allow easier
development of higher-level package-management tools. The
functionality exposed by @seclink["cmdline"]{the @exec{raco planet} command-line tool} is
also available programmatically through this library.

@defproc[(download/install-pkg [owner string?]
			       [pkg (and/c string? #rx"[.]plt$")]
			       [maj natural-number/c]
			       [min natural-number/c])
         (or/c pkg? #f)]{
Downloads and installs the package specifed by the given owner name,
package name, major and minor version number. Returns false if no such
package is available; otherwise returns a package structure for the
installed package.

The @racket[pkg] argument must end with @racket[".plt"].
}

@defproc[(install-pkg [pkg-spec pkg-spec?]
                      [file path-string?]
                      [maj natural-number/c]
                      [min natural-number/c])
         (or/c pkg-spec? #f)]{
 Installs the package represented by the arguments, using
 the @racket[pkg-spec] argument to find the path and name of
 the package to install.
 
 See @racket[get-package-spec] to build a @racket[pkg-spec] argument.
 
 Returns a new @racket[pkg-spec?] corresponding to the package
 that was actually installed.
}


@defproc[(get-package-spec [owner string?]
                           [pkg (and/c string? #rx"[.]plt$")]
                           [maj (or/c #f natural-number/c) #f]
                           [min (or/c #f natural-number/c) #f])
         pkg-spec?]{
  Builds a @racket[pkg-spec?] corresponding to the package specified by 
  @racket[owner], @racket[pkg], @racket[maj], and @racket[min].
  
  The @racket[pkg] argument must end with the string @racket[".plt"].
}

@defproc[(pkg-spec? [v any/c]) boolean?]{
  Recognizes the result of @racket[get-package-spec] (and @racket[install-pkg]).
}
                   
@defparam[current-cache-contents contents
          (listof
           (list/c string? 
                   (listof 
                    (list/c string? 
                            (cons/c natural-number/c 
                                    (listof natural-number/c))))))]{
Holds a listing of all package names and versions installed in the
local cache.}

@defproc[(current-linkage)
         (listof (list/c path-string? 
                         (list/c string?
                                 (list/c string?) 
                                 natural-number/c
                                 natural-number/c)))]{
Returns the current linkage table.

The linkage table is an association between file locations (encoded as path strings)
and concrete planet package versions. If a require line in the associated file requests a package,
this table is consulted to determine a particular concrete package to satisfy the request.}

@defproc[(make-planet-archive [directory path-string?]
                              [output-file (or/c path? path-string?) 
                                           (string-append (path->string name) ".plt")]) 
         path-string?]{
Makes a .plt archive file suitable for PLaneT whose contents are all
files in the given directory and returns that file's name.  If the
optional filename argument is provided, that filename will be used as
the output file's name.

See also @racket[build-scribble-docs?] and @racket[force-package-building?]
}

@defparam[build-scribble-docs? b boolean?]{
  Determines if @racket[make-planet-archive] builds scribble docs (or not).
}

@defparam[force-package-building? b boolean?]{
  Determines if @racket[make-planet-archive] signals an error and refuses
  to continue packaging for certain, more significant errors.
  
  Defaults to @racket[#t], and thus packaging will signal errors.
}

@defproc[(download-package [pkg-spec pkg-spec?]) 
         (or/c (list/c #true path? natural-number/c natural-number/c)
               string?
               (list/c #false string?))]{
  Downloads the package given by @racket[pkg-spec]. If the result is
  a list whose first element is @racket[#true], then the package was
  downloaded successfully and the rest of the elements of the list
  indicate where it was downloaded, and the precise version number.
  
  The other two possible results indicate errors. If the result is
  a list, then the server is saying that there is no matching package;
  otherwise the error is some lower-level problem (perhaps no networking, etc.)
}

@defproc[(pkg->download-url [pkg pkg?]) url?]{
  Returns the url for a given package.
}

@defproc[(get-package-from-cache [pkg-spec pkg-spec?]) (or/c #false path?)]{
  Returns the location of the already downloaded package, 
  if it exists (and @racket[#false] otherwise).
}

@defproc[(lookup-package-by-keys [owner string?]
                                 [name string?]
                                 [major exact-nonnegative-integer?]
                                 [minor-lo exact-nonnegative-integer?]
                                 [minor-hi exact-nonnegative-integer?])
         (or/c (list/c path?
                       string?
                       string?
                       (listof string?)
                       exact-nonnegative-integer?
                       exact-nonnegative-integer?)
               #false)]{
   Looks up and returns a list representation of the package named by the given owner,
   package name, major and (range of) minor version(s).
}

@defproc[(unpack-planet-archive [plt-file (or/c path? path-string?)]
				[output-dir (or/c path? path-string?)])
 	 any]{
Unpacks the PLaneT archive with the given filename, placing its contents
into the given directory (creating that path if necessary).}

@defproc[(remove-pkg [owner string?]
		     [pkg   (and/c string? #rx"[.]plt$")]
		     [maj   natural-number/c]
		     [min   natural-number/c])
	 any]{
Removes the specified package from the local planet cache, deleting the installed files.
}

@defproc[(erase-pkg [owner string?]
                    [pkg   (and/c string? #rx"[.]plt$")]
                    [maj   natural-number/c]
                    [min   natural-number/c])
	 any]{
Like @racket[remove-pkg], removes the specified package from the local planet cache and deletes
all of the files corresponding to the package, but also deletes the cached @filepath{.plt} file
(so it will be redownloaded later).
}
             
@defproc[(display-plt-file-structure [plt-file (or/c path-string? path?)])
         any]{
Print a tree representing the file and directory structure of the
PLaneT archive .plt file named by @racket[plt-file] to @racket[(current-output-port)].}

@defproc[(display-plt-archived-file [plt-file (or/c path-string? path?)]
				    [file-to-print string?])
	 any]{
Print the contents of the file named @racket[file-to-print] within the 
PLaneT archive .plt file named by @racket[plt-file] to @racket[(current-output-port)].}

@defproc[(unlink-all) any]{
Removes the entire linkage table from the system, which will force all
modules to relink themselves to PLaneT modules the next time they run.}

@defproc[(add-hard-link [owner string?]
			[pkg   (and/c string? #rx"[.]plt$")]
			[maj   natural-number/c]
			[min   natural-number/c]
			[dir   path?])
	 any]{
Adds a development link between the specified package and the given
directory; once a link is established, PLaneT will treat the cache as
having a package with the given owner, name, and version whose files
are located in the given path. This is intended for package
development; users only interested in using PLaneT packages
available online should not need to create any development links.

If the specified package already has a development link, this function
first removes the old link and then adds the new one.

The @racket[pkg] argument must end with the string @racket[".plt"].
}

@defproc[(remove-hard-link [owner string?]
	 		   [pkg   (and/c string? #rx"[.]plt$")]
			   [maj   natural-number/c]
			   [min   natural-number/c]
                           [#:quiet? quiet? boolean? #false])
 	 any]{
Removes any hard link that may be associated with the given package.

The @racket[pkg] argument must end with the string @racket[".plt"].
The @racket[maj] and @racket[min] arguments must be integers. This
procedure signals an error if no such link exists, unless
@racket[#:quiet?] is @racket[#true].
}

@defproc[(resolve-planet-path [spec quoted-planet-require-spec?])
	 path?]{
Returns the file system path to the file specified by the given quoted 
planet require specification. This function downloads and installs the
specified package if necessary, but does not verify that the actual
file within it actually exists.}

@defproc[(path->package-version [p path?])
         (or/c (list/c string? string? natural-number/c natural-number/c) #f)]{

Given a path that corresponds to a PLaneT package (or some part of one),
produces a list corresponding to its name and version, exactly like
@racket[(this-package-version)].  Given any other path, produces @racket[#f].

}

@defstruct[(exn:fail:planet exn:fail) ([message string?] [continuation-marks continuation-mark-set?])]{
   This exception record is used to report planet-specific exceptions.
}

@defproc[(pkg? [v any/c]) boolean?]{
  Determines if its argument is a pkg, the representation of an
  installed package.
}

@section[#:tag "version.rkt"]{Package Version}

Provides bindings for @|PLaneT| developers that automatically
produce references to the name and version of the containing @|PLaneT| package
so the same code may be reused across releases without accidentally referring to
a different version of the same package.

@defmodule[planet/version #:use-sources (planet/private/version)]

@deftogether[(
@defform[(this-package-version)]
@defform*[[(this-package-version-symbol)
           (this-package-version-symbol suffix-id)]]
@defform[(this-package-version-name)]
@defform[(this-package-version-owner)]
@defform[(this-package-version-maj)]
@defform[(this-package-version-min)]
)]{

Macros that expand into expressions that evaluate to information about the name,
owner, and version number of the package in which they
appear. @racket[this-package-version] returns a list consisting of a string
naming the package's owner, a string naming the package, a number indicating the
package major version and a number indicating the package minor version, or
@racket[#f] if the expression appears outside the context of a package.
The macros @racket[this-package-version-name],
@racket[this-package-version-owner], @racket[this-package-version-maj], and
@racket[this-package-version-min] produce the relevant fields of the package
version list.

@racket[this-package-version-symbol] produces a symbol
suitable for use in @racket[planet] module paths.  For instance, in version
@racketmodfont{1:0} of the package @racketmodfont{package.plt} owned by
@racketmodfont{author}, @racket[(this-package-version-symbol dir/file)] produces
@racket['author/package:1:0/dir/file].  In the same package,
@racket[(this-package-version-symbol)] produces @racket['author/package:1:0].

}

@defform[(this-package-in suffix-id ...)]{

A @racket[require] sub-form that requires modules from within the same @|PLaneT|
package version as the require, as referred to by each @racket[suffix-id]. For
instance, in version @racketmodfont{1:0} of the package
@racketmodfont{package.plt} owned by @racketmodfont{author},
@racket[(require (this-package-in dir/file))] is equivalent to
@racket[(require (planet author/package:1:0/dir/file))].

@italic{Note:} Use @racket[this-package-in] when documenting @|PLaneT| packages
with Scribble to associate each documented binding with the appropriate package.

}

@defproc[(make-planet-symbol [stx syntax?]
                             [suffix (or/c #false string?) #false])
         (or/c #false symbol?)]{
  Returns a symbol representing a require spec for the location of @racket[stx],
  as a planet package.
}

@defproc[(package-version->symbol [ver (or/c (list/c string? string? exact-nonnegative-integer? exact-nonnegative-integer?)
                                             #false)]
                                  [suffix (or/c #false string?) #false])
         (or/c #false symbol?)]{
  Returns a symbol representing the require spec for @racket[ver],
  as a planet package.
}

@section[#:tag "syntax.rkt"]{Macros and Syntax Objects}

@defmodule[planet/syntax]

Provides bindings useful for @|PLaneT|-based macros.

@deftogether[(
@defproc[(syntax-source-planet-package [stx syntax?]) (or/c list? #f)]
@defproc[(syntax-source-planet-package-owner [stx syntax?]) (or/c string? #f)]
@defproc[(syntax-source-planet-package-name [stx syntax?]) (or/c string? #f)]
@defproc[(syntax-source-planet-package-major [stx syntax?]) (or/c integer? #f)]
@defproc[(syntax-source-planet-package-minor [stx syntax?]) (or/c integer? #f)]
@defproc[(syntax-source-planet-package-symbol
           [stx syntax?]
           [suffix (or/c symbol? #f) #f])
         (or/c symbol? #f)]
)]{

Produce output analogous to @racket[this-package-version],
@racket[this-package-version-owner], @racket[this-package-version-name],
@racket[this-package-version-maj], @racket[this-package-version-min], and
@racket[this-package-version-symbol] based on the source location of
@racket[stx].

}

@defproc[(make-planet-require-spec
           [stx syntax?]
           [suffix (or/c symbol? #f) #f])
         syntax?]{

Produces a @racket[require] sub-form for the module referred to by
@racket[suffix] in the @|PLaneT| package containing the source location of
@racket[stx].

}

@section[#:tag "scribble.rkt"]{Scribble Documentation}

@defmodule[planet/scribble]

Provides bindings for documenting @|PLaneT| packages.

@defform[(this-package-in suffix-id ...)]{

This binding from @racketmodname[planet/version] is also exported from
@racketmodname[planet/scribble], as it is useful for @racket[for-label] imports
in Scribble documentation.

}

@deftogether[(
@defform[(racketmod/this-package maybe-file suffix-id datum ...)]
@defform*[((racketmodname/this-package suffix-id)
           (racketmodname/this-package (#,(racket unsyntax) suffix-expr)))]
@defform[(racketmodlink/this-package suffix-id pre-content-expr ...)]
@defform[(defmodule/this-package maybe-req suffix-id maybe-sources pre-flow ...)]
@defform*[((defmodulelang/this-package suffix-id maybe-sources pre-flow ...)
           (defmodulelang/this-package suffix-id
             #:module-paths (mod-suffix-id ...) maybe-sources 
             pre-flow ...))]
@defform[(defmodulereader/this-package suffix-id maybe-sources pre-flow ...)]
@defform[(defmodule*/this-package maybe-req (suffix-id ...+) 
           maybe-sources pre-flow ...)]
@defform*[((defmodulelang*/this-package (suffix-id ...+) 
             maybe-sources pre-flow ...)
           (defmodulelang*/this-package (suffix-id ...+) 
             #:module-paths (mod-suffix-id ...) maybe-sources 
             pre-flow ...))]
@defform[(defmodulereader*/this-package (suffix-id ...+) 
           maybe-sources pre-flow ...)]
@defform[(defmodule*/no-declare/this-package maybe-req (suffix-id ...+)
           maybe-sources pre-flow ...)]
@defform*[((defmodulelang*/no-declare/this-package (suffix-id ...+)
             maybe-sources pre-flow ...)
           (defmodulelang*/no-declare/this-package (suffix-id ...+) 
             #:module-paths (mod-suffix-id ...) maybe-sources pre-flow ...))]
@defform[(defmodulereader*/no-declare/this-package (suffix-id ...+)
           maybe-sources pre-flow ...)]
@defform[(declare-exporting/this-package suffix-id ... maybe-sources)]
)]{

Variants of @racket[racketmod], @racket[racketmodname],
@racket[racketmodlink], @racket[defmodule], @racket[defmodulereader],
@racket[defmodulelang], @racket[defmodule*], @racket[defmodulelang*],
@racket[defmodulereader*], @racket[defmodule*/no-declare],
@racket[defmodulelang*/no-declare],
@racket[defmodulereader*/no-declare], and @racket[declare-exporting],
respectively, that implicitly refer to the PLaneT package that
contains the enclosing module.

The full module name passed to @racket[defmodule], etc is formed by
appending the @racket[suffix-id] or @racket[mod-suffix-id] to the
symbol returned by @racket[(this-package-version-symbol)], separated
by a @litchar{/} character, and tagging the resulting symbol as a
@racket[planet] module path. As a special case, if @racket[suffix-id]
is @racketid[main], the suffix is omitted.

For example, within a package named @tt{package.plt} by @tt{author},
version @tt{1:0}, the following are equivalent:
@racketblock[
(defmodule/this-package dir/file)
  @#,elem{=} (defmodule (planet author/package:1:0/dir/file))
]
and
@racketblock[
(defmodule/this-package main)
  @#,elem{=} (defmodule (planet author/package:1:0))
]
}

@section{Terse Status Updates}

@defmodule[planet/terse-info]

This module provides access to some PLaneT status information. This
module is first loaded by PLaneT in the initial namespace (when
PLaneT's resolver is loaded), but PLaneT uses @racket[dynamic-require] to load
this module each time it wants to announce information. Similarly, the
state of which procedures are registered (via @racket[planet-terse-register]) 
is saved in the namespace, making the listening and information producing
namespace-specific.

@defproc[(planet-terse-register
          [proc (-> (or/c 'download 'install 'docs-build 'finish)
                    string?
                    any/c)])
         void?]{
Registers @racket[proc] as a function to be called when
@racket[planet-terse-log] is called.

Note that @racket[proc] is called 
asynchronously (ie, on some thread other than the one calling @racket[planet-terse-register]).
}

@defproc[(planet-terse-log [id (or/c 'download 'install 'finish)]
                           [msg string?]) void?]{
This function is called by PLaneT to announce when things are happening. See also
@racket[planet-terse-set-key].
}

@defproc[(planet-terse-set-key [key any/c]) void?]{
  This sets a @seclink["threadcells" #:doc '(lib "scribblings/reference/reference.scrbl")]{thread cell}
  to the value of @racket[key].
  The value of the thread cell is used as an index into a table to determine which
  of the functions passed to @racket[planet-terse-register] to call when 
  @racket[planet-terse-log] is called.
  
  The table holding the key uses ephemerons and a weak hash table to ensure that
  when the @racket[key] is unreachable, then the procedures passed to @racket[planet-terse-log]
  cannot be reached through the table.
}

@section{The Cache File's Path}

@defmodule[planet/cachepath]

@defproc[(get-planet-cache-path) (and/c path? absolute-path?)]{
 Returns the path to the @filepath{cache.rktd} file for the planet installation.
}
