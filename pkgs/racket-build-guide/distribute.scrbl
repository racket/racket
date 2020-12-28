#lang scribble/manual
@(require "common.rkt")

@(define distro-build-doc '(lib "distro-build/distro-build.scrbl"))

@(define distro-build-package
   @seclink["top"
            #:doc '(lib "distro-build/distro-build.scrbl")]{the @filepath{distro-build} package})

@title[#:tag "distribute"]{Distributing Racket Variants}

This chapter is about distributing variants of Racket, as opposed to
distributing applications that are built with Racket. See
@secref["exe-dist" #:doc '(lib "scribblings/raco/raco.scrbl")]
for information about distributing applications.

@bold{Important:} To build installers that can be distributed to other
users, do not use @exec{make in-place} or @exec{make unix-style}, but
instead start from a clean repository.

Use one non-Windows machine as a server, where packages will be
pre-built. Then, as described below, create platform-specific
installers on some number of client machines, each of which contacts
the server machine to obtain pre-built packages. The server can act as
a client, naturally, to create an installer for the server's platform.

GNU @exec{make} is required on the server and non-windows client
machines, @exec{nmake} is required on Windows client machines.

The distribution-build process is a collaboration between the Racket
Git repository's top-level makefile and @|distro-build-package|.

@; ------------------------------------------------------------
@section{Running Build Farms}

The @exec{installers} target of the makefile will do everything to
generate installers: build a server on the current machine, run
clients on hosts specified via @exec{CONFIG}, and start/stop
VirtualBox virtual machines or Docker containers that act as client
machines.

If the server is already built, the @exec{installers-from-built}
target will drive the client builds without re-building the server.

See the documentation of @|distro-build-package| for a description of
the site-configuration module and requirements on client hosts.

If @filepath{my-site-config.rkt} is a configuration module, then

@commandline{make installers CONFIG=my-site-config.rkt}

drives the build farm, and the resulting installers are in
@filepath{build/installers}, with a hash table mapping descriptions to
installer filenames in @filepath{build/installer/table.rktd}. A log
file for each client is written to @filepath{build/log}.

If you have the @filepath{distro-build-server} package installed in
some Racket build (not the one for building installers), you can use

@commandline{make describe-clients CONFIG=my-site-config.rkt}

to see, without building anything, the effect of the configuration in
@filepath{my-site-config.rkt} and the planned build steps. See also the
@racket[#:fake-installers?] site-configuration option.

The default @exec{CONFIG} path is @filepath{build/site.rkt}, so you
could put your configuration file there and omit the @exec{CONFIG}
argument to @exec{make}. A default configuration file is created there
automatically. Supply @exec{CONFIG_MODE=...} to pass a configuration
mode on to your site-configuration module (accessible via the
@exec{current-mode} parameter). Supply @exec{CLEAN_MODE=--clean} to
make the default @racket[#:clean?] configuration for a client to
@racket[#t] instead of @racket[#f], supply
@exec{RELEASE_MODE=--release} to make the default @racket[#:release?]
configuration @racket[#t], supply @exec{SOURCE_MODE=--source} to make the
default @racket[#:source?] configuration @racket[#t], and supply
@exec{VERSIONLESS_MODE=--version} to make the default
@racket[#:versionless?] configuration @racket[#t].

A configuration file can specify the packages to include, host address
of the server, distribution name, installer directory, and
documentation search URL, but defaults can be provided as @exec{make}
arguments via @exec{PKGS}, @exec{SERVER} plus @exec{SERVER_PORT} plus
@exec{SERVER_HOSTS}, @exec{DIST_NAME}, @exec{DIST_BASE}, and
@exec{DIST_DIR}, @exec{DOC_SEARCH}, respectively. The site
configuration's top-level options for packages and documentation
search URL are used to configure the set of packages that are
available to client machines to include in installers.

For each installer written to @filepath{build/installers}, the
installer's name is

@centerline{@filepath{@nonterm{dist-base}-@nonterm{version}-@nonterm{platform}-@nonterm{dist-suffix}.@nonterm{ext}}}

where @nonterm{dist-base} defaults to @filepath{racket} (but can be
set via @exec{DIST_BASE}), @nonterm{platform} is from
@racket[(system-library-subpath #f)] but normalizing the Windows
results to @filepath{i386-win32} and @filepath{x86_63-win32},
-@nonterm{dist-suffix} is omitted unless a @racket[#:dist-suffix]
string is specified for the client in the site configuration, and
@nonterm{ext} is platform-specific: @filepath{.sh} for Unix (including
Linux), @filepath{.dmg} or @filepath{.pkg} for Mac OS, and
@filepath{.exe} for Windows.

The server supports both @racket['cs] and @racket['3m] clients by
creating built packages in machine-independent form (which is then
recompiled to the client's native format, still much faster than
compiling from source). Set @exec{SERVER_COMPILE_MACHINE=} to disable
machine-independent format for built packages.

@; ------------------------------------------------------------
@section{Generating Installer Web Sites}

The @exec{site} target of the makefile uses the @exec{installers} target to
generate a set of installers, and then it combines the installers,
packages, a package catalog, and log files into a directory that is
suitable for access via a web server.

Supply the same @exec{CONFIG=...} and @exec{CONFIG_MODE=...} arguments for
@exec{site} as for @exec{installers}. The configuration file should have a
@racket[#:dist-base-url] entry for the URL where installers and packages will
be made available; the @exec{installers} target uses @racket[#:dist-base-url] to
embed suitable configuration into the installers. Specifically,
installers are configured to access pre-built packages and
documentation from the site indicated by @racket[#:dist-base-url].

Note that @racket[#:dist-base-url] should almost always end with
@filepath{/}, since others URLs will be constructed as relative to
@racket[#:dist-base-url].

The site is generated as @filepath{build/site} by default. A
@racket[#:site-dest] entry in the configuration file can select an
alternate destination.

Use the @exec{site-from-installers} makefile target to perform the
part of @exec{site} that happens after @exec{installers} (i.e., to
generate a @exec{site} from an already-generated set of installers).

@; ------------------------------------------------------------
@section{Managing Snapshot Web Sites}

The @exec{snapshot-site} makefile target uses @exec{site} (so supply
the same @exec{CONFIG=...} and @exec{CONFIG_MODE=...} arguments), and
then treats the resulting site as a snapshot with additional
snapshot-management tasks.

For snapshot management, the destination of the files generated for
@exec{site} (as specified by @racket[#:site-dest]) should be within a
directory of snapshots. The configuration file can use
@racket[(current-stamp)] to get a string that represents the current
build, and then use the string both for @racket[#:dist-base-url] and
@racket[#:site-dest]. Normally, the stamp string is a combination of
the date and Git commit hash.

Snapshot management includes creating an @filepath{index.html} file in
the snapshots directory (essentially a copy of the snapshot's own
@filepath{index.html}) and pruning snapshot subdirectories to keep the
number of snapshots at the amount specified by
@racket[#:max-snapshots] configuration-file entry (with a default
value of @racket[5]).

Use the @exec{snapshot-at-site} makefile target to perform the part of
@exec{snapshot-site} that happens after @exec{site} (i.e., to manage
snapshots around an already-generated site).

@; ------------------------------------------------------------
@section{Separate Server and Clients}

Instead of using the @exec{installers} makefile target and a site
configuration file, you can run server and client processes manually.

Roughly, the steps are as follows

@itemlist[#:style 'ordered

 @item{On the server machine:

       @commandline{make server PKGS="@nonterm{pkgs}"}

       See step 2 in the detailed steps below for more information on
       variables other than @exec{PKGS} that you can provide with
       @exec{make}.}

 @item{On each client machine:
 
       @commandline{make client SERVER=@nonterm{address} PKGS="@nonterm{pkgs}"}

       or

       @commandline{nmake win32-client SERVER=@nonterm{address} PKGS="@nonterm{pkgs}"}

      See 4 in the detailed steps below for more information on
      variables other than @exec{SERVER} and @exec{PKGS} that you can
      provide with @exec{make}.}

]

In more detail, the steps are as follows:

@itemlist[#:style 'ordered


 @item{Build @exec{racket} on a server.
    
       The @exec{base} target of the makefile will do that, if you
       haven't done it already. (The server only works on non-Windows
       platforms, currently.)}

 @item{On the server, build packages and start a catalog server.

      The @exec{server-from-base} target of the makefile will do that.

      Alternatively, use the @exec{server} target, which combines
      @exec{base} and @exec{server-from-base} (i.e., steps 1 and 2).

      The @exec{SERVER_PORT} variable of the makefile choose the port
      on which the server listens to clients. The default is port
      @tt{9440}.

      The @exec{SERVER_HOSTS} variable of the makefile determines the
      interfaces at which the server listens. The default is
      @tt{localhost} which listens only on the loopback device (for
      security). Supply the empty string to listen on all interfaces.
      Supply multiple addresses by separating them with a comma.

      The @exec{PKGS} variable of the makefile determines which
      packages are built for potential inclusion in a distribution.

      The @exec{DOC_SEARCH} variable of the makefile determine a URL
      that is embedded in rendered documentation for cases where a
      remote search is needed (because other documentation is not
      installed).

      The @exec{SRC_CATALOG} variable determines the catalog that is
      used to get package sources and native-library packages. The
      default is @tt{http://pkgs.racket-lang.org}.

      The @exec{SERVER_PKG_INSTALL_OPTIONS} variable determines extra
      flags that are passed to @exec{raco pkg install} when installing
      on the server (to create package builds that are sent to
      clients). For example,
      @exec{SERVER_PKG_INSTALL_OPTIONS=--source} could be useful to
      ensure that the server always builds from sources.

      The @exec{PACK_BUILT_OPTIONS} variable can be set to
      @exec{--mode @nonterm{mode}} to set the package mode for built
      packages. The default @exec{infer} mode infers uses the
      package's @exec{distribution-preference} @filepath{info.rkt}
      field, if any, infers @exec{binary} if the package has any
      native libraries and no Racket sources, and infers @exec{built}
      otherwise.

      The server provides README files from the
      @filepath{build/readmes} directory. If @filepath{README.txt}
      does not exist when the sever is started, a default file is
      created (and clients download @filepath{README.txt} by default).

      If you stop the server and want to restart it, use the
      @exec{built-package-server} makefile target instead of starting
      over with the @exec{server} target.}

 @item{On each client (one for each platform to bundle), build @exec{racket}.

      This is the same as step 1, but on each client. If the client and
      server are the same, there's nothing more to do for step 3.}

 @item{On each client, create an installer.

      The @exec{client} (or @exec{win32-client}) target of the
      makefile will do that.

      Provide @exec{SERVER} as the hostname of the server machine, but
      a @tt{localhost}-based tunnel back to the server is more secure
      and avoids the need to specify @exec{SERVER_HOSTS} when starting
      the server in step 2. Also, provide @exec{SERVER_PORT} if an
      alternate port was specified in step 2.

      Provide the same @exec{PKGS} (or a subset) as in step 2 if you
      want a different set than the ones listed in the makefile.
      Similarly, @exec{DOC_SEARCH} normally should be the same as in
      step 2, but for a client, it affects future documentation builds
      in the installation.

      Alternatively, use the @exec{client} target, which combines
      @exec{base} and @exec{client-from-base} (i.e., steps 3 and 4).

      On Windows, you need NSIS installed, either in the usual location
      or with @exec{makensis} in your command-line path.

      To create a release installer, provide @exec{RELEASE_MODE} as
      @DFlag{release} to @exec{make}. A release installer has slightly
      different defaults that are suitable for infrequently updated
      release installations, as opposed to frequently updated snapshot
      installations.

      To create a source archive, provide @exec{SOURCE_MODE} as
      @DFlag{source} to @exec{make}.

      To create an archive that omits the version number and also omit
      and version number in installer paths, provide
      @exec{VERSIONLESS_MODE} as @DFlag{versionless} to @exec{make}.

      To change the human-readable name of the distribution as
      embedded in the installer, provide @exec{DIST_NAME} to
      @exec{make}. The default distribution name is @tt{Racket}.
      Whatever name you pick, the Racket version number is
      automatically added for various contexts.

      To change the base name of the installer file, provide
      @exec{DIST_BASE} to @exec{make}. The default is @tt{racket}.

      To change the directory name for installation on Unix (including
       Linux), provide @exec{DIST_DIR} to @exec{make}. The default is
       @tt{racket}.

      To add an extra piece to the installer's name, such as an
      identifier for a variant of Linux, provide @exec{DIST_SUFFIX} to
      @exec{make}. The default is @tt{"", which} omits the prefix and
      its preceding hyphen.

      To set the description string for the installer, provide
      @exec{DIST_DESC} to @exec{make}. The description string is
      recorded alongside the installer.

      To set the initial package catalogs URLs for an installation,
      provide @exec{DIST_CATALOGS_q} to @exec{make}. Separate multiple
      URLs with a space, and use an empty string in place of a URL to
      indicate that the default catalogs should be used. The @tt{_q}
      in the variable name indicates that its value can include double
      quotes (but not single quotes)---which are needed to specify an
      empty string, for example.

      To select a @filepath{README} file for the client, provide
      @exec{README} to @exec{make}. The @exec{README} value is used as
      a file name to download from the server.

      To create a @filepath{.tgz} archive instead of an installer (or
      any platform), set @exec{TGZ_MODE} to @DFlag{tgz}.

      For a Mac OS installer, set @exec{SIGN_IDENTITY} as the name to
      which the signing certificate is associated. Set
      @exec{MAC_PKG_MODE} to @DFlag{mac-pkg} to create a
      @filepath{.pkg} installer instead of a @filepath{.dmg} image.

      For a Windows installer, set @exec{OSSLSIGNCODE_ARGS_BASE64} as
      a Base64 encoding of an S-expression for a list of argument
      strings for @exec{osslsigncode}. The @Flag{n}, @Flag{t},
      @Flag{in}, and @Flag{out} arguments are provided to
      @exec{osslsigncode} automatically, so supply the others.

      The @exec{SERVER_CATALOG_PATH} and @exec{SERVER_COLLECTS_PATH}
      makefile variables specify paths at @exec{SERVER} plus
      @exec{SERVER_PORT} to access the package catalog and pre-built
      @filepath{collects} tree needed for a client, but those paths
      should be empty for a server started with @exec{make server},
      and they are used mainly by @exec{make client-from-site}
      (described below).

      The @exec{UPLOAD} makefile variable specifies a URL to use as an
      upload destination for the created installed, where the
      installer's name is added to the end of the URL, or leave as
      empty for no upload.}

]

On each client, step 4 produces a @filepath{bundle/installer.txt} file
that contains the path to the generated installer on one line,
followed by the description on a second line. The installer is also
uploaded to the server, which leaves the installer in a
@filepath{build/installers} directory and records a mapping from the
installer's description to its filename in
@filepath{build/installers/table.rktd}.

If you provide @exec{JOB_OPTIONS=@nonterm{options}} for either a
client or server build, the options are used both for @exec{raco
setup} and @exec{raco pkg install}. Normally, @exec{JOB_OPTIONS} is
used to control parallelism.

@; ------------------------------------------------------------
@section{Creating a Client from an Installer Web Site}

If you (or someone else) previously created an installer site with
@exec{make site}, then @exec{make client-from-site} in a clean
repository creates an installer for the current platform drawing
packages from the site.

At a minimum, provide @exec{SERVER}, @exec{SERVER_PORT} (usually 80 or 443),
@exec{SERVER_URL_SCHEME} (if @litchar{https} instead of @litchar{http})
and @exec{SITE_PATH} (if not empty, include a trailing @litchar{/})
makefile variables to access a site at

@centerline{@tt{http://$(SERVER):$(SERVER_PORT)/$(SITE_PATH)}}

The @exec{client-from-site} makefile target chains to @exec{make
client} while passing suitable values for @exec{DIST_CATALOGS_q},
@exec{DOC_SEARCH}, @exec{SERVER_CATALOG_PATH}, and
@exec{SERVER_COLLECTS_PATH}. Supply any other suitable variables, such
as @exec{DIST_NAME} or @exec{RELEASE_MODE}, the same as for @exec{make
client}.
