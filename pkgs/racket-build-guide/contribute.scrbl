#lang scribble/manual
@(require "common.rkt"
          (for-label (only-in scribble/manual history)))

@title[#:tag "contribute"]{Contributing to Racket Development}

The Racket developers are happy to receive bug reports and
improvements to the implementation and documentation through GitHub
issues and pull requests:

@itemlist[

 @item{Issues (bug reports): @url{https://github.com/racket/racket/issues}}

 @item{Pull requests (improvements): @url{https://github.com/racket/racket/pulls}}

]

The Racket distribution includes scores of packages that have their
own separate repositories, which somewhat complicates the process of
sending pull requests. The mechanism is the same, but see
@secref["pkg-contribute"] for more guidance.

By making a contribution, you are agreeing that your contribution is
licensed under the LGPLv3, Apache 2.0, and MIT licenses. Those
licenses are available in the @hyperlink[git-repo]{Racket Git
repository} in the files @filepath{LICENSE.txt},
@filepath{LICENSE-APACHE.txt}, and @filepath{LICENSE-MIT.txt}.

@; ------------------------------------------------------------
@section[#:tag "main-contribute"]{Main-Repository Contributions}

The @hyperlink[git-repo]{main Racket Git repository} contains the
implementation of everything that is in the Minimal Racket
distribution. That includes the runtime system, core libraries, and
@exec{raco pkg} so that other packages can be installed.

The main Racket repository also has the source to the Racket
Reference, Racket Guide, and other core-ish documentation, including
the source to the document that you are reading. Those document
sources are in the repository's @filepath{pkgs} directory.

Finally, the main repository includes a few other packages that are
especially tightly bound to the runtime-system implementation, such as
the @filepath{compiler-lib} package or the @filepath{racket-test}
package. Those package sources are also in the repository's
@filepath{pkgs} directory.

To develop improvements to any of those parts of Racket, following the
usual GitHub-based workflow:

@itemlist[

 @item{Fork the Racket repository.}

 @item{Create an in-place build as described in @secref["build"].}

 @item{Make your changes and rebuild with @exec{make} or @exec{make
       as-is} or @exec{raco setup}, where @exec{raco setup} is the
       best choice when modifying Racket libraries that are in
       @filepath{collects} or a package. If your changes involve
       modifying things that are part of the @exec{racket} executable,
       then a simple @exec{make} may not suffice; see ``Modifying
       Racket'' in @filepath{racket/src/README.txt} for more
       information.}

 @item{Commit changes to your fork and
       @hyperlink["https://help.github.com/en/articles/creating-a-pull-request"]{submit
       a pull request}.}

]

See the @secref["contribute-guidelines"].

@; ------------------------------------------------------------
@section[#:tag "pkg-contribute"]{Distribution-Package Contributions}

If you find yourself changing a file that is in a
@filepath{share/pkgs} subdirectory (either installed as part of a
Racket release or as a product of an in-place build), then that file
is not part of the main Racket Git repository. It almost certainly has
its own Git repository somewhere else, possibly within
@url{https://github.com/racket}, but possibly in another user's space.
The name of the directory in @filepath{share/pkgs} is almost certainly
the package name.

To start working on a package @nonterm{pkg-name} from a Racket release
or snapshot, you first need to adjust the package installation to use
the source specified by the main package catalog

@commandline{raco pkg update @DFlag{no-setup} @DFlag{catalog} https://pkgs.racket-lang.org @nonterm{pkg-name}}

and then in the directory you'd like to hold the package's source

@commandline{raco pkg update @DFlag{clone} @nonterm{pkg-name}}

will clone the package's source Git repository into
@filepath{@nonterm{pkg-name}} within the current directory.

Alternatively, if you already have an in-place build of the main
Racket repository, you can start working on a package
@nonterm{pkg-name}, by going to the root directory of your Racket
repository checkout and running

@commandline{raco pkg update @DFlag{clone} extra-pkgs/@nonterm{pkg-name}}

That will create @filepath{extra-pkgs/@nonterm{pkg-name}} as a clone
of the package's source Git repository, it will replace the current
installation of the package in your Racket build to point at that
directory, and then it will rebuild (essentially by using @exec{raco
setup}) with the new location of the package installation. Now you can
edit in @filepath{extra-pkgs/@nonterm{pkg-name}}, and your changes
will be live.

Some information that might improve your experience:

@itemlist[

 @item{You can add @DFlag{no-setup} to the @exec{raco pkg update}
       command to skip the @exec{raco setup} step, which makes sense
       if you want to make changes and then run @exec{raco setup}
       yourself.}

 @item{A package is sometimes a subdirectory within a Git repository,
       and it would be better if the checkout in @filepath{extra-pkgs}
       matched the repository name instead of the package name. If you
       know the repository name, you can use

       @commandline{raco pkg update @DFlag{clone} extra-pkgs/@nonterm{repo-name} @nonterm{pkg-name}}

       to make the distinction.}

 @item{This same approach will generally work if you're starting from
       a distribution installer instead of the checkout of the Racket
       sources from the main Git repository. You'll need write
       permission to the installation, though, so that @exec{raco pkg
       update} can redirect the package. Also, there's no particular
       reason to use @exec{extra-pkgs} in that case.}

 @item{If you're done and want to go back to the normal installation
       for @nonterm{pkg-name}, use

        @commandline{raco pkg update @DFlag{lookup} @nonterm{pkg-name}}}

 @item{See @secref["git-workflow" #:doc '(lib
       "pkg/scribblings/pkg.scrbl")] for more information about how
       packages are meant to work as Git repositories.}

]

Note that none of this is necessary if you're modifying a package in
the main Racket repository's @filepath{pkgs} directory. Those are
automatically linked in place for an in-place build of Racket.

@; ------------------------------------------------------------
@section[#:tag "contribute-guidelines"]{General Contribution Guidelines}

When you make a pull request, the Racket developers will help you get
the improvement in shape to merge to the Racket repository. You can
make that process faster by keeping a few guidelines in mind:

@itemlist[

 @item{Try to follow the @seclink["top" #:doc '(lib "scribblings/style/style.scrbl")]{style guide}.}

 @item{When you fix a bug or create a new feature, include a test
       case for it.

       Note that core Racket tests are in
       @filepath{pkgs/racket-test-core/tests/racket}, and tests for
       other libraries are also sometimes in a separate
       @filepath{-test} package.}

 @item{Include new or updated documentation as appropriate.

       To locate a documentation (Scribble) source file,
       visit the current documentation in a browser, and click at the page heading.
       A box will appear with a URL to a documentation source.
       Note that while it is likely that the documentation source will not be the file
       that you want to edit exactly, it should give you a rough idea for where it is.
       Particularly, the Racket reference is in
       @filepath{pkgs/racket-doc/scribblings/reference}, and the Racket guide is in
       @filepath{pkgs/racket-doc/scribblings/guide}.

       When adding to a library or extending an existing binding's
       behavior, be sure to include a @racket[history] note in the
       documentation to record the change.}

 @item{Build with your changes.

       Don't break the Racket build. That means at least checking that
       @exec{raco setup} runs and completes without errors. If you
       added or modified documentation, visually inspect the newly
       rendered documentation to make sure it reads as intended.

       A common mistake is to just run a modified library or its
       tests, but where a change creates a new package dependency that
       will only be detected by a full @exec{raco setup}.
       @italic{Really:} run @exec{raco setup}.}

 @item{For changes to the C code, ensure your code follows the C99 standard.

       On Unix systems, extensions that are part of the @exec{_DEFAULT_SOURCE}
       pre-processor flag are also allowed. See the
       @hyperlink["https://www.gnu.org/software/libc/manual/html_node/Feature-Test-Macros.html#index-_005fDEFAULT_005fSOURCE"]{glibc}
       manual for more details.}

]

@; ------------------------------------------------------------
@section[#:tag "contribute-more"]{More Resources}

For additional pointers on how to contribute to Racket, see

@centerline{@url{https://github.com/racket/racket/wiki/Ways-to-contribute-to-Racket}}
