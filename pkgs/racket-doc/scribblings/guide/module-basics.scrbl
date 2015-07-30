#lang scribble/doc
@(require scribble/manual 
          scribble/eval 
          "guide-utils.rkt"
          "module-hier.rkt"
          (for-label setup/dirs
                     setup/link
                     racket/date))

@title[#:tag "module-basics"]{Module Basics}

Each Racket module typically resides in its own file. For example,
suppose the file @filepath{cake.rkt} contains the following module:

@racketmod[
#:file "cake.rkt"
racket

(provide print-cake)

(code:comment @#,t{draws a cake with @racket[n] candles})
(define (print-cake n)
  (show "   ~a   " n #\.)
  (show " .-~a-. " n #\|)
  (show " | ~a | " n #\space)
  (show "---~a---" n #\-))

(define (show fmt n ch)
  (printf fmt (make-string n ch))
  (newline))
]

Then, other modules can import @filepath{cake.rkt} to use the
@racket[print-cake] function, since the @racket[provide] line in
@filepath{cake.rkt} explicitly exports the definition
@racket[print-cake]. The @racket[show] function is private to
@filepath{cake.rkt} (i.e., it cannot be used from other modules),
since @racket[show] is not exported.

The following @filepath{random-cake.rkt} module imports
@filepath{cake.rkt}:

@racketmod[
#:file "random-cake.rkt"
racket

(require "cake.rkt")

(print-cake (random 30))
]

The relative reference @racket["cake.rkt"] in the import
@racket[(require "cake.rkt")] works if the @filepath{cake.rkt} and
@filepath{random-cake.rkt} modules are in the same
directory. Unix-style relative paths are used for relative module
references on all platforms, much like relative URLs in HTML pages.

@; ----------------------------------------
@section[#:tag "module-org"]{Organizing Modules}

The @filepath{cake.rkt} and @filepath{random-cake.rkt} example
demonstrates the most common way to organize a program into modules:
put all module files in a single directory (perhaps with
subdirectories), and then have the modules reference each other
through relative paths. A directory of modules can act as a
project, since it can be moved around on the filesystem or copied to
other machines, and relative paths preserve the connections among
modules.

As another example, if you are building a candy-sorting program, you
might have a main @filepath{sort.rkt} module that uses other modules
to access a candy database and a control sorting machine. If the
candy-database module itself is organized into sub-modules that handle
barcode and manufacturer information, then the database module could
be @filepath{db/lookup.rkt} that uses helper modules
@filepath{db/barcodes.rkt} and @filepath{db/makers.rkt}.  Similarly,
the sorting-machine driver @filepath{machine/control.rkt} might use
helper modules @filepath{machine/sensors.rkt} and
@filepath{machine/actuators.rkt}.

@centerline[module-hierarchy]

The @filepath{sort.rkt} module uses the relative paths
@filepath{db/lookup.rkt} and @filepath{machine/control.rkt} to import
from the database and machine-control libraries:

@racketmod[
#:file "sort.rkt"
racket
(require "db/lookup.rkt" "machine/control.rkt")
....]

The @filepath{db/lookup.rkt} module similarly uses paths relative to
its own source to access the @filepath{db/barcodes.rkt} and
@filepath{db/makers.rkt} modules:


@racketmod[
#:file "db/lookup.rkt"
racket
(require "barcode.rkt" "makers.rkt")
....]

Ditto for @filepath{machine/control.rkt}:

@racketmod[
#:file "machine/control.rkt"
racket
(require "sensors.rkt" "actuators.rkt")
....]

Racket tools all work automatically with relative paths. For example,

@commandline{racket sort.rkt}

on the command line runs the @filepath{sort.rkt} program and
automatically loads and compiles required modules. With a large enough
program, compilation from source can take too long, so use

@commandline{raco make sort.rkt}

@margin-note{See @secref[#:doc '(lib "scribblings/raco/raco.scrbl")
"make"] for more information on @exec{raco make}.}

to compile @filepath{sort.rkt} and all its dependencies to bytecode
files. Running @exec{racket sort.rkt} will automatically use bytecode
files when they are present.

@; ----------------------------------------
@section{Library Collections}

A @deftech{collection} is a hierarchical grouping of installed library modules.  A
module in a @tech{collection} is referenced through an unquoted,
suffixless path. For example, the following module refers to the
@filepath{date.rkt} library that is part of the @filepath{racket}
@tech{collection}:

@racketmod[
racket

(require racket/date)

(printf "Today is ~s\n"
        (date->string (seconds->date (current-seconds))))
]

When you search the online Racket documentation, the search results
indicate the module that provides each binding. Alternatively, if you
reach a binding's documentation by clicking on hyperlinks, you can
hover over the binding name to find out which modules provide
it.

A module reference like @racketmodname[racket/date] looks like an
identifier, but it is not treated in the same way as @racket[printf]
or @racket[date->string]. Instead, when @racket[require] sees a module
reference that is unquoted, it converts the reference to a
collection-based module path:

@itemlist[

 @item{First, if the unquoted path contains no @litchar{/}, then
       @racket[require] automatically adds a @filepath{/main} to the
       reference. For example, @racket[(require
       @#,racketmodname[slideshow])] is equivalent to @racket[(require
       slideshow/main)].}

 @item{Second, @racket[require] implicitly adds a @filepath{.rkt}
       suffix to the path.}

 @item{Finally, @racket[require] resolves the path by searching among
       installed @tech{collections}, instead of treating the path as relative to
       the enclosing module's path.}

]

To a first approximation, a @tech{collection} is implemented as a
filesystem directory. For example, the @filepath{racket} collection is
mostly located in a @filepath{racket} directory within the Racket
installation's @filepath{collects} directory, as reported by

@racketmod[
racket

(require setup/dirs)

(build-path (find-collects-dir) (code:comment @#,t{main collection directory})
            "racket")
]

The Racket installation's @filepath{collects} directory, however, is
only one place that @racket[require] looks for collection directories.
Other places include the user-specific directory reported by
@racket[(find-user-collects-dir)] and directories configured through
the @envvar{PLTCOLLECTS} search path. Finally, and most typically,
collections are found through installed @tech{packages}.

@; ----------------------------------------
@section[#:tag "packages-and-collections"]{Packages and Collections}

A @deftech{package} is a set of libraries that are installed through
the Racket package manager (or included as pre-installed in a Racket
distribution). For example, the @racketmodname[racket/gui] library is
provided by the @filepath{gui} package, while
@racketmodname[parser-tools/lex] is provided by the
@filepath{parser-tools} library.@margin-note{More precisely,
@racketmodname[racket/gui] is provided by @filepath{gui-lib},
@racketmodname[parser-tools/lex] is provided by
@filepath{parser-tools-lib}, and the @filepath{gui} and
@filepath{parser-tools} packages extend @filepath{gui-lib} and
@filepath{parser-tools-lib} with documentation.}

Racket programs do not refer to @tech{packages} directly. Instead,
programs refer to libraries via @tech{collections}, and adding or
removing a @tech{package} changes the set of collection-based
libraries that are available. A single package can supply
libraries in multiple collections, and two different packages can
supply libraries in the same collection (but not the same libraries,
and the package manager ensures that installed packages do not
conflict at that level).

For more information about packages, see @other-manual['(lib
"pkg/scribblings/pkg.scrbl")].

@; ----------------------------------------
@section[#:tag "link-collection"]{Adding Collections}

Looking back at the candy-sorting example of @secref["module-org"],
suppose that modules in @filepath{db/} and @filepath{machine/} need a
common set of helper functions. Helper functions could be put in a
@filepath{utils/} directory, and modules in @filepath{db/} or
@filepath{machine/} could access utility modules with relative paths
that start @filepath{../utils/}. As long as a set of modules work
together in a single project, it's best to stick with relative paths.
A programmer can follow relative-path references without knowing about
your Racket configuration.

Some libraries are meant to be used across multiple projects, so that
keeping the library source in a directory with its uses does not make
sense. In that case, the best option is add a new
@tech{collection}. After the library is in a collection, it can be
referenced with an unquoted path, just like libraries that are
included with the Racket distribution.

You could add a new collection by placing files in the Racket
installation or one of the directories reported by
@racket[(get-collects-search-dirs)]. Alternatively, you could add to
the list of searched directories by setting the @envvar{PLTCOLLECTS}
environment variable.@margin-note*{If you set @envvar{PLTCOLLECTS},
include an empty path in by starting the value with a colon (Unix and
Mac OS X) or semicolon (Windows) so that the original search paths are
preserved.} The best option, however, is to add a @tech{package}.

Creating a package @emph{does not} mean that you have to register with
a package server or perform a bundling step that copies your source
code into an archive format. Creating a package can simply mean using
the package manager to make your libraries locally accessible as a
collection from their current source locations.

For example, suppose you have a directory @filepath{/usr/molly/bakery}
that contains the @filepath{cake.rkt} module (from the
@seclink["module-basics"]{beginning} of this section) and other
related modules. To make the modules available as a @filepath{bakery}
collection, either

@itemlist[

 @item{Use the @exec{raco pkg} command-line tool:

        @commandline{raco pkg install --link /usr/molly/bakery}

       where the @DFlag{link} flag is not actually needed when the
       provided path includes a directory separator.}

 @item{Use DrRacket's @onscreen{Package Manager} item from the
       @onscreen{File} menu. In the @onscreen{Do What I Mean} panel,
       click @onscreen{Browse...}, choose the
       @filepath{/usr/molly/bakery} directory, and click
       @onscreen{Install}.}

]

Afterward, @racket[(require bakery/cake)] from any module will import
the @racket[print-cake] function from
@filepath{/usr/molly/bakery/cake.rkt}.

By default, the name of the directory that you install is used both as
the @tech{package} name and as the @tech{collection} that is provided
by the package.  Also, the package manager normally defaults to
installation only for the current user, as opposed to all users of a
Racket installation. See @other-manual['(lib
"pkg/scribblings/pkg.scrbl")] for more information.

If you intend to distribute your libraries to others, choose
collection and package names carefully. The collection namespace is
hierarchical, but top-level collection names are global, and the
package namespace is flat. Consider putting one-off libraries under
some top-level name like @filepath{molly} that identifies the
producer.  Use a collection name like @filepath{bakery} when producing
the definitive collection of baked-goods libraries.

After your libraries are put in a @tech{collection} you can still
use @exec{raco make} to compile the library sources, but it's better
and more convenient to use @exec{raco setup}. The @exec{raco setup}
command takes a collection name (as opposed to a file name) and
compiles all libraries within the collection. In addition, @exec{raco setup} can
build documentation for the collection and add it to the documentation
index, as specified by a @filepath{info.rkt} module in the collection.
See @secref[#:doc '(lib "scribblings/raco/raco.scrbl") "setup"] for
more information on @exec{raco setup}.
