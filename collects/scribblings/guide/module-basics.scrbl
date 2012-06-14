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

on the comamnd line runs the @filepath{sort.rkt} program and
automatically loads and compiles required modules. With a large enough
program, compilation from source can take too long, so use

@commandline{raco make sort.rkt}

to compile @filepath{sort.rkt} and all its dependencies to bytecode
files. Running @exec{racket sort.rkt} will automatically use bytecode
files when they are present.

@margin-note{See @secref[#:doc '(lib "scribblings/raco/raco.scrbl")
"make"] for more information on @exec{raco make}.}

@; ----------------------------------------
@section{Library Collections}

A @deftech{collection} is a set of installed library modules.  A
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

 @item{Finally, @racket[require] treats the path as relative to the
       installation location of the collection, instead of relative to
       the enclosing module's path.}

]

The @filepath{racket} collection is located in a directory with the
Racket installation. A user-specific directory can contain additional
collections, and even more collection directories can be specified in
configuration files or through the @envvar{PLTCOLLECTS} search
path. Try running the following program to find out how your
collection search path is configured:

@racketmod[
racket

(require setup/dirs)

(find-collects-dir) (code:comment @#,t{main collection directory})
(find-user-collects-dir) (code:comment @#,t{user-specific collection directory})
(get-collects-search-dirs) (code:comment @#,t{complete search path})
]

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
sense. In that case, you have two options:

@itemlist[

 @item{Add the library to a new or existing @tech{collection}. After
       the library is in a collection, it can be referenced with an
       unquoted path, just like libraries that are included with the
       Racket distribution.}

 @item{Add the library to a new or existing @|PLaneT| package. Libraries
       in a @|PLaneT| package are referenced with a path of the form
       @racket[(planet ....)] path.
       @margin-note*{See @other-doc['(lib "planet/planet.scrbl")]
       for more information on @|PLaneT|.}}

]

The simplest option is to add a new collection. You could add a new
collection by placing files in the Racket installation or one of the
directories reported by
@racket[(get-collects-search-dirs)]. Alternatively, you could add to
the list of searched directories by setting the @envvar{PLTCOLLECTS}
environment variable; if you set @envvar{PLTCOLLECTS}, include an
empty path in by starting the value with a colon (Unix and Mac OS X)
or semicolon (Windows) so that the original search paths are
preserved. Finally, instead of using one of the default directories or
setting @envvar{PLTCOLLECTS}, you can use @exec{raco link}.

The @exec{raco link} command-line tool creates a link from a
collection name to a directory for the collection's modules. For
example, suppose you have a directory @filepath{/usr/molly/bakery}
that contains the @filepath{cake.rkt} module (from the
@seclink["module-basics"]{beginning} of this section) and other
related modules. To make the modules available as a @filepath{bakery}
collection, use @margin-note*{Instead of installing a single
collection directory, the @DFlag{root} or @Flag{d} flag for @exec{raco
link} can install a directory that contains collections, much like
adding to @envvar{PLTCOLLECTS}.}

@commandline{raco link /usr/molly/bakery}

Afterward, @racket[(require bakery/cake)] from any module will import
the @racket[print-cake] function from
@filepath{/usr/molly/bakery/cake.rkt}.

To make a collection name different from the name of the directory
that contains the collection's modules, use the @DFlag{name} or
@Flag{n} option for @exec{raco link}. By default, @exec{raco link}
installs a collection link only for the current user, but you can
supply the @DFlag{installation} or @Flag{i} flag to install the link
for all users of your Racket installation.

@margin-note{See @secref[#:doc '(lib "scribblings/raco/raco.scrbl")
"link"] for more information on @exec{raco link}.}

If you intend to distribute your library collection to others, choose
the collection name carefully. The collection namespace is
hierarchical, but (unlike @|PLaneT|) the collection system has no
built-in feature to avoid conflicts from different producers or
different versions. Consider putting one-off libraries under some
top-level name like @filepath{molly} that identifies the producer.
Use a collection name like @filepath{bakery} when producing the
definitive collection of baked-goods libraries.

After your libraries are put in a @tech{collection} you can still
use @exec{raco make} to compile the library sources, but it's better
and more convenient to use @exec{raco setup}. The @exec{raco setup}
command takes a collection name (as opposed to a file name) and
compiles all libraries within the collection. In addition, it can
build documentation for the collection and add it to the documentation
index, as specified by a @filepath{info.rkt} module in the collection.
See @secref[#:doc '(lib "scribblings/raco/raco.scrbl") "setup"] for
more information on @exec{raco setup}.
