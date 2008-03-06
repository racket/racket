#lang scribble/doc
@(require scribble/manual
          scribble/bnf
          (for-label setup/dirs
                     rnrs/programs-6))

@(define guide-src '(lib "scribblings/guide/guide.scrbl"))

@(define r6rs @elem{R@superscript{6}RS})

@title{@bold{R6RS}: Standard Language}

The ``The Revised@superscript{6} Report on the Algorithmic Language
Scheme'' @cite["Sperber07"] defines a dialect of Scheme. We use
@defterm{@|r6rs|} to refer to both the standard and the language
defined by the standard.

@|r6rs| defines both @defterm{libraries} and @defterm{top-level
programs}. Both correspond to PLT Scheme @defterm{modules} (see
@secref[#:doc guide-src "modules"]). That is, although @|r6rs| defines
top-level programs as entry points, you can just as easily treat a
library as an entry point when using PLT Scheme. The only difference
is that an @|r6rs| top-level program cannot export any bindings to
other modules.

@table-of-contents[]

@; ----------------------------------------

@section{Running Top-Level Programs}

To run a top-level program, either:

@itemize{

 @item{Use the @exec{plt-r6rs} executable, supplying the file that
       contains the program on the command line:

       @commandline{plt-r6rs @nonterm{program-file}}

       Additional command-line arguments are propagated as
       command-line arguments to the program (accessed via 
       @scheme[command-line]).

       To compile the file to bytecode (to speed future runs of the
       program), use @exec{plt-r6rs} with the @DFlag{compile} flag:

       @commandline{plt-r6rs --compile @nonterm{program-file}}
  
       The bytecode file is written in a @filepath{compiled}
       sub-directory next to @nonterm{program-file}.

       For example, if @filepath{hi.scm} contains

       @schemeblock[
       (import (rnrs))
       (display "hello\n")
       ]

       then

       @commandline{plt-r6rs hi.scm}

       prints ``hello.''}

 @item{Prefix the program with @schememetafont{#!r6rs}, which counts
       as a comment from the @|r6rs| perspective, but is a synonym for
       @scheme[#,(hash-lang) r6rs] from the PLT Scheme perspective.
       Such files can be run like any other PLT Scheme module, such as
       using @exec{mzscheme}:

       @commandline{mzscheme @nonterm{program-file}}

       or using DrScheme with the @onscreen{Module} language. The
       file can also be compiled to bytecode using @exec{mzc}:

       @commandline{mzc @nonterm{program-file}}

       For example, if @filepath{hi.ss} contains

       @schemeblock[
       #,(schememetafont "#!r6rs")
       (import (rnrs))
       (display "hello\n")
       ]

       then

       @commandline{mzscheme hi.ss}

       prints ``hello.'' Similarly, opening @filepath{hi.ss} in
       DrScheme and clicking @onscreen{Run} prints ``hello'' within
       the DrScheme interactions window.}

}

@; ----------------------------------------

@section{Installing Libraries}
       
To reference an @|r6rs| library from a top-level program or another
library, it must be installed as a collection-based library in PLT
Scheme.

One way to produce an @|r6rs| installed library is to create in
a @techlink[#:doc guide-src]{collection} a file that starts with
@schememetafont{#!r6rs} and that contains a @scheme[library] form. For
example, the following file might be created in a @filepath{hello.ss}
file within a @filepath{examples} collection directory:

       @schemeblock[
       #,(schememetafont "#!r6rs")
       (library (examples hello)
         (export greet)
         (import (rnrs))
         
         (define (greet)
           (display "hello\n")))
       ]

Alternately, the @exec{plt-r6rs} executable with the @DFlag{install}
flag accepts a sequence of @scheme[library] declarations and installs
them into separate files in a collection directory, based on the
declared name of each library:

 @commandline{plt-r6rs --install @nonterm{libraries-file}}

By default, libraries are installed into the user-specific collection
directory (see @scheme[find-user-collects-dir]). The @DFlag{all-users}
flag causes the libraries to be installed into the main installation,
instead (see @scheme[find-collects-dir]):

 @commandline{plt-r6rs --install --all-users @nonterm{libraries-file}}

See @secref["libpaths"] for information on how @|r6rs| library names
are turned into collection-based module paths, which determines where
the files are written. Libraries installed by @exec{plt-r6rs
@DFlag{install}} are automatically compiled to bytecode form.

@; ----------------------------------------

@section[#:tag "libpaths"]{Libraries and Collections}

An @|r6rs| library name is sequence of symbols, optionally followed by
a version as a sequence of exact, non-negative integers. Such a name
is converted to a PLT Scheme module pathname (see @secref[#:doc
guide-src "module-paths"]) by concatenating the symbols with a
@litchar{/} separator, and then appending the version integers each
with a preceeding @litchar{-}. As a special case, when an @|r6rs| path
contains a single symbol followed by a version, a @schemeidfont{main}
symbol is effectively inserted after the initial symbol.

Examples:

@schemeblock[
(rnrs io simple (6))  #, @elem{corresponds to}  rnrs/io/simple-6
(rnrs)                #, @elem{corresponds to}  rnrs
(rnrs (6))            #, @elem{corresponds to}  rnrs/main-6
]

When an @|r6rs| library or top-level program refers to another
library, it can supply version constraints rather than naming a
specific version. The version constraint is resolved at compile time
by searching the set of installed files.

@; ----------------------------------------

@section{Scheme Interoperability}

Using the conversion rules in @secref["libpaths"], and @r6rs library
can refer to modules that are implemented in other dialects supported
by PLT Scheme, and other PLT Scheme modules can refer to libraries
that are implemented in @|r6rs|.

Beware that a @defterm{pair} in @|r6rs| corresponds to a
@defterm{mutable pair} in @schememodname[scheme/base]. Otherwise,
@|r6rs| libraries and @schememodname[scheme/base] share the same
datatype for numbers, characters, strings, bytevectors (a.k.a. byte
strings), vectors, and so on. Hash tables are different. Input and
output ports from @schememodname[scheme/base] can be used directly as
binary ports with @|r6rs| libraries, and all @|r6rs| ports can be used
as ports in @schememodname[scheme/base] programs, but only textual
ports created via @|r6rs| libraries can be used by other @|r6rs|
operations that expect textual ports.

@; ----------------------------------------------------------------------

@(bibliography

 (bib-entry #:key "Sperber07"
            #:author "Michael Sperber, R. Kent Dybvig, Matthew Flatt, and Anton van Straaten (editors)"
            #:title @elem{The Revised@superscript{6} Report on the Algorithmic Language Scheme}
            #:date "2007"
            #:url "http://www.r6rs.org/")

)
