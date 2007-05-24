#reader(lib "docreader.ss" "scribble")
@require[(lib "manual.ss" "scribble")]
@require[(lib "eval.ss" "scribble")]
@require["guide-utils.ss"]

@title{A Guide to PLT Scheme}

This guide is intended for programmers who are new to Scheme, new to
PLT Scheme, or new to some part of PLT Scheme. 

If you are new to programming, consider instead reading @|HtDP|.

If you want a quicker, prettier, higher-level overview of PLT Scheme,
start with @|Quick|.

For everyone else, this guide assumes some programming experience.
After a somewhat gentle introduction to Scheme (chapters 1 through 4),
we dive into the details of putting Scheme to work. This guide covers
much of the PLT Scheme toolbox, but it leaves the nitty-gritty details
to @|MzScheme| and other reference manuals.

@table-of-contents[]

@include-section["welcome.scrbl"]
@include-section["syntax.scrbl"]
@include-section["lists.scrbl"]
@include-section["truth.scrbl"]

@; ----------------------------------------------------------------------
@section[#:tag "datatypes"]{Built-In and Programmer-Defined Datatypes}

We have seen some of Scheme's built-in datatypes, including numbers,
strings, lists, and procedures. This section provides a more complete
coverage of the built-in datatypes, and also introduces the
@scheme[define-struct] form for creating your own datatypes. We defer
a discussion of the class-based object system to @secref["classes"].

...

@; ----------------------------------------------------------------------
@section[#:tag "scheme-read"]{Reading and Printing}

As we mentioned @seclink["syntax-overview"]{before}, the syntax
of a Scheme program is specified in an unusual way. Specifically, it
is defined by two phases:

@itemize{

 @item{a @defterm{read} phase, which parses a stream of characters
       into a tree-structured @defterm{S-expression}; and}

 @item{an @defterm{expand} phase, which parses the S-expression into a
       program.}

}

The second phase is particularly easy to extend, so that Scheme has no
grammar in the traditional sense. At the same time, the first phase is
useful on it's own, and directly available through the @scheme[read]
function. Furthermore, reading of data is naturally matched with
conventions for @defterm{printing} data.

@; ----------------------------------------------------------------------
@section[#:tag "scheme-forms"]{Programs and Expressions}

@subsection{Module Basics}

@subsection{Definition and Expression Forms}

@; ----------------------------------------------------------------------
@section[#:tag "contracts"]{Contracts}

In the reference manual, the documentation for each procedure
describes the acceptable arguments and the result of the procedure
using @idefterm{contracts}.

@; ----------------------------------------------------------------------
@section[#:tag "classes"]{Classes and Objects}


@; ----------------------------------------------------------------------
@section[#:tag "control"]{Exceptions and Control}


@; ----------------------------------------------------------------------
@section[#:tag "threads"]{Threads}


@; ----------------------------------------------------------------------
@section[#:tag "i/o"]{I/O and Networking}


@; ----------------------------------------------------------------------
@section[#:tag "regexp"]{Regular-Expression Matching (Regexps)}


@; ----------------------------------------------------------------------
@section[#:tag "match"]{Pattern Matching}


@; ----------------------------------------------------------------------
@section[#:tag "units"]{Higher-Order Modules (Units)}


@; ----------------------------------------------------------------------
@section[#:tag "macros"]{Syntactic Extension (Modules and Macros)}


@; ----------------------------------------------------------------------
@section[#:tag "reflection"]{Reflection and Dynamic Evaluation}


@; ----------------------------------------------------------------------
@section[#:tag "macros"]{Reader Extension}


@; ----------------------------------------------------------------------
@section[#:tag "security"]{Security}


@; ----------------------------------------------------------------------
@section[#:tag "memory-management"]{Memory Management}


@; ----------------------------------------------------------------------
@section[#:tag "performance"]{Performance}


@; ----------------------------------------------------------------------
@section[#:tag "ffi"]{Foreign-Function Interface (FFI)}


@; ----------------------------------------------------------------------
@section[#:tag "scripts"]{Scripts}


@; ----------------------------------------------------------------------
@section[#:tag "gui"]{Graphical User Interfaces (GUIs)}


@; ----------------------------------------------------------------------
@section[#:tag "tools"]{More Tools}

In the @seclink["intro"]{introduction}, we mentioned that PLT Scheme
includes more tools bsides DrScheme and MzScheme:

@itemize{

 @tool["MrEd"]{extends MzScheme with graphical user interface (GUI)
 and drawing primitives}

 @tool["Setup PLT"]{a command-line tool for installation tasks}

 @tool["planet"]{a command-line tool for managing packages that are
 normally downloaded automatically, on demand}

 @tool["mzc"]{a command-line tool for miscellaneous tasks, such as
 compiling Scheme source, compiling C-implemented extensions to the
 run-time system, generating executables, and building distribution
 packages}

}

@; ----------------------------------------------------------------------

@index-section["guide-index"]
