#reader(lib "docreader.ss" "scribble")
@require[(lib "manual.ss" "scribble")]
@require[(lib "eval.ss" "scribble")]
@require["guide-utils.ss"]

@title{A Guide to PLT Scheme}

This guide is intended for programmers who are new to Scheme, new to PLT
Scheme, or new to some part of PLT Scheme. It assumes
programming experience, so if you are new to programming, consider
instead reading @|HtDP|. If you want a quick and pretty overview of PLT
Scheme, start with @|Quick|. 

@seclink["to-scheme"]{Chapter 2} provides a brief introduction to
Scheme. From @seclink["datatypes"]{Chapter 3} on, this guide dives
into details---covering much of the PLT Scheme toolbox, but leaving
precise details to @|MzScheme| and other reference manuals.

@table-of-contents[]

@include-section["welcome.scrbl"]

@include-section["to-scheme.scrbl"]

@include-section["data.scrbl"]

@include-section["define-struct.scrbl"]

@include-section["forms.scrbl"]

@include-section["module-basics.scrbl"]

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
