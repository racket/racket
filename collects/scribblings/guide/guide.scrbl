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

@bold{This guide describes a potential future version of PLT Scheme.
      It does not match the current implementation.}

@table-of-contents[]

@include-section["welcome.scrbl"]

@include-section["to-scheme.scrbl"]

@include-section["data.scrbl"]

@include-section["forms.scrbl"]

@include-section["define-struct.scrbl"]

@include-section["modules.scrbl"]

@include-section["io.scrbl"]

@; ----------------------------------------------------------------------
@section[#:tag "guide:contracts"]{Contracts}

In the reference manual, the documentation for each procedure
describes the acceptable arguments and the result of the procedure
using @idefterm{contracts}.

@; ----------------------------------------------------------------------
@include-section["class.scrbl"]


@; ----------------------------------------------------------------------
@section[#:tag "control"]{Exceptions and Control}


@; ----------------------------------------------------------------------
@include-section["for.scrbl"]


@; ----------------------------------------------------------------------
@section[#:tag "regexp"]{Regular-Expression Matching@aux-elem{ (Regexps)}}


@; ----------------------------------------------------------------------
@section[#:tag "match"]{Pattern Matching}

@subsection{Simple Dispatch: @scheme[case]}

The @scheme[case] form dispatches to a clause by matching the result
of an expression to the values for the clause:

@specform[(case [(_datum ...+) expr ...+]
                ...)]

@; ----------------------------------------------------------------------
@include-section["qq.scrbl"]

@; ----------------------------------------------------------------------
@section[#:tag "units"]{Units (Higher-Order Modules)}


@; ----------------------------------------------------------------------
@section[#:tag "threads"]{Threads}

@; ----------------------------------------------------------------------
@include-section["macros.scrbl"]


@; ----------------------------------------------------------------------
@section[#:tag "reflection"]{Reflection and Dynamic Evaluation}


@; ----------------------------------------------------------------------
@section[#:tag "macros"]{Reader Extension}

@; ----------------------------------------------------------------------
@section[#:tag "security"]{Security}


@; ----------------------------------------------------------------------
@section[#:tag "memory-management"]{Memory Management}

@subsection[#:tag "guide:weakboxes"]{Weak Boxes}

@subsection[#:tag "guide:ephemerons"]{Ephemerons}

@; ----------------------------------------------------------------------
@section[#:tag "performance"]{Performance}

Every definition or expression is compiled to an internal bytecode
format. Standard optimizations are applied when compiling the
bytecode. For example, in an environment where when @scheme[+] has its
usual binding, the expression @scheme[(let ([x 1][y (lambda () 4)]) (+
1 (y)))] is compiled the same as the constant @scheme[5] due to
constant propagation, constant folding, and inlining optimizations.


@; ----------------------------------------------------------------------
@section[#:tag "ffi"]{Foreign-Function Interface@aux-elem{ (FFI)}}


@; ----------------------------------------------------------------------
@section[#:tag "scripts"]{Scripts}


@; ----------------------------------------------------------------------
@section[#:tag "gui"]{Graphical User Interfaces@aux-elem{ (GUIs)}}


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
