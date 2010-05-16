#lang scribble/doc
@(require scribble/manual)

@title{@bold{Swindle}}

@defmodulelang[swindle]

Swindle extends Racket with many additional features.  The main
feature that started this project is a CLOS-like object system based
on Tiny-CLOS from Xerox, but there is a lot more.

Some documentation is available at
@link["http://barzilay.org/Swindle/"]{http://barzilay.org/Swindle/}.

@; @table-of-contents[]

@; ------------------------------
@section{Features}

The following is a high-level description of major features provided by
Swindle.  For every feature, the file that provides it is specified, if
only a subset of the system is needed.

@itemize[

@item{Some basic syntax extensions, including lambda &-keywords, and
  improved @scheme[define] and @scheme[let] forms.  (Available
  separately using @scheme[swindle/base])}

@item{Generic setters with @scheme[set!], additional useful mutation
  forms: @scheme[pset!], @scheme[shift!], @scheme[rotate!], and some
  simple ones like @scheme[inc!], and @scheme[push!].  (Available
  separately using @scheme[swindle/setf], where the names
  @scheme[setf!] and @scheme[psetf!] are used to avoid changing the
  Racket form)}

@item{Easy macro-defining macros --- simple @scheme[syntax-rules] macros with
  @scheme[defsubst], and a generic @scheme[defmacro] utility, all with a local
  @scheme[let...] form, and extended to easily create symbol macros.
  (@scheme[swindle/misc])}

@item{A @scheme[collect] macro that provides very sophisticated list
  comprehensions and much more.  (@scheme[swindle/misc])}

@item{An @scheme[echo] mechanism which is an alternative to using
  format strings, and contains many useful features including a list
  iteration construct, and is easy to extend.
  (@scheme[swindle/misc.ss])}

@item{A @scheme[regexp-case] syntax which is similar to a
  @scheme[case] on strings with easy access to submatches.
  (@scheme[swindle/misc])}

@item{A CLOS-like object system -- based on Tiny CLOS, but with many
  extensions that bring it much closer to CLOS, and heavily optimized.
  Some added features include singleton and struct classes, applicable
  stand-alone methods, method-combination, and some MOP extensions.
  (Available without syntax bindings in @scheme[swindle/tiny-clos])}

@item{Good integration with the Racket implementation: primitive
  values have corresponding Swindle classes, and struct types can also
  be used as type specializers.  A Swindle class will be made when
  needed, and it will reflect the struct hierarchy.  In addition,
  structs can be defined with a Swindle-line @scheme[defstruct] syntax which
  will also make it possible to create these structs with
  @scheme[make] using keyword arguments.  (@scheme[swindle/tiny-clos]
  and @scheme[swindle/extra])}

@item{Many hairy macros that make the object system much more convenient
  (CLOS has also a lot of macro code).  Some of the macros (especially
  @scheme[defclass]) can be customized.  (@scheme[swindle/clos])}

@item{Useful generic functions, including @scheme[print-object] which
  is used to display all objects.  (@scheme[swindle/extra])}

@item{A @scheme[match] mechanism with a generic-like interface.
 (@scheme[swindle/extra])}

@item{The fun @scheme[amb] toy.  (@scheme[swindle/extra])}

@item{A language that can easily create HTML, where the result is
  human-editable.  (@scheme[swindle/html])}

@item{Customizable syntax: easy to add customized languages to DrRacket.
  (@scheme[custom])}

]

@; ------------------------------
@section{Libraries}

Files marked with ``module'' provide a module by the same name, files
marked with "language module" modify the language and should be used
as an initial import for other modules.  Most files (and especially
all language modules) are useful by themselves, even without using the
whole Swindle environment.

@itemize[

@item{@scheme[swindle/base] (language module) ---
  Basic syntax extensions, mainly Lisp-like lambda argument &-keywords.}

@item{@scheme[swindle/setf] (module) ---
  Generic setters similar to @scheme[setf] in Lisp, and a few more useful
  macros.}

@item{@scheme[swindle/misc] (module) --- Lots of useful functionality
  bits, including everything from frequently useful Racket legacy
  libraries (@schememodname[mzlib/list], @schememodname[mzlib/etc],
  and @schememodname[mzlib/string]).}

@item{@scheme[swindle/turbo] (language module) --- A module that
  packages functionality from @scheme[swindle/base],
  @scheme[swindle/setf] (overriding @scheme[set!] with
  @scheme[setf!]), and @scheme[swindle/misc].}

@item{@scheme[swindle/tiny-clos] (module) ---
  The core object system, based on Tiny CLOS from Xerox, but heavily
  modified, optimized and extended.}

@item{@scheme[swindle/clos] (module) --- Convenient macro wrappers for
  @scheme[swindle/tiny-clos].}

@item{@scheme[swindle/extra] (module) --- Extra functionality on top
  of @scheme[swindle/clos].}

@item{@scheme[swindle/swindle] (language module) --- The main Swindle
  environment module: packages @scheme[swindle/tiny-clos],
  @scheme[swindle/clos], and @scheme[swindle/extra] on top of
  @scheme[swindle/turbo], and some more general definitions.}

@item{@scheme[swindle/info] (module) ---
  Compilation definitions.}

@item{@scheme[swindle/tool] (module) ---
  Setup for Swindle in DrRacket: makes some languages available in
  DrRacket, including custom Swindle-based languages.}

@item{@scheme[swindle/custom] (module) ---
  A sample file that demonstrates how to create a Swindle-based
  customized language; see the source for instructions.}

@item{@scheme[swindle/html] (module) ---
  A language for creating HTML.}

]
