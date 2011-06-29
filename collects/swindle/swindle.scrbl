#lang scribble/doc
@(require scribble/manual)

@title{Swindle}

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
  improved @racket[define] and @racket[let] forms.  (Available
  separately using @racket[swindle/base])}

@item{Generic setters with @racket[set!], additional useful mutation
  forms: @racket[pset!], @racket[shift!], @racket[rotate!], and some
  simple ones like @racket[inc!], and @racket[push!].  (Available
  separately using @racket[swindle/setf], where the names
  @racket[setf!] and @racket[psetf!] are used to avoid changing the
  Racket form)}

@item{Easy macro-defining macros --- simple @racket[syntax-rules] macros with
  @racket[defsubst], and a generic @racket[defmacro] utility, all with a local
  @racket[let...] form, and extended to easily create symbol macros.
  (@racket[swindle/misc])}

@item{A @racket[collect] macro that provides very sophisticated list
  comprehensions and much more.  (@racket[swindle/misc])}

@item{An @racket[echo] mechanism which is an alternative to using
  format strings, and contains many useful features including a list
  iteration construct, and is easy to extend.
  (@racket[swindle/misc])}

@item{A @racket[regexp-case] syntax which is similar to a
  @racket[case] on strings with easy access to submatches.
  (@racket[swindle/misc])}

@item{A CLOS-like object system -- based on Tiny CLOS, but with many
  extensions that bring it much closer to CLOS, and heavily optimized.
  Some added features include singleton and struct classes, applicable
  stand-alone methods, method-combination, and some MOP extensions.
  (Available without syntax bindings in @racket[swindle/tiny-clos])}

@item{Good integration with the Racket implementation: primitive
  values have corresponding Swindle classes, and struct types can also
  be used as type specializers.  A Swindle class will be made when
  needed, and it will reflect the struct hierarchy.  In addition,
  structs can be defined with a Swindle-line @racket[defstruct] syntax which
  will also make it possible to create these structs with
  @racket[make] using keyword arguments.  (@racket[swindle/tiny-clos]
  and @racket[swindle/extra])}

@item{Many hairy macros that make the object system much more convenient
  (CLOS has also a lot of macro code).  Some of the macros (especially
  @racket[defclass]) can be customized.  (@racket[swindle/clos])}

@item{Useful generic functions, including @racket[print-object] which
  is used to display all objects.  (@racket[swindle/extra])}

@item{A @racket[match] mechanism with a generic-like interface.
 (@racket[swindle/extra])}

@item{The fun @racket[amb] toy.  (@racket[swindle/extra])}

@item{A language that can easily create HTML, where the result is
  human-editable.  (@racket[swindle/html])}

@item{Customizable syntax: easy to add customized languages to DrRacket.
  (@racket[custom])}


]

@; ------------------------------
@section{Libraries}

Files marked with ``module'' provide a module by the same name, files
marked with "language module" modify the language and should be used
as an initial import for other modules.  Most files (and especially
all language modules) are useful by themselves, even without using the
whole Swindle environment.

@itemize[

@item{@racket[swindle/base] (language module) ---
  Basic syntax extensions, mainly Lisp-like lambda argument &-keywords.}

@item{@racket[swindle/setf] (module) ---
  Generic setters similar to @racket[setf] in Lisp, and a few more useful
  macros.}

@item{@racket[swindle/misc] (module) --- Lots of useful functionality
  bits, including everything from frequently useful Racket legacy
  libraries (@racketmodname[mzlib/list], @racketmodname[mzlib/etc],
  and @racketmodname[mzlib/string]).}

@item{@racket[swindle/turbo] (language module) --- A module that
  packages functionality from @racket[swindle/base],
  @racket[swindle/setf] (overriding @racket[set!] with
  @racket[setf!]), and @racket[swindle/misc].}

@item{@racket[swindle/tiny-clos] (module) ---
  The core object system, based on Tiny CLOS from Xerox, but heavily
  modified, optimized and extended.}

@item{@racket[swindle/clos] (module) --- Convenient macro wrappers for
  @racket[swindle/tiny-clos].}

@item{@racket[swindle/extra] (module) --- Extra functionality on top
  of @racket[swindle/clos].}

@item{@racket[swindle/swindle] (language module) --- The main Swindle
  environment module: packages @racket[swindle/tiny-clos],
  @racket[swindle/clos], and @racket[swindle/extra] on top of
  @racket[swindle/turbo], and some more general definitions.}

@item{@racket[swindle/info] (module) ---
  Compilation definitions.}

@item{@racket[swindle/tool] (module) ---
  Setup for Swindle in DrRacket: makes some languages available in
  DrRacket, including custom Swindle-based languages.}

@item{@racket[swindle/custom] (module) ---
  A sample file that demonstrates how to create a Swindle-based
  customized language; see the source for instructions.}

@item{@racket[swindle/html] (module) ---
  A language for creating HTML.}

]
