====< Swindle >=========================================================

This is the Swindle Reference Manual.

Swindle is a collection of modules that extend PLT Scheme with many
additional features.  The main feature which started this project is a
CLOS-like object system based on Tiny-CLOS from Xerox, but there is a
lot more -- see the feature list below for a rough picture.  Swindle is
now part of PLT Scheme.


====< Feature List >====================================================

The following is a high-level description of major features provided by
Swindle.  For every feature, the file that provides it is specified, if
only a subset of the system is needed.

* Some basic syntax extensions, including lambda &-keywords, and
  improved `define' and `let' forms.  (Available separately using
  "base.rkt".)

* Generic setters with `set!', additional useful mutation forms:
  `pset!', `shift!', `rotate!', and some simple ones like `inc!', and
  `push!'.  (Available separately using "setf.rkt", where the names
  `setf!' and `psetf!' are used to avoid changing the Scheme form.)

* Easy macro-defining macros -- simple syntax-rules macros with
  `defsubst', and a generic `defmacro' utility, all with a local
  `let...' form, and extended to easily create symbol macros.
  ("misc.rkt")

* A `collect' macro that provides very sophisticated list comprehensions
  and much more.  ("misc.rkt")

* An `echo' mechanism which is an alternative to using format strings,
  and contains many useful features including a list iteration
  construct, and is easy to extend.  ("misc.rkt")

* A `regexp-case' syntax which is similar to a `case' on strings with
  easy access to submatches.  ("misc.rkt")

* A CLOS-like object system -- based on Tiny CLOS, but with many
  extensions that bring it much closer to CLOS, and heavily optimized.
  Some added features include singleton and struct classes, applicable
  stand-alone methods, method-combination, and some MOP extensions.
  (Available without syntax bindings in "tiny-clos.rkt")

* Good integration with the Scheme implementation: primitive values have
  corresponding Swindle classes, and struct types can also be used as
  type specializers.  A Swindle class will be made when needed, and it
  will reflect the struct hierarchy.  In addition, structs can be
  defined with a Swindle-line `defstruct' syntax which will also make it
  possible to create these structs with `make' using keyword arguments.
  ("tiny-clos.rkt" and "extra.rkt")

* Many hairy macros that make the object system much more convenient
  (CLOS has also a lot of macro code).  Some of the macros (especially
  `defclass') can be customized.  ("clos.rkt")

* Useful generic functions, including `print-object' which is used to
  display all objects.  ("extra.rkt")

* A `match' mechanism with a generic-like interface.  ("extra.rkt")

* The fun `amb' toy.  ("extra.rkt")

* A language that can easily create HTML, where the result is
  human-editable.  ("html.rkt")

* Customizable syntax: easy to add customized languages to DrRacket.
  ("custom.rkt")


====< Reference Manual >================================================

Files marked with "module" provide a module by the same name, files
marked with "language module" modify the language and should be used as
an initial import for other modules.  Most files (and especially all
language modules) are useful by themselves, even without using the whole
Swindle environment.

* base.rkt (language module)
  Basic syntax extensions, mainly Lisp-like lambda argument &-keywords.

* setf.rkt (module)
  Generic setters similar to `setf' in Lisp, and a few more useful
  macros.

* misc.rkt (module)
  Lots of useful functionality bits, including everything from
  frequently useful Racket standard libraries (`list.rkt', `etc.rkt',
  and `string.rkt').

* turbo.rkt (language module)
  A module that packages functionality from `base', `setf' (overriding
  `set!' with `setf!'), and `misc'.

* tiny-clos.rkt (module)
  The core object system, based on Tiny CLOS from Xerox, but heavily
  modified, optimized and extended.

* clos.rkt (module)
  Convenient macro wrappers for "tiny-clos.rkt".

* extra.rkt (module)
  Extra functionality on top of clos.

* swindle.rkt (language module)
  The main Swindle environment module: packages `tiny-clos', `clos', and
  `extra' on top of `turbo', and some more general definitions.

* info.rkt (module)
  Compilation definitions.

* tool.rkt (module)
  Setup for Swindle in DrRacket -- makes some languages available in
  DrRacket, including custom Swindle-based languages.

* custom.rkt (module)
  A sample file that demonstrates how to create a Swindle-based
  customized language -- see the source for instructions.

* html.rkt (module)
  A language for creating HTML.

* html-doc.txt
  Documentation file for "html.rkt".

* doc.txt
  Descriptions of user-level functions, macros, generic functions and
  variables, in a format that help-desk can use.  (Not included, an HTML
  manual is created instead.)

* copying.txt
  Full copyright text (LGPL).


====< Copyright Notice >================================================

Copyright (C) 1998-2014 Eli Barzilay (eli@barzilay.org)

    This library is free software; you can redistribute it and/or modify
    it under the terms of the GNU Lesser General Public License as
    published by the Free Software Foundation; either version 2.1 of the
    License, or (at your option) any later version.

    This library is distributed in the hope that it will be useful, but
    WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
    Lesser General Public License for more details.

    You should have received a copy of the GNU Lesser General Public
    License along with this library; if not, write to the Free Software
    Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
    02110-1301 USA.

====< * >===============================================================
