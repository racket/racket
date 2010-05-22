#lang scribble/doc
@(require scribble/manual
          scribble/eval
          scribble/bnf
          "../scribble.ss"
          (for-label scheme/base scribble/manual unstable/cce/scribble))

@title[#:tag "cce-scribble"]{Scribble Documentation}

@defmodule[unstable/cce/scribble]

This module provides tools for Scribble documentation; specifically, of
@|PLaneT| packages.  In addition to the bindings described below, this module
provides @scheme[this-package-version-symbol] from
@schememodname[unstable/cce/planet], @scheme[this-package-in] from
@schememodname[unstable/cce/require-provide], and
@scheme[make-scribble-evaluator] and @scheme[make-scribble-module-evaluator]
from @schememodname[unstable/cce/sandbox].

@defform*[[
(defmodule/this-package)
(defmodule/this-package #:use-sources [src-path ...] [src ...])
(defmodule/this-package path)
(defmodule/this-package path #:use-sources [src-path ...] [src ...])
]]{

This Scribble form corresponds to @scheme[defmodule] within a planet package.
The displayed module path is a @scheme[planet] module path to the current planet
package, possibly with a @nonterm{path} (from the grammar of @scheme[planet]
module specs) into the package.  If the @scheme[#:use-sources] option is
present, each @scheme[src-path] is similarly treated as a path into the current
planet package, while each @scheme[src] is treated normally.  Both sets of paths
are concatenated and passed to the normal @scheme[defmodule].

}

@defform*[[
(defmodule*/no-declare/this-package [src-path ...] [src ...])
]]{

This Scribble form corresponds to @scheme[defmodule*/no-declare] within a planet
package.  The displayed module paths are @scheme[planet] module paths to the
current planet package, possibly with @nonterm{path}s (from the grammar of
@scheme[planet] module specs) into the package.  Each @scheme[src-path] is
similarly treated as a path into the current planet package, while each
@scheme[src] is treated normally.  Both sets of paths are concatenated and
passed to the normal @scheme[defmodule*/no-declare].

}

@defform*[[
(schememodname/this-package)
(schememodname/this-package path)
]]{

This Scribble form corresponds to @scheme[schememodname] much like
@scheme[defmodule/this-package] above corresponds to @scheme[defmodule].  The
@scheme[path], if present, is treated as a @nonterm{path} (from the grammar of
@scheme[planet] module specs) into the current planet package, and converted
into a @scheme[planet] module spec.

}

@defform*[[
(declare-exporting/this-package [mod-path ...] [mod ...])
(declare-exporting/this-package [mod-path ...] [mod ...]
                                #:use-sources [src-path ...] [src ...])
]]{

This Scribble form corresponds to @scheme[declare-exporting] much like
@scheme[defmodule/this-package] above corresponds to @scheme[defmodule].  Each
@scheme[mod-path] and @scheme[src-path] is treated as a @nonterm{path} (from the
grammar of @scheme[planet] module specs) into the current package.  They are
concatenated with the lists of @scheme[mod]s and @scheme[src]s, respectively,
and passed to the normal @scheme[declare-exporting].

}
