#lang scribble/doc
@(require "common.ss"
          (for-label mzlib/math
                     (only-in scheme/math euler)))

@mzlib[#:mode title math]

Re-exports @schememodname[scheme/math], except that @scheme[euler] is
renamed on export to @scheme[e].
