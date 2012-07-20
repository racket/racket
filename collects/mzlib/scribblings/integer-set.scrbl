#lang scribble/doc
@(require "common.rkt"
          (for-label data/integer-set
                     mzlib/integer-set))

@mzlib[#:mode title integer-set]

@deprecated[@racketmodname[data/integer-set]]{}

The @racketmodname[mzlib/integer-set] library re-exports bindings
from @racketmodname[data/integer-set] except that it renames
@racket[symmetric-difference] to @racket[xor], @racket[subtract]
to @racket[difference], and @racket[count] to @racket[card].
